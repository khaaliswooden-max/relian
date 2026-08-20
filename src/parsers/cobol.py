"""COBOL language parser using ANTLR."""

import antlr4
from typing import List
from .base import LanguageParser, ASTNode, NodeType
from .antlr.cobol.Cobol85Lexer import Cobol85Lexer
from .antlr.cobol.Cobol85Parser import Cobol85Parser
from .antlr.cobol.Cobol85Visitor import Cobol85Visitor

class CobolASTVisitor(Cobol85Visitor):
    """Visitor to convert ANTLR parse tree to ASTNode.

    Bound to the vendored ProLeap COBOL-85 grammar (``Cobol85.g4``, see
    ``docs/GRAMMAR_PROVENANCE.md``). That grammar nests several of the
    divisions this visitor walks one level deeper than the reduced grammar it
    replaced — ``dataDivision`` holds ``dataDivisionSection*`` rather than the
    sections directly, ``procedureDivision`` holds a ``procedureDivisionBody``,
    and a data item's name, level and clauses live on
    ``dataDescriptionEntryFormat1`` rather than on ``dataDescriptionEntry``
    itself — so every accessor below goes through the intermediate rule rather
    than reaching for a child that no longer exists there.
    """

    def __init__(self):
        self.root = None
        self.current_parent = None

    def visitCompilationUnit(self, ctx: Cobol85Parser.CompilationUnitContext):
        results = []
        if ctx.programUnit():
            for pu in ctx.programUnit():
                res = self.visit(pu)
                if res:
                    results.append(res)
        return results

    def visitProgramUnit(self, ctx: Cobol85Parser.ProgramUnitContext):
        program_id = "Unknown"
        ident = ctx.identificationDivision()
        if ident and ident.programIdParagraph():
            program_id = ident.programIdParagraph().programName().getText()

        node = ASTNode(
            node_type=NodeType.PROGRAM,
            name=program_id,
            children=[],
            metadata={},
            source_location=self._get_location(ctx)
        )

        # Visit children to populate the program node
        # We manually visit divisions to structure the AST
        if ctx.environmentDivision():
            self._visit_and_add(ctx.environmentDivision(), node)
        if ctx.dataDivision():
            self._visit_and_add(ctx.dataDivision(), node)
        if ctx.procedureDivision():
            self._visit_and_add(ctx.procedureDivision(), node)

        return node

    def _visit_and_add(self, ctx, parent_node):
        child_node = self.visit(ctx)
        if child_node:
            if isinstance(child_node, list):
                parent_node.children.extend(child_node)
            else:
                parent_node.children.append(child_node)

    def visitEnvironmentDivision(self, ctx: Cobol85Parser.EnvironmentDivisionContext):
        # We might want to capture config, but for now just return None or minimal info
        return None

    def visitDataDivision(self, ctx: Cobol85Parser.DataDivisionContext):
        """Return a list of variable nodes.

        ``dataDivision: DATA DIVISION DOT_FS dataDivisionSection*`` — the
        sections are reached through ``dataDivisionSection``, which is the
        choice among FILE / WORKING-STORAGE / LINKAGE / SCREEN and the rest.
        FILE SECTION records are not collected here, matching the previous
        behaviour.
        """
        variables = []
        for section in ctx.dataDivisionSection():
            for accessor in (section.workingStorageSection, section.linkageSection):
                sub = accessor()
                if sub is None:
                    continue
                res = self.visit(sub)
                if res:
                    variables.extend(res)
        return variables

    def visitWorkingStorageSection(self, ctx: Cobol85Parser.WorkingStorageSectionContext):
        return self._data_entries(ctx)

    def visitLinkageSection(self, ctx: Cobol85Parser.LinkageSectionContext):
        return self._data_entries(ctx)

    def _data_entries(self, ctx):
        var_nodes = []
        for entry in ctx.dataDescriptionEntry():
            res = self.visit(entry)
            if res:
                var_nodes.append(res)
        return var_nodes

    def visitDataDescriptionEntry(self, ctx: Cobol85Parser.DataDescriptionEntryContext):
        """Format 1 only — the ordinary ``<level> <name> <clauses>.`` entry.

        ``dataDescriptionEntry`` is a choice of four formats: format 1 is the
        ordinary data item, format 2 is ``66 … RENAMES``, format 3 is an
        ``88`` condition name, and the fourth is an embedded ``EXEC SQL``
        declaration. Only format 1 declares a field with a level, a name and a
        PICTURE, so the other three yield no VARIABLE node rather than a node
        with invented attributes (R1).
        """
        fmt1 = ctx.dataDescriptionEntryFormat1()
        if fmt1 is None:
            return None

        name = "FILLER"
        if fmt1.dataName():
            name = fmt1.dataName().getText()

        # The level number is a bare token on format 1 (INTEGERLITERAL, or
        # LEVEL_NUMBER_77 for the standalone 77 level), not a levelNumber rule.
        level_tok = fmt1.INTEGERLITERAL() or fmt1.LEVEL_NUMBER_77()
        metadata = {"level": level_tok.getText() if level_tok is not None else ""}

        # Extract picture string
        for child in fmt1.getChildren():
            if isinstance(child, Cobol85Parser.DataPictureClauseContext):
                metadata["picture"] = child.pictureString().getText()
            if isinstance(child, Cobol85Parser.DataUsageClauseContext):
                metadata["usage"] = child.getText()
            if isinstance(child, Cobol85Parser.DataValueClauseContext):
                metadata["value"] = child.getText()

        return ASTNode(
            node_type=NodeType.VARIABLE,
            name=name,
            children=[],
            metadata=metadata,
            source_location=self._get_location(fmt1)
        )

    def visitProcedureDivision(self, ctx: Cobol85Parser.ProcedureDivisionContext):
        """Return list of procedures/paragraphs.

        ``procedureDivision`` reaches its paragraphs through
        ``procedureDivisionBody: paragraphs procedureSection*``, so sections
        are siblings of the leading paragraphs rather than members of them.
        """
        body = ctx.procedureDivisionBody()
        if body is None:
            return []
        nodes = []
        if body.paragraphs():
            res = self.visit(body.paragraphs())
            if res:
                nodes.extend(res)
        for sect in body.procedureSection():
            res = self.visit(sect)
            if res:
                nodes.append(res)
        return nodes

    def visitParagraphs(self, ctx: Cobol85Parser.ParagraphsContext):
        """``paragraphs: sentence* paragraph*``.

        The leading ``sentence*`` is procedure-division code written before
        any paragraph label. It carries statements but no name, so it produces
        no PROCEDURE node — only the named paragraphs do.
        """
        nodes = []
        for para in ctx.paragraph():
            res = self.visit(para)
            if res:
                nodes.append(res)
        return nodes

    def visitProcedureSection(self, ctx: Cobol85Parser.ProcedureSectionContext):
        name = ctx.procedureSectionHeader().sectionName().getText()
        node = ASTNode(
            node_type=NodeType.PROCEDURE,
            name=name,
            children=[],
            metadata={"type": "SECTION"},
            source_location=self._get_location(ctx)
        )
        # Visit paragraphs in section
        if ctx.paragraphs():
            for p_node in self.visit(ctx.paragraphs()) or []:
                node.children.append(p_node)
        return node

    def visitParagraph(self, ctx: Cobol85Parser.ParagraphContext):
        name = ctx.paragraphName().getText()
        node = ASTNode(
            node_type=NodeType.PROCEDURE,
            name=name,
            children=[],
            metadata={"type": "PARAGRAPH"},
            source_location=self._get_location(ctx)
        )
        # Visit sentences/statements. `paragraph` is
        # `paragraphName DOT_FS (alteredGoTo | sentence*)`, so a paragraph that
        # is the target of an ALTER has no sentences at all.
        for sentence in ctx.sentence():
            for stmt in sentence.statement():
                s_node = self.visit(stmt)
                if s_node:
                    node.children.append(s_node)
        return node

    def visitStatement(self, ctx: Cobol85Parser.StatementContext):
        # Delegate to specific statement visitors
        return self.visitChildren(ctx)

    def visitCallStatement(self, ctx: Cobol85Parser.CallStatementContext):
        target = ctx.getChild(1).getText() # CALL target ...
        return ASTNode(
            node_type=NodeType.CALL,
            name=target,
            children=[],
            metadata={},
            source_location=self._get_location(ctx)
        )

    # NOTE: there is no visitCopyStatement. `COPY` is a lexer token in
    # `Cobol85.g4` that no parser rule references — the upstream project
    # consumes it in the separate `Cobol85Preprocessor.g4`, which this repo
    # vendors but does not yet run. COPY dependencies are therefore recovered
    # from the token stream by `COBOLParser._copy_dependencies`, and labelled
    # as such, rather than from the tree.

    def _get_location(self, ctx):
        start = ctx.start
        stop = ctx.stop
        return (start.line, stop.line, start.column, stop.column + len(stop.text))


class COBOLParser(LanguageParser):
    """Parser for COBOL language."""
    
    def __init__(self):
        self.lexer = None
        self.parser = None
    
    def parse_file(self, filepath: str) -> ASTNode:
        """Parse COBOL source file."""
        with open(filepath, 'r', encoding='utf-8') as f:
            source = f.read()
        return self.parse_string(source)
    
    def parse_string(self, source_code: str) -> ASTNode:
        """Parse COBOL source code string."""
        input_stream = antlr4.InputStream(source_code)
        self.lexer = Cobol85Lexer(input_stream)
        token_stream = antlr4.CommonTokenStream(self.lexer)
        self.parser = Cobol85Parser(token_stream)

        # Parse. `startRule: compilationUnit EOF` is the grammar's entry point;
        # entering at `compilationUnit` instead would let a file whose tail the
        # grammar cannot parse report a clean tree over its prefix.
        tree = self.parser.startRule().compilationUnit()

        # Build AST
        visitor = CobolASTVisitor()
        # compilationUnit returns list of programUnits (from visitChildren)
        # We take the first one or wrap them
        result = visitor.visit(tree)

        if isinstance(result, list):
            # If multiple programs, maybe wrap in a 'File' node?
            # For now, just return the first one or a dummy root if multiple
            if len(result) == 1:
                root = result[0]
            else:
                # Create a dummy root
                root = ASTNode(NodeType.PROGRAM, "CompilationUnit", result, {})
        else:
            root = result

        if root is not None:
            root.children.extend(self._copy_dependencies(token_stream))
        return root

    def _copy_dependencies(self, token_stream) -> List[ASTNode]:
        """Recover ``COPY <name>`` targets from the token stream, not the tree.

        `COPY` is a lexer token in `Cobol85.g4` that **no parser rule
        references**; upstream consumes it in the separate
        `Cobol85Preprocessor.g4`, which this repo vendors but does not yet run
        (see `docs/GRAMMAR_PROVENANCE.md`). So there is no `copyStatement`
        context to visit, and a COPY-bearing program is a syntax error under
        this grammar rather than a tree carrying a COPY node.

        Rather than drop the dependency edge silently, it is recovered
        lexically — the token after `COPY`, skipping hidden-channel tokens —
        and every node produced this way is stamped
        ``recovered_by="token_scan"`` so a consumer can tell a lexically
        recovered edge from a parsed one. This is a scan, not a parse: it does
        not resolve `REPLACING`, and it does not know whether the copybook
        exists. Both are preprocessor work.
        """
        token_stream.fill()
        tokens = token_stream.tokens
        deps: List[ASTNode] = []
        for i, tok in enumerate(tokens):
            if tok.type != Cobol85Lexer.COPY:
                continue
            target = None
            for nxt in tokens[i + 1:]:
                if nxt.channel != antlr4.Token.DEFAULT_CHANNEL:
                    continue
                if nxt.type in (Cobol85Lexer.DOT_FS, antlr4.Token.EOF):
                    break
                target = nxt.text
                break
            if not target:
                continue
            deps.append(
                ASTNode(
                    node_type=NodeType.EXPRESSION,
                    name=f"COPY {target}",
                    children=[],
                    metadata={
                        "dependency": target,
                        "type": "COPY",
                        "recovered_by": "token_scan",
                    },
                    source_location=(tok.line, tok.line, tok.column,
                                     tok.column + len(tok.text)),
                )
            )
        return deps


    def extract_functions(self, ast: ASTNode) -> List[ASTNode]:
        """Extract COBOL paragraphs/procedures as functions."""
        functions = []
        self._find_nodes_by_type(ast, NodeType.PROCEDURE, functions)
        return functions
    
    def extract_variables(self, ast: ASTNode) -> List[ASTNode]:
        """Extract COBOL DATA DIVISION variables."""
        variables = []
        self._find_nodes_by_type(ast, NodeType.VARIABLE, variables)
        return variables
    
    def get_dependencies(self, ast: ASTNode) -> List[str]:
        """Extract COPY statements and CALL targets."""
        dependencies = set()
        
        def find_deps(node):
            if node.node_type == NodeType.CALL:
                dependencies.add(node.name)
            if node.metadata.get("type") == "COPY":
                dependencies.add(node.metadata.get("dependency"))
            for child in node.children:
                find_deps(child)
                
        find_deps(ast)
        return list(dependencies)
    
    def _find_nodes_by_type(self, node: ASTNode, target_type: NodeType, 
                           results: List[ASTNode]) -> None:
        """Recursively find nodes of specific type."""
        if node.node_type == target_type:
            results.append(node)
        for child in node.children:
            self._find_nodes_by_type(child, target_type, results)
