"""COBOL language parser using ANTLR."""

import antlr4
from typing import List, Dict, Any, Optional
from .base import LanguageParser, ASTNode, NodeType
from .antlr.cobol.Cobol85Lexer import Cobol85Lexer
from .antlr.cobol.Cobol85Parser import Cobol85Parser
from .antlr.cobol.Cobol85Visitor import Cobol85Visitor

class CobolASTVisitor(Cobol85Visitor):
    """Visitor to convert ANTLR parse tree to ASTNode."""
    
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
        if ctx.identificationDivision() and ctx.identificationDivision().programIdParagraph():
            program_id = ctx.identificationDivision().programIdParagraph().programName().getText()
            
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
        # Return a list of variable nodes
        variables = []
        if ctx.workingStorageSection():
            # ctx.workingStorageSection() returns a list
            for section in ctx.workingStorageSection():
                res = self.visit(section)
                if res:
                    variables.extend(res)
        if ctx.linkageSection():
            for section in ctx.linkageSection():
                res = self.visit(section)
                if res:
                    variables.extend(res)
        # File section variables could also be added
        return variables

    def visitWorkingStorageSection(self, ctx: Cobol85Parser.WorkingStorageSectionContext):
        vars = []
        for child in ctx.getChildren():
            if isinstance(child, Cobol85Parser.DataDescriptionEntryContext):
                res = self.visit(child)
                if res:
                    vars.append(res)
        return vars

    def visitLinkageSection(self, ctx: Cobol85Parser.LinkageSectionContext):
        vars = []
        for child in ctx.getChildren():
            if isinstance(child, Cobol85Parser.DataDescriptionEntryContext):
                res = self.visit(child)
                if res:
                    vars.append(res)
        return vars

    def visitDataDescriptionEntry(self, ctx: Cobol85Parser.DataDescriptionEntryContext):
        name = "FILLER"
        if ctx.dataName():
            name = ctx.dataName().getText()
        
        level = ctx.levelNumber().getText()
        metadata = {"level": level}
        
        # Extract picture string
        for child in ctx.getChildren():
            if isinstance(child, Cobol85Parser.PictureClauseContext):
                metadata["picture"] = child.pictureString().getText()
            if isinstance(child, Cobol85Parser.UsageClauseContext):
                metadata["usage"] = child.getText()
            if isinstance(child, Cobol85Parser.ValueClauseContext):
                metadata["value"] = child.literal().getText()

        return ASTNode(
            node_type=NodeType.VARIABLE,
            name=name,
            children=[],
            metadata=metadata,
            source_location=self._get_location(ctx)
        )

    def visitProcedureDivision(self, ctx: Cobol85Parser.ProcedureDivisionContext):
        # Return list of procedures/paragraphs
        if ctx.paragraphs():
            return self.visit(ctx.paragraphs())
        return []

    def visitParagraphs(self, ctx: Cobol85Parser.ParagraphsContext):
        nodes = []
        for child in ctx.getChildren():
            res = self.visit(child)
            if res:
                if isinstance(res, list):
                    nodes.extend(res)
                else:
                    nodes.append(res)
        return nodes

    def visitSection(self, ctx: Cobol85Parser.SectionContext):
        name = ctx.sectionHeader().sectionName().getText()
        node = ASTNode(
            node_type=NodeType.PROCEDURE,
            name=name,
            children=[],
            metadata={"type": "SECTION"},
            source_location=self._get_location(ctx)
        )
        # Visit paragraphs in section
        for child in ctx.getChildren():
            if isinstance(child, Cobol85Parser.ParagraphContext):
                p_node = self.visit(child)
                if p_node:
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
        # Visit sentences/statements
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

    def visitCopyStatement(self, ctx: Cobol85Parser.CopyStatementContext):
        target = ctx.textName().getText() if ctx.textName() else ctx.literal().getText()
        return ASTNode(
            node_type=NodeType.EXPRESSION, # Using EXPRESSION for COPY for now, or add DEPENDENCY type?
            name=f"COPY {target}",
            children=[],
            metadata={"dependency": target, "type": "COPY"},
            source_location=self._get_location(ctx)
        )

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
        with open(filepath, 'r') as f:
            source = f.read()
        return self.parse_string(source)
    
    def parse_string(self, source_code: str) -> ASTNode:
        """Parse COBOL source code string."""
        input_stream = antlr4.InputStream(source_code)
        self.lexer = Cobol85Lexer(input_stream)
        token_stream = antlr4.CommonTokenStream(self.lexer)
        self.parser = Cobol85Parser(token_stream)
        
        # Parse
        tree = self.parser.compilationUnit()
        
        # Build AST
        visitor = CobolASTVisitor()
        # compilationUnit returns list of programUnits (from visitChildren)
        # We take the first one or wrap them
        result = visitor.visit(tree)
        
        if isinstance(result, list):
             # If multiple programs, maybe wrap in a 'File' node? 
             # For now, just return the first one or a dummy root if multiple
             if len(result) == 1:
                 return result[0]
             else:
                 # Create a dummy root
                 return ASTNode(NodeType.PROGRAM, "CompilationUnit", result, {})
        return result
    
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
