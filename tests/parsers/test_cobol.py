import unittest
from src.parsers.cobol import COBOLParser
from src.parsers.base import NodeType

class TestCOBOLParser(unittest.TestCase):
    def setUp(self):
        self.parser = COBOLParser()
        # Deliberately COPY-free. `COPY` is a lexer token in the vendored
        # ProLeap grammar that no parser rule references (it is consumed by the
        # separate Cobol85Preprocessor.g4, which this repo vendors but does not
        # run), so a COPY in the PROCEDURE DIVISION aborts the parse at that
        # point and everything after it is never seen. Structure, variables and
        # paragraphs are therefore asserted on source the grammar can actually
        # parse; the COPY case has its own fixture below.
        self.sample_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  VAR1 PIC X(10) VALUE "HELLO".
       01  VAR2 PIC 9(5).
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "START".
           CALL "SUBPROG" USING VAR1.
           PERFORM CALC-PARA.
           STOP RUN.
       CALC-PARA.
           ADD 1 TO VAR2.
       """
        # Same program with a COPY. Used only to assert that the copybook
        # dependency is still recovered — lexically, from the token stream —
        # even though no tree node for it exists.
        self.sample_with_copy = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  VAR1 PIC X(10).
       PROCEDURE DIVISION.
       MAIN-PARA.
           COPY MYCOPY.
           STOP RUN.
       """

    def test_parse_structure(self):
        ast = self.parser.parse_string(self.sample_cobol)
        self.assertEqual(ast.node_type, NodeType.PROGRAM)
        self.assertEqual(ast.name, "TESTPROG")

    def test_extract_variables(self):
        ast = self.parser.parse_string(self.sample_cobol)
        vars = self.parser.extract_variables(ast)
        self.assertEqual(len(vars), 2)
        self.assertEqual(vars[0].name, "VAR1")
        self.assertEqual(vars[0].metadata["picture"], "X(10)")
        self.assertEqual(vars[1].name, "VAR2")

    def test_extract_functions(self):
        ast = self.parser.parse_string(self.sample_cobol)
        funcs = self.parser.extract_functions(ast)
        self.assertEqual(len(funcs), 2)
        names = [f.name for f in funcs]
        self.assertIn("MAIN-PARA", names)
        self.assertIn("CALC-PARA", names)

    def test_get_dependencies(self):
        ast = self.parser.parse_string(self.sample_cobol)
        deps = self.parser.get_dependencies(ast)
        self.assertIn("\"SUBPROG\"", deps) # CALL target is literal string

        # COPY has no parser rule, so its edge comes from the token stream and
        # says so. A consumer must be able to tell the two apart.
        copy_ast = self.parser.parse_string(self.sample_with_copy)
        self.assertIn("MYCOPY", self.parser.get_dependencies(copy_ast))
        copy_nodes = [c for c in copy_ast.children
                      if c.metadata.get("type") == "COPY"]
        self.assertEqual(len(copy_nodes), 1)
        self.assertEqual(copy_nodes[0].metadata["recovered_by"], "token_scan")

if __name__ == '__main__':
    unittest.main()
