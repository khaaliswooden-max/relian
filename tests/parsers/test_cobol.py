import unittest
from src.parsers.cobol import COBOLParser
from src.parsers.base import NodeType

class TestCOBOLParser(unittest.TestCase):
    def setUp(self):
        self.parser = COBOLParser()
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
           COPY MYCOPY.
           PERFORM CALC-PARA.
           STOP RUN.
       CALC-PARA.
           ADD 1 TO VAR2.
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
        self.assertIn("MYCOPY", deps)

if __name__ == '__main__':
    unittest.main()
