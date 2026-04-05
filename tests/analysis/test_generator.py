import unittest
import os
import asyncio
from src.analysis.test_generator import TestGenerator, TestCase
from src.parsers.base import ASTNode, NodeType

class TestTestGenerator(unittest.IsolatedAsyncioTestCase):
    def setUp(self):
        self.generator = TestGenerator(output_dir="tests/generated_test_output")
        self.sample_ast = ASTNode(
            node_type=NodeType.FUNCTION,
            name="calc_func",
            children=[],
            metadata={}
        )
        
    async def test_generate_tests(self):
        # Mock legacy executable path
        legacy_exe = "path/to/legacy.exe"
        
        tests = await self.generator.generate_tests(self.sample_ast, "source", legacy_exe)
        
        self.assertEqual(len(tests), 3)
        self.assertEqual(tests[0].name, "calc_func_path_0")
        self.assertEqual(tests[0].expected_output, 15)
        self.assertEqual(tests[0].test_type, "path")

    def test_to_pytest(self):
        tc = TestCase(
            name="test1",
            inputs={"x": 1, "y": 2},
            expected_output=3,
            test_type="path",
            description="Simple add"
        )
        code = tc.to_pytest("my_func")
        self.assertIn("def test_test1():", code)
        self.assertIn("result = my_func(x=1, y=2)", code)
        self.assertIn("expected = 3", code)
        self.assertIn("assert result == expected", code)

    async def test_generate_pytest_file(self):
        legacy_exe = "path/to/legacy.exe"
        tests = await self.generator.generate_tests(self.sample_ast, "source", legacy_exe)
        
        output_file = "tests/generated_test_output/test_calc.py"
        self.generator.generate_pytest_file(tests, "my_module.calc_func", output_file)
        
        self.assertTrue(os.path.exists(output_file))
        with open(output_file, 'r') as f:
            content = f.read()
            self.assertIn("def test_calc_func_path_0():", content)
            
    def tearDown(self):
        # Cleanup generated files
        if os.path.exists("tests/generated_test_output/test_calc.py"):
            os.remove("tests/generated_test_output/test_calc.py")
        if os.path.exists("tests/generated_test_output"):
            os.rmdir("tests/generated_test_output")

if __name__ == '__main__':
    unittest.main()
