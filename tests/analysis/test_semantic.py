import unittest
from unittest.mock import MagicMock, AsyncMock, patch
import json
from src.analysis.semantic import SemanticAnalyzer
from src.parsers.base import ASTNode, NodeType
from src.storage.neo4j_client import Neo4jClient

class TestSemanticAnalyzer(unittest.IsolatedAsyncioTestCase):
    async def asyncSetUp(self):
        # Patch LLM clients
        self.openai_patcher = patch('src.analysis.semantic.AsyncOpenAI')
        self.anthropic_patcher = patch('src.analysis.semantic.AsyncAnthropic')
        self.mock_openai = self.openai_patcher.start()
        self.mock_anthropic = self.anthropic_patcher.start()
        
        self.mock_neo4j = MagicMock(spec=Neo4jClient)
        self.analyzer = SemanticAnalyzer(neo4j_client=self.mock_neo4j)
        
        # Mock _call_llm to avoid API calls
        self.analyzer._call_llm = AsyncMock()
        
        self.sample_ast = ASTNode(
            node_type=NodeType.FUNCTION,
            name="calculate_discount",
            children=[],
            metadata={}
        )
        self.sample_code = "def calculate_discount(age): return 0.1 if age > 65 else 0"
        
        self.sample_analysis = {
            "purpose": "Calculate senior discount",
            "inputs": ["age"],
            "outputs": ["discount_rate"],
            "business_rules": ["If age > 65, discount is 10%"],
            "decision_trees": ["age > 65"],
            "calculations": ["0.1"],
            "edge_cases": [],
            "confidence": 0.95
        }
        
    async def test_analyze_function_success(self):
        # Setup mock response
        self.analyzer._call_llm.return_value = json.dumps(self.sample_analysis)
        
        # Run analysis
        result = await self.analyzer.analyze_function(self.sample_ast, self.sample_code)
        
        # Verify result
        self.assertEqual(result["purpose"], "Calculate senior discount")
        self.assertEqual(result["confidence"], 0.95)
        
        # Verify Neo4j storage called
        self.mock_neo4j.store_function_analysis.assert_called_once_with(
            "calculate_discount", 
            self.sample_analysis
        )

    async def test_analyze_function_low_confidence(self):
        # Setup mock response with low confidence (simulating failure to extract)
        low_conf_analysis = self.sample_analysis.copy()
        low_conf_analysis["confidence"] = 0.0
        self.analyzer._call_llm.return_value = json.dumps(low_conf_analysis)
        
        # Run analysis
        result = await self.analyzer.analyze_function(self.sample_ast, self.sample_code)
        
        # Verify Neo4j storage NOT called (based on logic if confidence > 0)
        # Actually my implementation checks if confidence > 0, so 0.0 should skip
        self.mock_neo4j.store_function_analysis.assert_not_called()

    async def test_analyze_function_storage_failure(self):
        # Setup mock response
        self.analyzer._call_llm.return_value = json.dumps(self.sample_analysis)
        
        # Setup Neo4j to raise exception
        self.mock_neo4j.store_function_analysis.side_effect = Exception("DB Error")
        
        # Run analysis - should not raise exception
        result = await self.analyzer.analyze_function(self.sample_ast, self.sample_code)
        
        # Verify result still returned
        self.assertEqual(result["purpose"], "Calculate senior discount")
        self.mock_neo4j.store_function_analysis.assert_called_once()

    async def asyncTearDown(self):
        self.openai_patcher.stop()
        self.anthropic_patcher.stop()

if __name__ == '__main__':
    unittest.main()
