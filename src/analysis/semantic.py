"""LLM-powered semantic code understanding."""

from typing import Dict, Any
import json
import os

from openai import AsyncOpenAI
from anthropic import AsyncAnthropic

from src.parsers.base import ASTNode
from src.storage.neo4j_client import Neo4jClient

class SemanticAnalyzer:
    """Extract business logic meaning from code using LLMs."""
    
    def __init__(self, provider: str = "openai", model: str = "gpt-4-turbo", neo4j_client: Neo4jClient = None):
        self.provider = provider
        self.model = model
        self.neo4j_client = neo4j_client or Neo4jClient()
        
        if provider == "openai":
            self.client = AsyncOpenAI(api_key=os.getenv("OPENAI_API_KEY"))
        elif provider == "anthropic":
            self.client = AsyncAnthropic(api_key=os.getenv("ANTHROPIC_API_KEY"))
        else:
            raise ValueError(f"Unknown provider: {provider}")
    
    async def analyze_function(self, ast_node: ASTNode, source_code: str) -> Dict[str, Any]:
        """
        Analyze a function to extract business logic.
        
        Returns:
            {
                "purpose": "What this function does (business perspective)",
                "inputs": [...],
                "outputs": [...],
                "business_rules": [...],
                "decision_trees": [...],
                "calculations": [...],
                "edge_cases": [...],
                "confidence": 0.0-1.0
            }
        """
        prompt = self._build_analysis_prompt(ast_node, source_code)
        
        response = await self._call_llm(prompt)
        analysis = self._parse_response(response)
        
        # Store in Neo4j
        if analysis.get("confidence", 0) > 0:
            try:
                self.neo4j_client.store_function_analysis(ast_node.name, analysis)
            except Exception as e:
                print(f"Warning: Failed to store analysis in Neo4j: {e}")
        
        return analysis
    
    def _build_analysis_prompt(self, ast_node: ASTNode, source_code: str) -> str:
        """Construct prompt for LLM analysis."""
        return f"""You are analyzing legacy code to extract business logic.

Function AST:
{json.dumps(ast_node.to_dict(), indent=2)}

Source Code:
{source_code}

Analyze this function and provide:
1. **Purpose**: What does this function accomplish from a business perspective?
2. **Inputs**: What data does it receive? Include types and constraints.
3. **Outputs**: What does it return or modify?
4. **Business Rules**: Extract any explicit rules (e.g., "if age > 65 then apply senior discount")
5. **Decision Trees**: Identify conditional logic flows
6. **Calculations**: Extract mathematical formulas
7. **Edge Cases**: Identify special conditions or boundary cases
8. **Confidence**: Your confidence in this analysis (0.0-1.0)

Respond in JSON format:
{{
  "purpose": "...",
  "inputs": [...],
  "outputs": [...],
  "business_rules": [...],
  "decision_trees": [...],
  "calculations": [...],
  "edge_cases": [...],
  "confidence": 0.95
}}
"""

    async def _call_llm(self, prompt: str) -> str:
        """Call LLM API."""
        if self.provider == "openai":
            response = await self.client.chat.completions.create(
                model=self.model,
                messages=[{"role": "user", "content": prompt}],
                temperature=0.1,  # Low temp for consistent analysis
                max_tokens=4000
            )
            return response.choices[0].message.content
        
        elif self.provider == "anthropic":
            response = await self.client.messages.create(
                model=self.model,
                messages=[{"role": "user", "content": prompt}],
                temperature=0.1,
                max_tokens=4000
            )
            return response.content[0].text
        else:
            raise ValueError(f"Provider {self.provider} not implemented in _call_llm")

    def _parse_response(self, response: str) -> Dict[str, Any]:
        """Parse LLM JSON response."""
        # Strip markdown code blocks if present
        response = response.strip()
        if response.startswith("```json"):
            response = response[7:-3]
        elif response.startswith("```"):
            response = response[3:-3]
        
        try:
            return json.loads(response)
        except json.JSONDecodeError as e:
            # Fallback or error handling
            print(f"Failed to parse JSON response: {e}")
            print(f"Response text: {response}")
            return {
                "purpose": "Error parsing analysis",
                "confidence": 0.0,
                "raw_response": response
            }
