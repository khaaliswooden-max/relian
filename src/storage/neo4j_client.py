import os
from neo4j import GraphDatabase
from typing import Dict, Any, List

class Neo4jClient:
    """Client for interacting with Neo4j database."""
    
    def __init__(self, uri: str = None, user: str = None, password: str = None):
        self.uri = uri or os.getenv("NEO4J_URI", "bolt://localhost:7687")
        self.user = user or os.getenv("NEO4J_USER", "neo4j")
        self.password = password or os.getenv("NEO4J_PASSWORD", "password")
        self.driver = None
        
    def connect(self):
        """Establish connection to Neo4j."""
        if not self.driver:
            self.driver = GraphDatabase.driver(self.uri, auth=(self.user, self.password))
            
    def close(self):
        """Close connection."""
        if self.driver:
            self.driver.close()
            
    def store_function_analysis(self, function_name: str, analysis: Dict[str, Any]):
        """Store function analysis results in the graph."""
        self.connect()
        
        with self.driver.session() as session:
            session.execute_write(self._create_function_nodes, function_name, analysis)
            
    @staticmethod
    def _create_function_nodes(tx, function_name: str, analysis: Dict[str, Any]):
        # Create Function node
        query = """
        MERGE (f:Function {name: $name})
        SET f.purpose = $purpose,
            f.confidence = $confidence,
            f.updated_at = datetime()
        RETURN f
        """
        tx.run(query, name=function_name, purpose=analysis.get("purpose"), 
               confidence=analysis.get("confidence"))
        
        # Create BusinessRule nodes
        for rule in analysis.get("business_rules", []):
            rule_query = """
            MATCH (f:Function {name: $func_name})
            MERGE (r:BusinessRule {description: $desc})
            MERGE (f)-[:IMPLEMENTS]->(r)
            """
            tx.run(rule_query, func_name=function_name, desc=rule)
            
        # Create DecisionTree nodes (simplified as LogicFlow)
        for decision in analysis.get("decision_trees", []):
            # Assuming decision is a string description for now
            dt_query = """
            MATCH (f:Function {name: $func_name})
            MERGE (d:LogicFlow {description: $desc})
            MERGE (f)-[:CONTAINS_LOGIC]->(d)
            """
            tx.run(dt_query, func_name=function_name, desc=str(decision))
            
        # Create EdgeCase nodes
        for case in analysis.get("edge_cases", []):
            ec_query = """
            MATCH (f:Function {name: $func_name})
            MERGE (e:EdgeCase {description: $desc})
            MERGE (f)-[:HANDLES]->(e)
            """
            tx.run(ec_query, func_name=function_name, desc=case)
