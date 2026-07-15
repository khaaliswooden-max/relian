"""Base parser interface for all legacy languages."""

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Dict, Any, Optional
from enum import Enum

class NodeType(Enum):
    """AST node types common across languages."""
    PROGRAM = "program"
    FUNCTION = "function"
    PROCEDURE = "procedure"
    VARIABLE = "variable"
    CONDITIONAL = "conditional"
    LOOP = "loop"
    ASSIGNMENT = "assignment"
    CALL = "call"
    EXPRESSION = "expression"

@dataclass
class ASTNode:
    """Universal AST node representation."""
    node_type: NodeType
    name: str
    children: List['ASTNode']
    metadata: Dict[str, Any]
    source_location: Optional[tuple] = None  # (line_start, line_end, col_start, col_end)
    
    def to_dict(self) -> Dict[str, Any]:
        """Serialize AST node to dictionary."""
        return {
            "type": self.node_type.value,
            "name": self.name,
            "children": [child.to_dict() for child in self.children],
            "metadata": self.metadata,
            "source_location": self.source_location
        }

class LanguageParser(ABC):
    """Abstract base class for language-specific parsers."""
    
    @abstractmethod
    def parse_file(self, filepath: str) -> ASTNode:
        """Parse a source file into AST."""

    @abstractmethod
    def parse_string(self, source_code: str) -> ASTNode:
        """Parse source code string into AST."""

    @abstractmethod
    def extract_functions(self, ast: ASTNode) -> List[ASTNode]:
        """Extract all function definitions from AST."""

    @abstractmethod
    def extract_variables(self, ast: ASTNode) -> List[ASTNode]:
        """Extract all variable declarations from AST."""

    @abstractmethod
    def get_dependencies(self, ast: ASTNode) -> List[str]:
        """Extract module/library dependencies."""
