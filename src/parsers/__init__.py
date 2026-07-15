from .base import LanguageParser, ASTNode, NodeType

try:
    from .cobol import COBOLParser
    __all__ = ['LanguageParser', 'ASTNode', 'NodeType', 'COBOLParser']
except ImportError:
    # antlr4 not installed; COBOL parser unavailable in this environment
    __all__ = ['LanguageParser', 'ASTNode', 'NodeType']
