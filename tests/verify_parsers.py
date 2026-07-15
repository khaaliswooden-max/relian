import sys
import os

# Add src to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../src')))

try:
    from parsers import LanguageParser, ASTNode, NodeType, COBOLParser
    print("Successfully imported parsers package and classes.")
    
    parser = COBOLParser()
    print("Successfully instantiated COBOLParser.")
    
    try:
        parser.parse_string("SOME COBOL CODE")
    except NotImplementedError as e:
        print(f"Caught expected NotImplementedError: {e}")
        
except ImportError as e:
    print(f"ImportError: {e}")
    sys.exit(1)
except Exception as e:
    print(f"An error occurred: {e}")
    sys.exit(1)
