import sys
import os
import asyncio

# Add project root to path (so 'src.xxx' imports work)
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))

try:
    from src.analysis import SemanticAnalyzer
    print("Successfully imported SemanticAnalyzer.")
    
    # Instantiate with dummy keys to check init logic
    os.environ["OPENAI_API_KEY"] = "dummy"
    analyzer = SemanticAnalyzer(provider="openai")
    print("Successfully instantiated SemanticAnalyzer with OpenAI.")
    
except ImportError as e:
    print(f"ImportError: {e}")
    sys.exit(1)
except Exception as e:
    print(f"An error occurred: {e}")
    sys.exit(1)
