import sys
import os
import asyncio

# Add src to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../src')))

try:
    from analysis import SemanticAnalyzer
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
