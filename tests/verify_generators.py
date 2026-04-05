import sys
import os
import asyncio

# Add src to path
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../src')))

try:
    from generators import TestGenerator, TestCase
    print("Successfully imported TestGenerator and TestCase.")
    
    generator = TestGenerator()
    print("Successfully instantiated TestGenerator.")
    
    # Simple smoke test for generate_tests
    async def run_test():
        tests = await generator.generate_tests(None, "def foo(): pass")
        print(f"Generated {len(tests)} tests (expected 0 for now).")

    asyncio.run(run_test())
    
except ImportError as e:
    print(f"ImportError: {e}")
    sys.exit(1)
except Exception as e:
    print(f"An error occurred: {e}")
    sys.exit(1)
