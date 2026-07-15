#!/usr/bin/env python3
"""
Relian Migration CLI

Command-line interface for running legacy code migrations.

Usage:
    python migrate.py --source <file> --target <language> [options]

Example:
    python migrate.py \
        --source examples/cobol/banking-system.cbl \
        --target java \
        --template banking \
        --output ./output/
"""

import argparse
import asyncio
import os
import sys
from datetime import datetime

# Add src to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))


def print_banner():
    """Print Relian banner."""
    banner = """
    ╔══════════════════════════════════════════════════════════════╗
    ║                                                              ║
    ║   ◈  R E L I A N                                             ║
    ║                                                              ║
    ║   Universal Legacy Refactoring Substrate                     ║
    ║   AI-Powered Migration • Blockchain-Verified                 ║
    ║                                                              ║
    ║   © 2025 Zuup, LLC                                          ║
    ╚══════════════════════════════════════════════════════════════╝
    """
    print(banner)


def print_progress(status, message):
    """Print progress update."""
    status_icons = {
        "pending": "⏳",
        "parsing": "📖",
        "analyzing": "🧠",
        "generating_tests": "🧪",
        "transforming": "🔄",
        "validating": "✅",
        "attesting": "🔗",
        "completed": "🎉",
        "failed": "❌",
    }
    icon = status_icons.get(status.value if hasattr(status, "value") else status, "•")
    print(f"  {icon} {message}")


async def run_migration(args):
    """Execute migration with the orchestrator."""
    from src.core.orchestrator import MigrationOrchestrator, MigrationConfig
    from src.parsers.cobol import COBOLParser

    # Create orchestrator
    orchestrator = MigrationOrchestrator()
    orchestrator.add_progress_callback(print_progress)

    # Register parsers
    orchestrator.register_parser("cobol", COBOLParser())

    # Load submodule-backed plugins (graceful — no-ops if submodules absent)
    plugin_report = orchestrator.load_plugins()
    if args.verbose:
        loaded = plugin_report.get("loaded", 0)
        total = plugin_report.get("total_plugins_attempted", 0)
        print(f"  [plugins] {loaded}/{total} plugins loaded")
        for pname, pinfo in plugin_report.get("details", {}).items():
            status = "OK" if pinfo.get("registered") else "unavailable"
            langs = ", ".join(pinfo.get("supported_languages", []))
            print(f"    {pname}: {status} ({langs})")
        print()

    # Create config
    config = MigrationConfig(
        source_path=args.source,
        source_language=args.source_lang,
        target_language=args.target,
        output_dir=args.output,
        template=args.template,
        enable_blockchain=not args.no_blockchain,
        generate_tests=not args.no_tests,
        risk_threshold=args.risk_threshold,
    )

    print(f"\n📁 Source: {args.source}")
    print(f"🎯 Target: {args.target.upper()}")
    print(f"📋 Template: {args.template or 'default'}")
    print(f"📂 Output: {args.output}")
    print()

    # Run migration
    print("Starting migration...\n")
    result = await orchestrator.migrate(config)

    # Print results
    print("\n" + "=" * 60)
    print("MIGRATION RESULTS")
    print("=" * 60)

    print(f"\nMigration ID: {result.migration_id}")
    print(f"Status: {result.status.value.upper()}")
    print(f"Duration: {result.duration_seconds:.2f} seconds")

    if result.status.value == "completed":
        print(f"\n📊 Metrics:")
        print(f"   • Semantic Score: {result.semantic_score:.1f}%")
        print(f"   • Risk Score: {result.risk_score:.1f}")
        print(f"   • Test Coverage: {result.test_coverage:.1f}%")
        print(f"   • Tests Generated: {result.tests_generated}")

        if result.attestation_tx:
            print(f"\n🔗 Blockchain Attestation:")
            print(f"   TX: {result.attestation_tx[:32]}...")

        if result.output_path:
            print(f"\n📄 Output saved to: {result.output_path}")

    if result.warnings:
        print(f"\n⚠️  Warnings:")
        for warning in result.warnings:
            print(f"   • {warning}")

    if result.errors:
        print(f"\n❌ Errors:")
        for error in result.errors:
            print(f"   • {error}")

    print()
    return 0 if result.status.value == "completed" else 1


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description="Relian - Legacy Code Migration Tool",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s --source code.cbl --target java
  %(prog)s --source code.cbl --target python --template banking
  %(prog)s --source code.cbl --target java --output ./migrated/
        """,
    )

    parser.add_argument(
        "--source",
        "-s",
        required=True,
        help="Path to source code file",
    )
    parser.add_argument(
        "--source-lang",
        default="cobol",
        choices=["cobol", "fortran", "ada", "mumps", "pli"],
        help="Source language (default: cobol)",
    )
    parser.add_argument(
        "--target",
        "-t",
        required=True,
        choices=["java", "python", "rust", "csharp", "cpp", "nodejs"],
        help="Target language for migration",
    )
    parser.add_argument(
        "--template",
        choices=["banking", "government", "healthcare", "manufacturing", "insurance"],
        help="Industry template to use",
    )
    parser.add_argument(
        "--output",
        "-o",
        default="./output",
        help="Output directory (default: ./output)",
    )
    parser.add_argument(
        "--risk-threshold",
        type=float,
        default=75.0,
        help="Maximum acceptable risk score (default: 75.0)",
    )
    parser.add_argument(
        "--no-blockchain",
        action="store_true",
        help="Disable blockchain attestation",
    )
    parser.add_argument(
        "--no-tests",
        action="store_true",
        help="Disable test generation",
    )
    parser.add_argument(
        "--verbose",
        "-v",
        action="store_true",
        help="Enable verbose output",
    )

    args = parser.parse_args()

    # Validate source file exists
    if not os.path.exists(args.source):
        print(f"Error: Source file not found: {args.source}")
        sys.exit(1)

    print_banner()

    # Run migration
    try:
        exit_code = asyncio.run(run_migration(args))
        sys.exit(exit_code)
    except KeyboardInterrupt:
        print("\n\nMigration cancelled by user.")
        sys.exit(130)
    except Exception as e:
        print(f"\n❌ Migration failed: {e}")
        if args.verbose:
            import traceback
            traceback.print_exc()
        sys.exit(1)


if __name__ == "__main__":
    main()

