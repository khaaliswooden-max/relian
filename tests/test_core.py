"""Tests for core migration orchestrator module."""

import unittest
import asyncio
import os
import tempfile
from src.core.orchestrator import (
    MigrationOrchestrator,
    MigrationConfig,
    MigrationResult,
    MigrationStatus,
)


class TestMigrationConfig(unittest.TestCase):
    """Test MigrationConfig dataclass."""

    def test_default_values(self):
        """Test default configuration values."""
        config = MigrationConfig(
            source_path="/path/to/source.cbl",
            source_language="cobol",
            target_language="java",
            output_dir="./output",
        )

        self.assertTrue(config.enable_blockchain)
        self.assertTrue(config.generate_tests)
        self.assertEqual(config.risk_threshold, 75.0)
        self.assertIsNone(config.template)

    def test_to_dict(self):
        """Test configuration serialization."""
        config = MigrationConfig(
            source_path="/path/to/source.cbl",
            source_language="cobol",
            target_language="java",
            output_dir="./output",
            template="banking",
        )

        data = config.to_dict()
        self.assertEqual(data["source_path"], "/path/to/source.cbl")
        self.assertEqual(data["template"], "banking")


class TestMigrationResult(unittest.TestCase):
    """Test MigrationResult dataclass."""

    def test_default_values(self):
        """Test default result values."""
        result = MigrationResult(
            migration_id="test123",
            status=MigrationStatus.PENDING,
            source_path="/path/to/source.cbl",
        )

        self.assertEqual(result.semantic_score, 0.0)
        self.assertEqual(result.risk_score, 0.0)
        self.assertEqual(result.errors, [])
        self.assertIsNotNone(result.started_at)

    def test_to_dict(self):
        """Test result serialization."""
        result = MigrationResult(
            migration_id="test123",
            status=MigrationStatus.COMPLETED,
            source_path="/path/to/source.cbl",
            semantic_score=95.0,
            risk_score=25.0,
        )

        data = result.to_dict()
        self.assertEqual(data["migration_id"], "test123")
        self.assertEqual(data["status"], "completed")
        self.assertEqual(data["semantic_score"], 95.0)


class TestMigrationOrchestrator(unittest.TestCase):
    """Test MigrationOrchestrator class."""

    def setUp(self):
        """Set up test fixtures."""
        self.orchestrator = MigrationOrchestrator()

        # Create temporary COBOL source file
        self.temp_dir = tempfile.mkdtemp()
        self.source_file = os.path.join(self.temp_dir, "test.cbl")
        with open(self.source_file, "w", encoding="utf-8") as f:
            f.write("""       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY "HELLO".
           STOP RUN.
""")

    def tearDown(self):
        """Clean up temporary files."""
        import shutil
        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def test_register_parser(self):
        """Test parser registration."""

        class MockParser:
            def parse_string(self, code):
                return None

        self.orchestrator.register_parser("cobol", MockParser())
        # Should not raise
        self.assertIn("cobol", self.orchestrator._parsers)

    def test_register_template(self):
        """Test template registration."""
        self.orchestrator.register_template("banking", {"patterns": []})
        self.assertIn("banking", self.orchestrator._templates)

    def test_progress_callback(self):
        """Test progress callback registration."""
        callbacks_received = []

        def callback(status, message):
            callbacks_received.append((status, message))

        self.orchestrator.add_progress_callback(callback)
        self.orchestrator._update_status(MigrationStatus.PARSING, "Testing")

        self.assertEqual(len(callbacks_received), 1)
        self.assertEqual(callbacks_received[0][0], MigrationStatus.PARSING)

    def test_get_extension(self):
        """Test file extension mapping."""
        self.assertEqual(self.orchestrator._get_extension("java"), "java")
        self.assertEqual(self.orchestrator._get_extension("python"), "py")
        self.assertEqual(self.orchestrator._get_extension("rust"), "rs")
        self.assertEqual(self.orchestrator._get_extension("unknown"), "txt")

    def test_migrate_basic(self):
        """Test basic migration execution."""

        async def run_test():
            config = MigrationConfig(
                source_path=self.source_file,
                source_language="cobol",
                target_language="java",
                output_dir=self.temp_dir,
                enable_blockchain=False,
                generate_tests=False,
            )

            result = await self.orchestrator.migrate(config)

            self.assertIsInstance(result, MigrationResult)
            self.assertIsNotNone(result.migration_id)
            self.assertIn(
                result.status,
                [MigrationStatus.COMPLETED, MigrationStatus.FAILED],
            )

        asyncio.run(run_test())

    def test_migrate_with_cobol_parser(self):
        """Test migration with actual COBOL parser."""
        from src.parsers.cobol import COBOLParser

        self.orchestrator.register_parser("cobol", COBOLParser())

        async def run_test():
            config = MigrationConfig(
                source_path=self.source_file,
                source_language="cobol",
                target_language="java",
                output_dir=self.temp_dir,
                enable_blockchain=False,
                generate_tests=False,
            )

            result = await self.orchestrator.migrate(config)

            self.assertEqual(result.status, MigrationStatus.COMPLETED)
            self.assertIsNotNone(result.source_hash)
            self.assertIsNotNone(result.target_hash)
            self.assertGreater(result.semantic_score, 0)

        asyncio.run(run_test())


class TestMigrationStatus(unittest.TestCase):
    """Test MigrationStatus enum."""

    def test_status_values(self):
        """Test all status values exist."""
        self.assertEqual(MigrationStatus.PENDING.value, "pending")
        self.assertEqual(MigrationStatus.PARSING.value, "parsing")
        self.assertEqual(MigrationStatus.ANALYZING.value, "analyzing")
        self.assertEqual(MigrationStatus.COMPLETED.value, "completed")
        self.assertEqual(MigrationStatus.FAILED.value, "failed")


if __name__ == "__main__":
    unittest.main()

