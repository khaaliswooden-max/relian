"""Tests for blockchain attestation module."""

import unittest
import asyncio
from src.blockchain.client import SolanaClient, MigrationAttestation


class TestSolanaClient(unittest.TestCase):
    """Test Solana blockchain client."""

    def setUp(self):
        """Set up test fixtures."""
        self.client = SolanaClient()
        self.sample_source = "IDENTIFICATION DIVISION. PROGRAM-ID. TEST."
        self.sample_target = "public class Test { }"

    def test_hash_artifact(self):
        """Test content hashing."""
        hash1 = self.client.hash_artifact(self.sample_source)
        hash2 = self.client.hash_artifact(self.sample_source)

        # Same content should produce same hash
        self.assertEqual(hash1, hash2)
        # Hash should be 64 characters (SHA256 hex)
        self.assertEqual(len(hash1), 64)

    def test_hash_different_content(self):
        """Test that different content produces different hashes."""
        hash1 = self.client.hash_artifact(self.sample_source)
        hash2 = self.client.hash_artifact(self.sample_target)

        self.assertNotEqual(hash1, hash2)

    def test_connect(self):
        """Test connection to Solana network."""
        result = self.client.connect()
        self.assertTrue(result)
        self.assertTrue(self.client.is_connected())

    def test_create_attestation(self):
        """Test creating a migration attestation."""

        async def run_test():
            attestation = await self.client.create_attestation(
                source_code=self.sample_source,
                target_code=self.sample_target,
                test_coverage=80.0,
                semantic_score=0.95,
                risk_score=25.0,
            )

            self.assertIsInstance(attestation, MigrationAttestation)
            self.assertIsNotNone(attestation.migration_id)
            self.assertEqual(len(attestation.migration_id), 16)
            self.assertIsNotNone(attestation.source_hash)
            self.assertIsNotNone(attestation.target_hash)
            self.assertEqual(attestation.test_coverage, 80.0)
            self.assertEqual(attestation.semantic_score, 0.95)
            self.assertEqual(attestation.risk_score, 25.0)
            self.assertIsNotNone(attestation.transaction_signature)

        asyncio.run(run_test())

    def test_attestation_serialization(self):
        """Test attestation to_dict/from_dict."""
        attestation = MigrationAttestation(
            migration_id="test123",
            source_hash="abc123",
            target_hash="def456",
            test_coverage=85.0,
            semantic_score=0.92,
            risk_score=30.0,
        )

        data = attestation.to_dict()
        self.assertEqual(data["migration_id"], "test123")
        self.assertEqual(data["source_hash"], "abc123")
        self.assertEqual(data["test_coverage"], 85.0)

        # Round trip
        restored = MigrationAttestation.from_dict(data)
        self.assertEqual(restored.migration_id, attestation.migration_id)
        self.assertEqual(restored.source_hash, attestation.source_hash)


class TestMigrationAttestation(unittest.TestCase):
    """Test MigrationAttestation dataclass."""

    def test_default_values(self):
        """Test default values are set correctly."""
        attestation = MigrationAttestation(
            migration_id="test",
            source_hash="abc",
            target_hash="def",
            test_coverage=80.0,
            semantic_score=0.9,
            risk_score=20.0,
        )

        self.assertIsNotNone(attestation.timestamp)
        self.assertEqual(attestation.attesters, [])
        self.assertIsNone(attestation.transaction_signature)


if __name__ == "__main__":
    unittest.main()

