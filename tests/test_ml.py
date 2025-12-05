"""Tests for ML risk scoring module."""

import unittest
from src.ml.risk_scorer import RiskScorer, RiskAssessment, CodeMetrics


class TestCodeMetrics(unittest.TestCase):
    """Test CodeMetrics dataclass."""

    def test_default_values(self):
        """Test default metric values."""
        metrics = CodeMetrics()
        self.assertEqual(metrics.lines_of_code, 0)
        self.assertEqual(metrics.cyclomatic_complexity, 0)
        self.assertEqual(metrics.num_goto_statements, 0)

    def test_to_feature_vector(self):
        """Test conversion to numpy array."""
        metrics = CodeMetrics(
            lines_of_code=100,
            cyclomatic_complexity=15,
            num_functions=5,
        )

        vector = metrics.to_feature_vector()
        self.assertEqual(len(vector), 18)  # Number of features
        self.assertEqual(vector[0], 100)  # lines_of_code
        self.assertEqual(vector[1], 15)  # cyclomatic_complexity

    def test_to_dict(self):
        """Test serialization to dictionary."""
        metrics = CodeMetrics(
            lines_of_code=200,
            cyclomatic_complexity=20,
        )

        data = metrics.to_dict()
        self.assertEqual(data["lines_of_code"], 200)
        self.assertEqual(data["cyclomatic_complexity"], 20)


class TestRiskScorer(unittest.TestCase):
    """Test RiskScorer class."""

    def setUp(self):
        """Set up test fixtures."""
        self.scorer = RiskScorer()
        self.sample_cobol = """
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST.
       PROCEDURE DIVISION.
       MAIN-PARA.
           IF A > B
               IF C > D
                   DISPLAY "NESTED".
           GO TO MAIN-PARA.
           STOP RUN.
       """

    def test_extract_metrics(self):
        """Test metric extraction from source code."""
        metrics = self.scorer.extract_metrics(None, self.sample_cobol)

        self.assertGreater(metrics.lines_of_code, 0)
        self.assertGreater(metrics.cyclomatic_complexity, 0)
        self.assertEqual(metrics.num_goto_statements, 1)

    def test_calculate_cyclomatic_complexity(self):
        """Test cyclomatic complexity calculation."""
        code_with_conditions = """
           IF A > B DISPLAY "A".
           IF C > D DISPLAY "C".
           EVALUATE X
               WHEN 1 DISPLAY "1"
               WHEN 2 DISPLAY "2"
           END-EVALUATE.
        """
        complexity = self.scorer._calculate_cyclomatic(code_with_conditions)
        self.assertGreater(complexity, 1)  # Base + decision points

    def test_calculate_nesting_depth(self):
        """Test nesting depth calculation."""
        nested_code = """
           IF A > B
               IF C > D
                   IF E > F
                       DISPLAY "DEEP".
                   END-IF
               END-IF
           END-IF.
        """
        depth = self.scorer._calculate_nesting(nested_code)
        self.assertEqual(depth, 3)

    def test_score_low_risk(self):
        """Test scoring for low risk code."""
        metrics = CodeMetrics(
            lines_of_code=100,
            cyclomatic_complexity=5,
            max_nesting_depth=2,
            num_goto_statements=0,
            comment_ratio=0.2,
        )

        assessment = self.scorer.score(metrics)
        self.assertIsInstance(assessment, RiskAssessment)
        self.assertLess(assessment.overall_score, 50)
        self.assertEqual(assessment.risk_level, "low")

    def test_score_high_risk(self):
        """Test scoring for high risk code."""
        metrics = CodeMetrics(
            lines_of_code=5000,
            cyclomatic_complexity=50,
            max_nesting_depth=8,
            num_goto_statements=20,
            comment_ratio=0.01,
        )

        assessment = self.scorer.score(metrics)
        self.assertGreater(assessment.overall_score, 50)
        self.assertIn(assessment.risk_level, ["high", "critical"])

    def test_recommendations_generated(self):
        """Test that recommendations are generated."""
        metrics = CodeMetrics(
            cyclomatic_complexity=25,
            max_nesting_depth=7,
            num_goto_statements=5,
            comment_ratio=0.02,
        )

        assessment = self.scorer.score(metrics)
        self.assertGreater(len(assessment.recommendations), 0)

    def test_assessment_serialization(self):
        """Test RiskAssessment serialization."""
        assessment = RiskAssessment(
            overall_score=45.0,
            confidence=0.85,
            risk_level="medium",
            recommendations=["Test recommendation"],
        )

        data = assessment.to_dict()
        self.assertEqual(data["overall_score"], 45.0)
        self.assertEqual(data["risk_level"], "medium")


class TestRiskLevels(unittest.TestCase):
    """Test risk level classification."""

    def setUp(self):
        """Set up test fixtures."""
        self.scorer = RiskScorer()

    def test_low_risk_level(self):
        """Test low risk classification (0-25)."""
        metrics = CodeMetrics(lines_of_code=50, cyclomatic_complexity=3)
        assessment = self.scorer.score(metrics)
        self.assertEqual(assessment.risk_level, "low")

    def test_medium_risk_level(self):
        """Test medium risk classification (25-50)."""
        metrics = CodeMetrics(
            lines_of_code=500,
            cyclomatic_complexity=20,
            max_nesting_depth=4,
        )
        assessment = self.scorer.score(metrics)
        self.assertEqual(assessment.risk_level, "medium")


if __name__ == "__main__":
    unittest.main()

