"""Tests for the end-to-end demo.

The point of most of these is not that the demo produces nice numbers — it is
that it produces *honest* ones. The honesty tests (no fabricated metric when the
oracle is missing; a crash never reported as a designed refusal) matter more
than the happy path, because those are the failures that would make the demo
worse than useless as evidence.
"""

from __future__ import annotations

import shutil
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from demo import cases as cases_mod  # noqa: E402
from demo import oracle, pipeline, report  # noqa: E402

HAVE_COBC = shutil.which("cobc") is not None
needs_cobc = pytest.mark.skipif(not HAVE_COBC, reason="GnuCOBOL (cobc) not installed")


def _case(case_id: str, limit: int = 2):
    found = cases_mod.select([case_id], limit=limit)
    assert found, f"no case matching {case_id!r}"
    return found[0]


# --------------------------------------------------------------------------
# case wiring
# --------------------------------------------------------------------------


def test_every_case_source_exists():
    for case in cases_mod.all_cases():
        assert case.source.exists(), f"{case.case_id}: {case.source} is missing"
        assert case.inputs, f"{case.case_id} has no inputs"


def test_corpus_cases_are_read_only_and_public_only():
    """The demo must never reach for a held-out vector file (rule 1, R3)."""
    for case in cases_mod.corpus_cases():
        assert "heldout" not in case.source.as_posix()
        vectors = case.source.parent / "vectors" / "public.jsonl"
        assert vectors.exists()
        assert len(case.inputs) == len(case.sealed)


def test_input_limit_is_respected():
    for case in cases_mod.all_cases(limit=2):
        assert len(case.inputs) <= 2


# --------------------------------------------------------------------------
# honesty
# --------------------------------------------------------------------------


def test_refusal_is_diagnosed_not_crashed(tmp_path):
    """CUSTUPD must produce a *named* refusal, not a stack trace."""
    result = pipeline.run_case(_case("CUSTUPD"), tmp_path)
    assert result.verdict == pipeline.REFUSED_UNSUPPORTED
    assert result.refused_verb == "SUBTRACT"
    assert result.refused_line is not None
    assert result.refused_paragraph == "MAIN-PARA"
    # Refusal means nothing was produced and nothing is claimed.
    assert result.transpiled is False
    assert result.java_sha256 is None
    assert result.ber is None
    assert result.attestation_gate == "BLOCKED"


def test_crash_is_not_reported_as_a_refusal(tmp_path):
    """A defect in the diagnosis path must not masquerade as designed behavior."""
    result = pipeline.run_case(_case("LEDGRPST"), tmp_path)
    assert result.verdict == pipeline.TRANSPILE_CRASHED
    assert result.verdict != pipeline.REFUSED_UNSUPPORTED
    assert result.refused_verb is None
    assert result.transpile_error
    assert result.ber is None
    assert result.attestation_gate == "BLOCKED"


def test_assessment_overreport_is_recorded(tmp_path):
    """The demo must record that the assessment got LEDGRPST wrong.

    If this ever starts failing because ``prediction_confirmed`` became True,
    the coverage analyzer learned to distinguish PERFORM's forms — good news,
    but the demo's narrative and README need updating with it.
    """
    result = pipeline.run_case(_case("LEDGRPST"), tmp_path)
    assert result.prediction_confirmed is False


def test_no_metric_is_invented_without_the_oracle(tmp_path, monkeypatch):
    """With no COBOL toolchain, equivalence is None — never 0.0, never assumed."""
    monkeypatch.setattr(oracle, "detect",
                        lambda: oracle.OracleInfo(False, None, None))
    result = pipeline.run_case(_case("P01_payroll"), tmp_path)
    assert result.ber is None
    assert result.oracle_agreement is None
    assert result.verdict == pipeline.NOT_MEASURED
    assert result.attestation_gate == "BLOCKED"
    # It still did the work it could do offline.
    assert result.transpiled is True
    assert result.java_build_ok is True


# --------------------------------------------------------------------------
# the happy path
# --------------------------------------------------------------------------


@needs_cobc
def test_supported_program_is_differentially_equivalent(tmp_path):
    result = pipeline.run_case(_case("P01_payroll", limit=3), tmp_path)
    assert result.verdict == pipeline.EQUIVALENCE_MEASURED
    assert result.ber is not None
    assert result.ber.value == 1.0
    assert result.ber.grade == "VERIFIED"
    assert result.vectors_total == 3
    assert result.deterministic is True
    assert result.gaming_violations == []
    assert result.attestation_gate == "PASSED"


@needs_cobc
def test_both_sides_actually_ran(tmp_path):
    """Guard against an equivalence rate computed from two absent executions."""
    result = pipeline.run_case(_case("P01_payroll", limit=2), tmp_path)
    for outcome in result.vectors:
        assert outcome.cobol_stdout is not None
        assert outcome.java_stdout is not None
        assert outcome.cobol_exit is not None
        assert outcome.java_exit is not None


@needs_cobc
def test_local_oracle_reproduces_the_sealed_public_vectors(tmp_path):
    """The live oracle is only trustworthy if it agrees with the sealed record."""
    result = pipeline.run_case(_case("P01_payroll", limit=3), tmp_path)
    assert result.oracle_agreement is not None
    assert result.oracle_agreement.value == 1.0


@needs_cobc
def test_exit_codes_are_compared_not_discarded(tmp_path):
    """P07 exercises nonzero RETURN-CODE; equivalence must include it."""
    result = pipeline.run_case(_case("P07_exitflow"), tmp_path)
    assert result.verdict == pipeline.EQUIVALENCE_MEASURED
    for outcome in result.vectors:
        assert outcome.cobol_exit == outcome.java_exit


# --------------------------------------------------------------------------
# reporting
# --------------------------------------------------------------------------


def test_report_renders_and_never_invents_a_number(tmp_path, monkeypatch):
    monkeypatch.setattr(oracle, "detect",
                        lambda: oracle.OracleInfo(False, None, None))
    run = pipeline.run([_case("P01_payroll")], tmp_path)
    assert run.portfolio_ber() is None
    html = report.render_html(run)
    assert "not measured" in html
    # The report must disclose that nothing is signed here (R4/R5). Scanning
    # prose for words like "simulated" would only catch the disclaimer that
    # says there isn't one, so assert the structural facts instead.
    assert "no signature is issued" in html
    assert "ships no simulated attestation" in html
    # An unmeasured run can never render a passed gate.
    assert "Attestation gate: PASSED" not in html
    assert "Attestation gate: BLOCKED" in html


@needs_cobc
def test_report_includes_the_provenance_of_every_rate(tmp_path):
    run = pipeline.run([_case("P01_payroll", limit=2)], tmp_path)
    html = report.render_html(run)
    assert "VERIFIED" in html
    assert "GnuCOBOL" in html
    pb = run.portfolio_ber()
    assert pb is not None and pb.provenance


@needs_cobc
def test_run_serialises_to_json_safe_types(tmp_path):
    import json

    run = pipeline.run([_case("P01_payroll", limit=2)], tmp_path)
    text = json.dumps(run.to_dict(), sort_keys=True)
    assert json.loads(text)["cases"][0]["ber"]["grade"] == "VERIFIED"
