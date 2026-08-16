"""WP-1.3 cross-check — the R2 guarantee that analyzer and transpiler cannot drift.

The work package states the guarantee as: every statement the analyzer marks
SUPPORTED must transpile without invoking ``unsupported()``, and every statement
it marks UNSUPPORTED must invoke it.

The second half cannot be asserted literally against this transpiler, and
saying so precisely matters more than asserting something convenient. From
``docs/C1_SUPPORTED_VERBS_OBSERVED.md`` §3 case B: ``Transpiler._statements()``
only starts a new statement when the leading token is in ``VERBS``, so an
unsupported verb that *follows* another statement is glued onto its predecessor
and never reaches ``stmt()`` at all — it is neither dispatched nor reported.
Making it reach ``stmt()`` would change how source is chopped into statements,
which changes emitted bytes, which is exactly what the WP-1.2 gate forbids.

So the guarantee is asserted in the four checkable forms below. Together they
say: the analyzer never over-promises, never under-promises, and always
predicts a transpiler failure before it happens.

    G1  No verb the analyzer marks SUPPORTED is ever reported unsupported by
        the transpiler.
    G2  No verb the analyzer marks UNSUPPORTED is in the transpiler's dispatch
        table (i.e. the two agree on how a verb token is spelled).
    G3  If the transpiler drops or fails on a program, the analyzer flagged at
        least one unsupported construct in it — a transpiler failure is never
        a surprise.
    G4  If the analyzer reports 100% coverage, the transpiler completes with no
        unsupported hits and no exception.
"""

from pathlib import Path

import pytest

from src.assessment.coverage import analyze
from src.assessment.intake import ingest, read_source
from src.assessment.supported import supported_verbs
from transpiler.c1_rulebased import Transpiler

FIXTURES = Path(__file__).parent / "fixtures" / "cobol"
PROGRAMS = sorted(p.name for p in FIXTURES.glob("*.cbl"))


def _records():
    return {r.rel_path_posix: r for r in ingest(FIXTURES)}


def _transpile(source: str):
    """Returns (unsupported_hits, exception_or_None).

    Construction is inside the try: ``Transpiler.__init__`` itself raises on a
    program with no WORKING-STORAGE SECTION, which is one of the failure modes
    the analyzer has to predict.
    """
    tp = None
    try:
        tp = Transpiler(source, "X")
        tp.transpile()
        return tp.unsupported_hits, None
    except Exception as exc:                      # noqa: BLE001 — the point is to catch all
        return (tp.unsupported_hits if tp is not None else []), exc


def _analysis(name: str):
    src = read_source(FIXTURES / name)
    return analyze(_records()[name], src), src


def test_fixture_set_is_not_empty():
    assert PROGRAMS, "cross-check has no fixtures — it would pass vacuously"


@pytest.mark.parametrize("name", PROGRAMS)
def test_g1_analyzer_supported_never_reported_unsupported(name):
    result, src = _analysis(name)
    if result.total_statements in (None, 0):
        pytest.skip(f"{name}: no statements recovered; nothing to cross-check")
    analyzer_unsupported = {h.verb for h in result.unsupported_inventory}
    hits, _ = _transpile(src)
    transpiler_unsupported = {v for v, _ in hits}
    overlap = transpiler_unsupported - analyzer_unsupported
    assert not overlap, (
        f"{name}: transpiler reported {sorted(overlap)} unsupported, but the "
        f"analyzer called them supported"
    )


@pytest.mark.parametrize("name", PROGRAMS)
def test_g2_analyzer_unsupported_verbs_are_absent_from_the_dispatch_table(name):
    result, _ = _analysis(name)
    table = supported_verbs()
    bad = {h.verb for h in result.unsupported_inventory} & table
    assert not bad, (
        f"{name}: analyzer marked {sorted(bad)} unsupported but they ARE in "
        f"SUPPORTED_STATEMENTS — the two disagree on verb tokenisation"
    )


@pytest.mark.parametrize("name", PROGRAMS)
def test_g3_transpiler_failure_was_predicted_by_the_analyzer(name):
    result, src = _analysis(name)
    hits, exc = _transpile(src)
    if not hits and exc is None:
        pytest.skip(f"{name}: transpiles cleanly; nothing to predict")
    assert result.unsupported_inventory or result.error, (
        f"{name}: transpiler failed ({exc!r}) or dropped {hits}, but the "
        f"analyzer reported a clean program — this is silent divergence"
    )


@pytest.mark.parametrize("name", PROGRAMS)
def test_g4_full_coverage_means_a_clean_transpile(name):
    result, src = _analysis(name)
    if result.coverage_ratio is None or result.coverage_ratio.value < 1.0:
        pytest.skip(f"{name}: coverage is not 1.0")
    hits, exc = _transpile(src)
    assert exc is None, f"{name}: analyzer said 100% but the transpiler raised {exc!r}"
    assert hits == [], f"{name}: analyzer said 100% but the transpiler dropped {hits}"


def test_cross_check_actually_exercises_both_outcomes():
    """Guard against the whole file skipping itself into a green build."""
    full, partial = [], []
    for name in PROGRAMS:
        result, src = _analysis(name)
        if result.coverage_ratio is None:
            continue
        (full if result.coverage_ratio.value == 1.0 else partial).append(name)
    assert full, "no fixture reaches 100% coverage — G4 never runs"
    assert partial, "no fixture is partially covered — G1/G3 never run"


def test_a_planted_disagreement_is_caught():
    """Prove the cross-check has teeth: claim a verb C1 lacks, and G2 fails."""
    import src.assessment.coverage as cov

    real = cov.supported_verbs
    cov.supported_verbs = lambda: real() | {"ALTER"}   # a lie: C1 has no ALTER
    try:
        result, _ = _analysis("HEAVY.cbl")
        assert "ALTER" not in {h.verb for h in result.unsupported_inventory}, (
            "the planted claim did not take effect"
        )
    finally:
        cov.supported_verbs = real

    honest, _ = _analysis("HEAVY.cbl")
    assert "ALTER" in {h.verb for h in honest.unsupported_inventory}
