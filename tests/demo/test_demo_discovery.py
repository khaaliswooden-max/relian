"""Tests for the demo's discovery track.

These are honesty tests before they are functionality tests. The discovery
track's job is to put six shipped modules in front of a reader; its *risk* is
that it prints a number nobody measured, or that it prints green when the thing
it checks has gone wrong. So the cases that matter most here are the ones that
break something on purpose and assert that the track says so:

* a corrupted seal must stop every oracle row from being read at all, rather
  than falling back to an unverified answer key;
* a copybook whose bytes have drifted from the sealed corpus must not be
  compared, because agreement would then be a statement about two files;
* an engine layout that disagrees with the oracle must reach ``failures()``,
  which is what puts it in the process exit status;
* a DDL schema that was generated and never executed must report NOT MEASURED,
  never a clean load.

The track needs no compiler and no JDK, so unlike the transform tests nothing
here skips on toolchain. It does need ``cryptography`` for the Ed25519 layers,
which is a hard dependency of the product.
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

import pytest

ROOT = Path(__file__).resolve().parents[2]
if str(ROOT) not in sys.path:
    sys.path.insert(0, str(ROOT))

from demo import discovery as dm                                # noqa: E402
from demo import report as report_mod                           # noqa: E402


@pytest.fixture(scope="module")
def result(tmp_path_factory):
    """One real run over the committed tree, shared by the read-only tests."""
    workdir = tmp_path_factory.mktemp("discovery")
    return dm.run(dm.DEFAULT_ROOT, workdir)


# --------------------------------------------------------------------------
# The tree the demo discovers
# --------------------------------------------------------------------------

def test_the_default_root_is_committed_and_is_not_the_benchmark() -> None:
    """The discovery tree must be in the repository and must not be a bench.

    Pointing the demo at ``bench/`` or ``discovery-bench/`` would make the
    walkthrough a benchmark run, which is the one thing it may never be (R3).
    """
    assert dm.DEFAULT_ROOT.is_dir()
    parts = dm.DEFAULT_ROOT.resolve().parts
    assert "bench" not in parts
    assert "discovery-bench" not in parts


def test_root_is_reported_repo_relative(result) -> None:
    """An absolute path makes two runs of one tree look like two trees (R8)."""
    assert result.root == "examples/demo"


# --------------------------------------------------------------------------
# Stages 1-2: resolve and layout
# --------------------------------------------------------------------------

def test_resolution_is_measured_and_counts_are_recomputed(result) -> None:
    assert result.resolve.status == dm.MEASURED
    assert result.resolve.files_scanned > 0
    assert result.resolve.resolved                      # at least one copybook
    assert result.resolve.copy_edges >= len(result.resolve.resolved)


def test_every_resolved_copybook_gets_a_layout_row(result) -> None:
    """A copybook that resolved and produced no record says so by name.

    The failure this closes is a resolved copybook silently vanishing between
    stages, which would shrink every denominator downstream without a row to
    show for it.
    """
    assert {row.copybook for row in result.layouts} == set(result.resolve.resolved)


def test_a_layout_row_never_carries_a_zero_for_an_unknown_length(result) -> None:
    for row in result.layouts:
        if row.status == "NO_RECORD":
            assert row.group_length is None
        else:
            assert row.group_length is not None and row.group_length > 0


# --------------------------------------------------------------------------
# Stage 3: the sealed oracle — the load-bearing one
# --------------------------------------------------------------------------

def test_oracle_agreement_is_measured_against_the_verified_seal(result) -> None:
    check = result.oracle
    assert check.status == dm.MEASURED, check.reason
    assert check.seal_ok
    assert list(check.seal_layers) == ["tree", "payload", "signature"]
    assert check.comparisons > 0
    assert check.agreed == check.comparisons
    assert not check.mismatches
    assert check.agreement is not None
    assert check.agreement.grade == "VERIFIED"
    assert str(check.comparisons) in check.agreement.provenance


def test_the_agreement_rate_counts_only_comparisons_that_were_made(result) -> None:
    """``not_covered`` is neither a pass nor a fail — it is out of the rate."""
    check = result.oracle
    compared = len(result.resolve.resolved) - len(check.not_covered)
    assert compared > 0
    assert f"over {compared} copybook(s)" in check.agreement.provenance


def test_a_broken_seal_stops_every_oracle_row_from_being_read(
    monkeypatch, tmp_path
) -> None:
    """An unverified answer key is not an answer key.

    The temptation this closes is a fallback: read ``oracle.json`` anyway and
    note the seal problem in passing. Then a tampered oracle produces a green
    cross-check, which is worse than no cross-check at all.
    """
    class _Broken:
        ok = False
        layers = []

        @staticmethod
        def failed_layers():
            return ["signature"]

    import tools.verify_manifest as vm

    monkeypatch.setattr(vm, "verify", lambda *a, **k: _Broken())
    run = dm.run(dm.DEFAULT_ROOT, tmp_path / "work")

    assert run.oracle.status == dm.FAILED
    assert run.oracle.comparisons == 0
    assert not run.oracle.seal_ok
    assert "seal" in (run.oracle.reason or "").lower()
    assert any("oracle cross-check" in f for f in run.failures())


def test_a_copybook_that_drifted_from_the_sealed_corpus_is_not_compared(
    monkeypatch, tmp_path
) -> None:
    """Agreement between two different files is not agreement."""
    real = dm._trusted_oracle()
    tampered = json.loads(json.dumps(real))
    for copybook in tampered["copybooks"]:
        copybook["sha256"] = "0" * 64

    monkeypatch.setattr(dm, "_trusted_oracle", lambda: tampered)
    run = dm.run(dm.DEFAULT_ROOT, tmp_path / "work")

    assert run.oracle.digest_mismatches
    assert run.oracle.comparisons == 0
    assert any("does not match the digest" in f for f in run.failures())


def test_a_disagreeing_layout_reaches_the_exit_status(monkeypatch, tmp_path) -> None:
    """The whole point of the cross-check: a wrong offset must go red."""
    baseline = dm.run(dm.DEFAULT_ROOT, tmp_path / "baseline")
    compared = {
        f"{name}.cpy" for name in baseline.resolve.resolved
        if name not in baseline.oracle.not_covered
    }
    assert compared, "no copybook in the demo tree is covered by the oracle"

    real = dm._trusted_oracle()
    tampered = json.loads(json.dumps(real))
    moved = False
    for copybook in tampered["copybooks"]:
        # Perturbing a copybook the demo tree does not contain would change
        # nothing, and the test would pass for the wrong reason.
        if copybook["file"] not in compared:
            continue
        for variant in copybook["variants"]:
            for field in variant["fields"]:
                field["offset"] = int(field["offset"]) + 1
                moved = True
                break
            break
        break
    assert moved, "the oracle carries no comparable field row to perturb"

    monkeypatch.setattr(dm, "_trusted_oracle", lambda: tampered)
    run = dm.run(dm.DEFAULT_ROOT, tmp_path / "work")

    assert run.oracle.status == dm.FAILED
    assert run.oracle.mismatches
    assert run.oracle.agreed < run.oracle.comparisons
    assert any("disagree with the sealed oracle" in f for f in run.failures())


# --------------------------------------------------------------------------
# Stages 4-5: inventory and lineage
# --------------------------------------------------------------------------

def test_inventory_reports_all_four_outcomes_and_invents_none(result) -> None:
    inventory = result.inventory
    assert inventory.status == dm.MEASURED
    assert set(inventory.outcomes) == {"AGREE", "DISAGREE", "NO_LRECL", "NO_LAYOUT"}
    assert sum(inventory.outcomes.values()) == len(
        [row for row in inventory.rows if row.outcome is not None]
    )


def test_a_cross_check_row_keeps_both_numbers_or_neither(result) -> None:
    """``AGREE``/``DISAGREE`` need both figures; the other two say which is
    missing rather than substituting a zero for it."""
    for row in result.inventory.rows:
        if row.outcome in ("AGREE", "DISAGREE"):
            assert row.computed_length is not None
            assert row.declared_lrecl is not None
        elif row.outcome == "NO_LRECL":
            assert row.declared_lrecl is None
        elif row.outcome == "NO_LAYOUT":
            assert row.computed_length is None


def test_findings_are_not_failures(result) -> None:
    """A finding is a discovery about the estate. If findings reached the exit
    status, a demo over a realistic tree would always be red and the signal
    would be worthless."""
    assert result.inventory.findings
    assert result.failures() == []


def test_lineage_publishes_the_bound_on_its_own_coverage(result) -> None:
    assert result.lineage.status == dm.MEASURED
    statement = result.lineage.coverage_statement or ""
    assert "not complete" in statement
    for _program, direction, _dataset in result.lineage.edges:
        assert direction in {"READS", "WRITES", "UPDATES", "APPENDS"}


def test_every_lineage_edge_names_a_dataset_the_graph_lists(result) -> None:
    assert {dataset for _p, _d, dataset in result.lineage.edges} == set(
        result.lineage.datasets
    )


# --------------------------------------------------------------------------
# Stage 6: DDL
# --------------------------------------------------------------------------

def test_generated_ddl_is_lint_clean_and_counted(result) -> None:
    assert result.ddl.lint_problems == ()
    assert result.ddl.tables == len(
        [row for row in result.layouts if row.status != "NO_RECORD"]
    )
    assert result.ddl.statements > 0


def test_ddl_execution_is_not_measured_without_a_server(monkeypatch, tmp_path) -> None:
    """Generating a schema is not evidence that it loads.

    The zero this closes is the obvious one: an ``executed`` counter that
    defaults to 0 problems and reads, to anyone skimming, as a clean load.
    """
    monkeypatch.delenv(dm.DDL_DSN_ENV, raising=False)
    run = dm.run(dm.DEFAULT_ROOT, tmp_path / "work")

    assert run.ddl.executed_status == dm.NOT_MEASURED
    assert run.ddl.statements_executed is None
    assert run.ddl.columns_reconciled is None
    assert dm.DDL_DSN_ENV in (run.ddl.executed_reason or "")
    assert run.failures() == []


def test_an_unreachable_server_is_not_measured_rather_than_passed(
    monkeypatch, tmp_path
) -> None:
    pytest.importorskip("psycopg2")
    monkeypatch.setenv(dm.DDL_DSN_ENV,
                       "postgresql://nobody:nobody@127.0.0.1:1/does-not-exist")
    run = dm.run(dm.DEFAULT_ROOT, tmp_path / "work")

    assert run.ddl.executed_status == dm.NOT_MEASURED
    assert run.ddl.statements_executed is None
    assert run.ddl.execution_problems == ()


# --------------------------------------------------------------------------
# Stage 7: the signed report
# --------------------------------------------------------------------------

def test_the_report_is_built_signed_and_verified_in_this_run(result) -> None:
    stage = result.report
    assert stage.status == dm.MEASURED, stage.reason
    assert set(stage.artifacts) == {
        "report.json", "report.md", "report.manifest.json"
    }
    assert stage.report_id and stage.manifest_sha256 and stage.instance_fingerprint


def test_the_verifier_reports_unattested_and_the_demo_calls_that_correct(
    result,
) -> None:
    """No countersignature has ever been issued and this demo ships no
    simulated one (R5). UNATTESTED is the right answer, so it must not be a
    failure — and the countersignature layer must read ABSENT, not FAIL."""
    stage = result.report
    assert stage.verify_status == "VALID AND UNATTESTED"
    verdicts = {layer: verdict for layer, verdict, _ in stage.verify_layers}
    assert verdicts["FILES"] == "PASS"
    assert verdicts["MANIFEST"] == "PASS"
    assert verdicts["INSTANCE"] == "PASS"
    assert verdicts["COUNTERSIGNATURE"] == "ABSENT"
    assert result.failures() == []


def test_the_instance_key_is_never_created_in_the_operators_home(
    tmp_path, monkeypatch
) -> None:
    """A demo that provisions a signing identity in ``~/.relian`` is taking an
    action it never announced."""
    fake_home = tmp_path / "home"
    fake_home.mkdir()
    monkeypatch.setenv("HOME", str(fake_home))

    workdir = tmp_path / "work"
    run = dm.run(dm.DEFAULT_ROOT, workdir)

    assert run.report.status == dm.MEASURED
    assert not (fake_home / ".relian").exists()
    assert (workdir / "instance-home" / ".relian").is_dir()


def test_the_perimeter_line_carries_three_digests_and_no_source(result) -> None:
    line = result.report.countersign_request or ""
    assert line.startswith("relian-countersign-request/1 ")
    assert "manifest_sha256=" in line
    assert "report_id=" in line
    assert "instance_fingerprint=" in line
    # Nothing from the customer's tree may appear in it.
    for name in result.resolve.resolved:
        assert name not in line
    assert "MUBPOST" not in line
    for _program, _direction, dataset in result.lineage.edges:
        assert dataset not in line


def test_the_findings_are_byte_reproducible_and_the_manifest_says_it_is_not(
    result,
) -> None:
    """Two builds over one tree agree on ``report.json``. The manifest cannot,
    because it timestamps — and it says so rather than being quietly excluded."""
    assert result.report.report_reproducible is True
    reason = result.report.manifest_differs_reason or ""
    assert "generated_at" in reason and "signed_at" in reason


# --------------------------------------------------------------------------
# Ed25519 absent — the path the documented quickstart actually produced
# --------------------------------------------------------------------------

def _no_signing(monkeypatch) -> None:
    """Make the probe report the backend missing, as a runtime-only install
    would. Patching the probe rather than the import machinery keeps the test
    about the contract — what the track does when signing is unavailable —
    rather than about how the absence is detected."""
    monkeypatch.setattr(
        dm, "signing_backend",
        lambda: "ImportError: No module named 'cryptography' (simulated)",
    )


def test_absent_signing_does_not_crash_the_track(monkeypatch, tmp_path) -> None:
    """The defect this closes: `python3 -m demo` following the README's own
    quickstart raised an unhandled ImportError and exited 1. `cryptography` is
    deliberately outside [project.dependencies], so a runtime-only install
    reaches this path legitimately and must be told, not crashed at."""
    _no_signing(monkeypatch)
    run = dm.run(dm.DEFAULT_ROOT, tmp_path / "work")

    assert run.report.status == dm.NOT_MEASURED
    assert run.oracle.status == dm.NOT_MEASURED
    # Everything that does not need Ed25519 still ran and still measured.
    assert run.resolve.status == dm.MEASURED
    assert run.inventory.status == dm.MEASURED
    assert run.lineage.status == dm.MEASURED
    assert run.layouts


def test_an_uncheckable_seal_is_not_reported_as_a_bad_seal(
    monkeypatch, tmp_path
) -> None:
    """A seal that failed to verify and a seal nobody could check are different
    findings. Reporting the second as the first accuses RELIAN-DISCOVERY-BENCH
    of something no one established."""
    _no_signing(monkeypatch)
    run = dm.run(dm.DEFAULT_ROOT, tmp_path / "work")

    reason = run.oracle.reason or ""
    assert "could not be CHECKED" in reason
    assert "not a statement that the seal is bad" in reason
    assert run.oracle.status is not dm.FAILED
    assert run.oracle.comparisons == 0
    assert run.oracle.agreement is None


def test_absent_signing_is_not_a_defect_in_the_exit_status(
    monkeypatch, tmp_path
) -> None:
    """Same precedent as an absent GnuCOBOL on the transform side: a stage that
    could not run is reported, not counted as a failure."""
    _no_signing(monkeypatch)
    run = dm.run(dm.DEFAULT_ROOT, tmp_path / "work")
    assert run.failures() == []


def test_the_unavailable_reason_names_the_remedy(monkeypatch, tmp_path) -> None:
    """A reason a reader cannot act on wastes the honesty it bought."""
    _no_signing(monkeypatch)
    run = dm.run(dm.DEFAULT_ROOT, tmp_path / "work")
    for reason in (run.oracle.reason or "", run.report.reason or ""):
        assert "cryptography" in reason


def test_signing_backend_reports_available_in_this_environment() -> None:
    """The probe's positive case. If this fails the suite is running without a
    dependency the discovery tests need, and the skip-count gate should say
    so rather than every downstream assertion failing separately."""
    assert dm.signing_backend() is None


# --------------------------------------------------------------------------
# The result object itself
# --------------------------------------------------------------------------

def test_a_missing_tree_is_not_measured_rather_than_empty(tmp_path) -> None:
    run = dm.run(tmp_path / "no-such-tree", tmp_path / "work")

    assert run.resolve.status == dm.NOT_MEASURED
    assert run.oracle.status == dm.NOT_MEASURED
    assert run.layouts == ()
    assert run.inventory.outcomes == {}
    assert run.failures() == []


def test_to_dict_round_trips_as_json_and_carries_every_stage(result) -> None:
    blob = json.dumps(result.to_dict(), sort_keys=True)
    restored = json.loads(blob)
    for key in ("resolve", "layouts", "oracle", "inventory", "lineage", "ddl",
                "report", "failures", "root"):
        assert key in restored
    assert restored["oracle"]["agreement"]["grade"] == "VERIFIED"


def test_no_stage_reports_an_unmeasured_quantity_as_zero(result) -> None:
    """R1 over the serialised form: the fields that stand for measurements are
    ``None`` when absent, never 0."""
    blob = result.to_dict()
    execution = blob["ddl"]["execution"]
    if execution["status"] != dm.MEASURED:
        assert execution["statements_executed"] is None
        assert execution["columns_reconciled"] is None
    if blob["oracle"]["status"] != dm.MEASURED:
        assert blob["oracle"]["agreement"] is None


# --------------------------------------------------------------------------
# Rendering
# --------------------------------------------------------------------------

def test_html_renders_the_discovery_section_without_a_transform_run(result) -> None:
    page = report_mod.render_html(None, result)
    assert "discovery evidence" in page
    assert "Portfolio equivalence" not in page      # nothing it did not measure
    assert result.report.verify_status in page
    assert "22" in page or str(result.oracle.comparisons) in page


def test_html_states_that_discovery_reads_jcl_and_copybooks(result) -> None:
    """The scope note used to say the demo touches no JCL and no copybooks.
    With this track that is false, and a page contradicting itself is worse
    than one that says less."""
    from demo import cases as cases_mod                          # noqa: PLC0415
    from demo import pipeline                                    # noqa: PLC0415

    run = pipeline.RunResult(
        cases=[], oracle_label="none", oracle_available=False,
        transpiler_sha256="0" * 64, git_sha="none",
    )
    page = report_mod.render_html(run, result)
    assert "does read copybooks and" in page
    assert cases_mod is not None
