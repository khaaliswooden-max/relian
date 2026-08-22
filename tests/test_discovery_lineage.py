"""WP-2.4 §3.3 — the lineage graph, and the coverage statement that bounds it.

Two assertions here matter more than the rest.

`test_the_report_cannot_claim_completeness` is the D30 guard: the rendered
lineage section is linted for completeness vocabulary, and the lint itself is
shown to bite on a planted claim. A lineage graph a customer reads as complete
will be acted on as complete, and the flows this method cannot see — dynamic
allocation, GDG generations, TSO-driven datasets, unparsed utilities — are
exactly the ones that break a migration when nobody knew to look.

`test_a_to_b_is_never_an_edge` is the no-transitive-inference guard.
`A → dataset → B` is observed; `A → B` assumes an execution order this analysis
never measured. The inferred pairs exist, are available, and are labelled.
"""

from __future__ import annotations

import textwrap
from pathlib import Path

import pytest

from src.discovery.files import build_inventory, parse_program
from src.discovery.jcl import JclInventory, parse_text
from src.discovery.lineage import (
    COMPLETENESS_VOCABULARY,
    INVISIBLE_TO_THIS_METHOD,
    Direction,
    LineageGraph,
    build_graph,
    lint_completeness_claim,
    render_markdown,
)

REPO_ROOT = Path(__file__).resolve().parents[1]
DEMO = REPO_ROOT / "examples" / "demo"


def cobol(body: str) -> str:
    return "\n".join(
        "       " + line if line.strip() else ""
        for line in textwrap.dedent(body).strip("\n").split("\n")
    ) + "\n"


WRITER = cobol("""
    IDENTIFICATION DIVISION.
    PROGRAM-ID. PRODUCER.
    ENVIRONMENT DIVISION.
    FILE-CONTROL.
        SELECT WORK-FILE ASSIGN TO WORKDD.
    DATA DIVISION.
    FILE SECTION.
    FD WORK-FILE.
    01 WORK-REC.
       05 W-KEY PIC X(10).
    PROCEDURE DIVISION.
        OPEN OUTPUT WORK-FILE.
""")

READER = cobol("""
    IDENTIFICATION DIVISION.
    PROGRAM-ID. CONSUMER.
    ENVIRONMENT DIVISION.
    FILE-CONTROL.
        SELECT WORK-FILE ASSIGN TO WORKDD.
    DATA DIVISION.
    FILE SECTION.
    FD WORK-FILE.
    01 WORK-REC.
       05 W-KEY PIC X(10).
    PROCEDURE DIVISION.
        OPEN INPUT WORK-FILE.
""")

PIPELINE_JCL = (
    "//JOB1     JOB (ACCT),'X'\n"
    "//STEP01   EXEC PGM=PRODUCER\n"
    "//WORKDD   DD DSN=AWS.M2.WORK.EXTRACT,DISP=(NEW,CATLG,DELETE)\n"
    "//STEP02   EXEC PGM=CONSUMER\n"
    "//WORKDD   DD DSN=AWS.M2.WORK.EXTRACT,DISP=SHR\n"
)


def _graph(tmp_path: Path, sources: dict, jcl_text: str) -> LineageGraph:
    programs = []
    for name, text in sources.items():
        path = tmp_path / name
        path.write_text(text, encoding="utf-8")
        programs.append(parse_program(path, tmp_path))
    jcl = JclInventory(root=".", members=(parse_text(jcl_text, "J.jcl"),))
    inventory = build_inventory(programs, jcl)
    return build_graph(programs, inventory)


# --------------------------------------------------------------------------
# Edges
# --------------------------------------------------------------------------

def test_open_output_yields_a_write_edge(tmp_path):
    graph = _graph(tmp_path, {"P.cbl": WRITER}, PIPELINE_JCL)
    edge = next(e for e in graph.edges if e.program == "PRODUCER")
    assert edge.direction is Direction.WRITES
    assert edge.dataset == "AWS.M2.WORK.EXTRACT"
    assert edge.ddname == "WORKDD"


def test_open_input_yields_a_read_edge(tmp_path):
    graph = _graph(tmp_path, {"C.cbl": READER}, PIPELINE_JCL)
    edge = next(e for e in graph.edges if e.program == "CONSUMER")
    assert edge.direction is Direction.READS


@pytest.mark.parametrize(
    "mode, direction",
    [
        ("INPUT", Direction.READS),
        ("OUTPUT", Direction.WRITES),
        ("I-O", Direction.UPDATES),
        ("EXTEND", Direction.APPENDS),
    ],
)
def test_every_open_mode_maps_to_a_direction(tmp_path, mode, direction):
    source = WRITER.replace("OPEN OUTPUT", f"OPEN {mode}")
    graph = _graph(tmp_path, {"P.cbl": source}, PIPELINE_JCL)
    assert graph.edges[0].direction is direction


def test_i_o_is_one_edge_that_both_reads_and_writes(tmp_path):
    """Splitting I-O into two edges double-counts one statement; collapsing it
    to WRITES loses the read."""
    source = WRITER.replace("OPEN OUTPUT", "OPEN I-O")
    graph = _graph(tmp_path, {"P.cbl": source}, PIPELINE_JCL)
    assert len(graph.edges) == 1
    assert graph.readers("AWS.M2.WORK.EXTRACT") == ("PRODUCER",)
    assert graph.writers("AWS.M2.WORK.EXTRACT") == ("PRODUCER",)


def test_an_open_that_binds_to_no_dataset_produces_no_edge(tmp_path):
    """Already a §3.2 finding. Inventing an edge to a DD name would put a
    non-dataset into a dataset graph."""
    graph = _graph(tmp_path, {"P.cbl": WRITER}, "//STEP01 EXEC PGM=PRODUCER\n")
    assert graph.edges == ()
    assert graph.coverage.programs_with_open == 1
    assert graph.coverage.edges_recovered == 0


def test_edges_carry_both_evidences(tmp_path):
    graph = _graph(tmp_path, {"P.cbl": WRITER}, PIPELINE_JCL)
    edge = graph.edges[0]
    assert edge.open_evidence.file == "P.cbl"
    assert edge.jcl_evidence is not None and edge.jcl_evidence.file == "J.jcl"


def test_gdg_relative_reference_is_carried_onto_the_edge(tmp_path):
    jcl = (
        "//STEP01 EXEC PGM=PRODUCER\n"
        "//WORKDD DD DSN=AWS.M2.WORK.EXTRACT(+1),DISP=(NEW,CATLG)\n"
    )
    graph = _graph(tmp_path, {"P.cbl": WRITER}, jcl)
    assert graph.edges[0].gdg_relative == "+1"
    assert graph.edges[0].dataset == "AWS.M2.WORK.EXTRACT"


# --------------------------------------------------------------------------
# No transitive inference
# --------------------------------------------------------------------------

def test_a_to_b_is_never_an_edge(tmp_path):
    """`A → dataset → B` is observed; `A → B` is an inference."""
    graph = _graph(tmp_path, {"P.cbl": WRITER, "C.cbl": READER}, PIPELINE_JCL)

    assert {(e.program, e.dataset) for e in graph.edges} == {
        ("PRODUCER", "AWS.M2.WORK.EXTRACT"),
        ("CONSUMER", "AWS.M2.WORK.EXTRACT"),
    }
    for edge in graph.edges:
        assert edge.dataset not in {"PRODUCER", "CONSUMER"}


def test_inferred_pairs_are_available_and_labelled(tmp_path):
    graph = _graph(tmp_path, {"P.cbl": WRITER, "C.cbl": READER}, PIPELINE_JCL)
    pairs = graph.inferred_program_pairs()

    assert len(pairs) == 1
    assert (pairs[0].producer, pairs[0].consumer) == ("PRODUCER", "CONSUMER")
    assert pairs[0].relationship == "INFERRED"
    assert "not something this analysis measured" in pairs[0].basis


def test_inferred_pairs_are_not_mixed_into_the_edge_list(tmp_path):
    graph = _graph(tmp_path, {"P.cbl": WRITER, "C.cbl": READER}, PIPELINE_JCL)
    rendered = graph.to_dict()
    assert "inferred_program_pairs" in rendered
    assert len(rendered["edges"]) == 2


def test_the_rendered_pair_table_says_inferred_not_observed(tmp_path):
    graph = _graph(tmp_path, {"P.cbl": WRITER, "C.cbl": READER}, PIPELINE_JCL)
    markdown = render_markdown(graph)
    assert "INFERRED, not observed" in markdown


# --------------------------------------------------------------------------
# D30 — coverage, and no completeness claim
# --------------------------------------------------------------------------

def test_coverage_statement_carries_the_three_counts(tmp_path):
    graph = _graph(tmp_path, {"P.cbl": WRITER, "C.cbl": READER}, PIPELINE_JCL)
    statement = graph.coverage.statement()

    assert "2 program(s) scanned" in statement
    assert "2 declare a FILE-CONTROL" in statement
    assert "2 program-to-dataset edge(s) recovered" in statement


def test_coverage_statement_disclaims_completeness(tmp_path):
    graph = _graph(tmp_path, {"P.cbl": WRITER}, PIPELINE_JCL)
    assert "not complete" in graph.coverage.statement()


def test_graph_has_no_completeness_property():
    """A boolean that could read True is a boolean some report will print."""
    forbidden = {"is_complete", "complete", "exhaustive", "is_exhaustive",
                 "percent_complete", "coverage_percent"}
    present = {n for n in dir(LineageGraph) if not n.startswith("_")}
    assert not (present & forbidden), sorted(present & forbidden)


def test_all_four_invisible_categories_are_named_in_the_output(tmp_path):
    graph = _graph(tmp_path, {"P.cbl": WRITER}, PIPELINE_JCL)
    markdown = render_markdown(graph)

    for needle in ("Dynamically allocated", "GDG relative", "TSO-driven",
                   "IDCAMS"):
        assert needle in markdown, needle
    assert len(INVISIBLE_TO_THIS_METHOD) >= 4


def test_the_report_cannot_claim_completeness(tmp_path):
    """D30's guard, over the real rendered section."""
    graph = _graph(tmp_path, {"P.cbl": WRITER, "C.cbl": READER}, PIPELINE_JCL)
    assert lint_completeness_claim(render_markdown(graph)) == []


@pytest.mark.parametrize("phrase", COMPLETENESS_VOCABULARY)
def test_the_completeness_lint_bites_on_a_planted_claim(phrase):
    """Negative control. A lint that has never fired is a lint nobody measured."""
    planted = f"This section documents the {phrase} for the portfolio."
    assert lint_completeness_claim(planted) != []


def test_the_completeness_lint_does_not_fire_on_the_disclaimer():
    """The converse control: the honest sentence contains 'complete' and must
    survive, or the lint would force the disclosure out of the report."""
    honest = (
        "This graph is not complete and is not offered as complete: it shows "
        "the flows that static analysis of COBOL and JCL can see."
    )
    assert lint_completeness_claim(honest) == []


def test_to_dict_states_completeness_is_not_claimed(tmp_path):
    graph = _graph(tmp_path, {"P.cbl": WRITER}, PIPELINE_JCL)
    assert graph.to_dict()["completeness"].startswith("NOT CLAIMED")


# --------------------------------------------------------------------------
# The committed demo corpus
# --------------------------------------------------------------------------

def test_demo_corpus_recovers_edges_with_directions():
    from src.discovery.files import scan_programs
    from src.discovery.jcl import scan

    programs = scan_programs(DEMO / "src")
    inventory = build_inventory(programs, scan(DEMO / "jcl"))
    graph = build_graph(programs, inventory)

    assert graph.coverage.edges_recovered > 0
    assert {e.direction for e in graph.edges} <= set(Direction)
    assert lint_completeness_claim(render_markdown(graph)) == []
