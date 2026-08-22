"""WP-2.4 §3.3 — program→dataset lineage, and the coverage statement that
bounds it.

``OPEN INPUT|OUTPUT|I-O|EXTEND`` in a program, joined through ``SELECT`` to a
DD name and through the job stream to a DSN, yields a directed edge: this
program reads that dataset, that program writes it. Built from source alone, no
dataset opened (R12).

D30 — the graph is what we can see, and the report says what we cannot
----------------------------------------------------------------------

The edges are real. The graph is **not complete**, and a lineage graph
presented as complete is a claim we cannot support. Four categories of flow are
structurally invisible to this method, and :data:`INVISIBLE_TO_THIS_METHOD`
names all four in the output rather than in a docstring:

* **dynamic allocation** — ``SVC 99``/``BPXWDYN``, allocated at run time from
  values this analysis never sees;
* **GDG relative references** — ``(+1)``/``(0)`` name a generation whose
  absolute dataset depends on the catalog at run time;
* **TSO-driven datasets** — ``EXEC PGM=IKJEFT01`` with allocations in the
  in-stream control cards; and
* **utilities we do not parse** — IDCAMS, SORT, IEBGENER and friends move data
  between datasets on control statements this version does not read.

So :class:`LineageGraph` carries a :class:`Coverage` — *n programs scanned, m
with FILE-CONTROL, k edges recovered* — and there is deliberately no
``is_complete`` that can return True. ``tests/test_discovery_lineage.py``
asserts the report cannot claim completeness.

No transitive inference
-----------------------

If program A writes a dataset and program B reads it, ``A → dataset → B`` is
observed. ``A → B`` as a dependency is an *inference*: it assumes the two run
in an order, in the same cycle, over the same generation. That may well be
true, and it is still not something this module measured. Transitive pairs are
available from :meth:`LineageGraph.inferred_program_pairs`, every one of them
labelled ``INFERRED``, and they are never mixed into ``edges``.
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import Dict, List, Mapping, Optional, Sequence, Tuple

from .files import FileInventory, FileRecord, ProgramFiles
from .jcl import Evidence


class Direction(str, Enum):
    """The direction of a program→dataset edge."""

    READS = "READS"
    WRITES = "WRITES"
    UPDATES = "UPDATES"       # I-O: both, on the same open
    APPENDS = "APPENDS"       # EXTEND: writes, without truncating


#: ``OPEN`` mode → direction. ``I-O`` is a single mode that both reads and
#: writes; splitting it into two edges would double-count one statement, and
#: collapsing it to WRITES would lose the read.
MODE_DIRECTION: Mapping[str, Direction] = {
    "INPUT": Direction.READS,
    "OUTPUT": Direction.WRITES,
    "I-O": Direction.UPDATES,
    "EXTEND": Direction.APPENDS,
}

#: Every category of data flow this method cannot see. Stated in the output.
INVISIBLE_TO_THIS_METHOD: Tuple[str, ...] = (
    "Dynamically allocated datasets (SVC 99 / BPXWDYN / dynamic ALLOCATE): the "
    "dataset name exists only at run time and appears in no JCL this scan "
    "read.",
    "GDG relative references ((+1), (0), (-1)): the generation a job actually "
    "touches depends on the catalog at run time, so an edge to the GDG base is "
    "recorded but the absolute dataset is not determined here.",
    "TSO-driven datasets (EXEC PGM=IKJEFT01 and similar): allocations made in "
    "in-stream control cards are not modelled by this version.",
    "Utilities whose control statements this version does not parse (IDCAMS, "
    "DFSORT/SYNCSORT, IEBGENER): these move data between datasets without any "
    "COBOL OPEN, so the flow is real and no edge is recovered for it.",
    "CICS and IMS resource access (EXEC CICS READ/WRITE, DL/I calls): data "
    "reached through a resource definition rather than a COBOL file, which "
    "this version does not follow.",
    "Programs outside the scanned tree: a dataset written by something we did "
    "not read has no producing edge here, which is not evidence that it has "
    "no producer.",
)


@dataclass(frozen=True)
class Edge:
    """One observed program→dataset flow."""

    program: str
    dataset: str
    direction: Direction
    mode: str
    ddname: Optional[str]
    file_name: Optional[str]
    open_evidence: Evidence
    jcl_evidence: Optional[Evidence]
    gdg_relative: Optional[str] = None

    def to_dict(self) -> Dict[str, object]:
        return {
            "program": self.program,
            "dataset": self.dataset,
            "direction": self.direction.value,
            "mode": self.mode,
            "ddname": self.ddname,
            "file_name": self.file_name,
            "gdg_relative": self.gdg_relative,
            "open_evidence": self.open_evidence.to_dict(),
            "jcl_evidence": (
                self.jcl_evidence.to_dict() if self.jcl_evidence else None
            ),
        }


@dataclass(frozen=True)
class InferredPair:
    """``A → B`` derived from a shared dataset. An inference, and labelled one."""

    producer: str
    consumer: str
    dataset: str
    relationship: str = "INFERRED"
    basis: str = (
        "one program writes this dataset and another reads it. That the two "
        "run in that order, in the same cycle, over the same generation, is "
        "not something this analysis measured."
    )

    def to_dict(self) -> Dict[str, object]:
        return {
            "producer": self.producer,
            "consumer": self.consumer,
            "dataset": self.dataset,
            "relationship": self.relationship,
            "basis": self.basis,
        }


@dataclass(frozen=True)
class Coverage:
    """What the scan looked at, so a reader can bound what it found.

    Every field is counted in the run that produced it. There is no
    ``percent_complete``: the denominator would be the number of flows that
    exist, which is exactly the quantity this method cannot measure.
    """

    programs_scanned: int
    programs_with_file_control: int
    programs_with_open: int
    edges_recovered: int
    datasets_reached: int
    files_without_dd: int
    dd_without_select: int

    def statement(self) -> str:
        """The sentence the report prints. Numbers, then the disclosure."""
        return (
            f"{self.programs_scanned} program(s) scanned; "
            f"{self.programs_with_file_control} declare a FILE-CONTROL "
            f"paragraph; {self.programs_with_open} contain at least one OPEN; "
            f"{self.edges_recovered} program-to-dataset edge(s) recovered "
            f"across {self.datasets_reached} dataset(s). This graph is not "
            f"complete and is not offered as complete: it shows the flows "
            f"that static analysis of COBOL and JCL can see."
        )

    def to_dict(self) -> Dict[str, object]:
        return {
            "programs_scanned": self.programs_scanned,
            "programs_with_file_control": self.programs_with_file_control,
            "programs_with_open": self.programs_with_open,
            "edges_recovered": self.edges_recovered,
            "datasets_reached": self.datasets_reached,
            "files_without_dd": self.files_without_dd,
            "dd_without_select": self.dd_without_select,
            "statement": self.statement(),
        }


@dataclass(frozen=True)
class LineageGraph:
    """Observed edges, plus the coverage that bounds them.

    There is no ``is_complete`` property and no way to set one. The absence is
    the design: a boolean that could read True is a boolean some report will
    print, and this method has no basis for printing it.
    """

    edges: Tuple[Edge, ...]
    coverage: Coverage
    limitations: Tuple[str, ...] = INVISIBLE_TO_THIS_METHOD

    def datasets(self) -> Tuple[str, ...]:
        return tuple(sorted({e.dataset for e in self.edges}))

    def programs(self) -> Tuple[str, ...]:
        return tuple(sorted({e.program for e in self.edges}))

    def writers(self, dataset: str) -> Tuple[str, ...]:
        return tuple(sorted({
            e.program for e in self.edges
            if e.dataset == dataset
            and e.direction in (Direction.WRITES, Direction.UPDATES,
                                Direction.APPENDS)
        }))

    def readers(self, dataset: str) -> Tuple[str, ...]:
        return tuple(sorted({
            e.program for e in self.edges
            if e.dataset == dataset
            and e.direction in (Direction.READS, Direction.UPDATES)
        }))

    def inferred_program_pairs(self) -> Tuple[InferredPair, ...]:
        """``A → B`` pairs through a shared dataset. Every one an inference.

        Kept out of :attr:`edges` on purpose. A caller that wants them must ask
        for them by a name that says what they are.
        """
        pairs: List[InferredPair] = []
        for dataset in self.datasets():
            for producer in self.writers(dataset):
                for consumer in self.readers(dataset):
                    if producer == consumer:
                        continue
                    pairs.append(InferredPair(producer, consumer, dataset))
        return tuple(pairs)

    def to_dict(self) -> Dict[str, object]:
        return {
            "edges": [e.to_dict() for e in self.edges],
            "coverage": self.coverage.to_dict(),
            "limitations": list(self.limitations),
            "inferred_program_pairs": [
                p.to_dict() for p in self.inferred_program_pairs()
            ],
            "completeness": (
                "NOT CLAIMED. See limitations; this graph shows observed flows "
                "only."
            ),
        }


def build_graph(
    programs: Sequence[ProgramFiles], inventory: FileInventory
) -> LineageGraph:
    """Join ``OPEN`` modes to datasets through the §3.2 file inventory."""
    by_program_file: Dict[Tuple[str, str], FileRecord] = {}
    for record in inventory.records:
        if record.program and record.file_name:
            by_program_file.setdefault((record.program, record.file_name), record)

    edges: List[Edge] = []
    programs_with_open = 0
    for program in sorted(programs, key=lambda p: (p.program, p.path)):
        if program.opens:
            programs_with_open += 1
        seen: set = set()
        for statement in program.opens:
            direction = MODE_DIRECTION.get(statement.mode)
            if direction is None:
                continue
            record = by_program_file.get((program.program, statement.file_name))
            if record is None or record.dd is None or record.dd.dataset is None:
                # An OPEN we cannot bind to a dataset is not an edge. It is
                # already reported as a §3.2 finding (a SELECT the job stream
                # never supplies); inventing an edge to a DD name would put a
                # non-dataset into a dataset graph.
                continue
            key = (program.program, record.dd.dataset, direction, statement.file_name)
            if key in seen:
                continue
            seen.add(key)
            edges.append(Edge(
                program=program.program,
                dataset=record.dd.dataset,
                direction=direction,
                mode=statement.mode,
                ddname=record.ddname,
                file_name=statement.file_name,
                open_evidence=statement.evidence,
                jcl_evidence=record.jcl_evidence,
                gdg_relative=record.dd.gdg_relative,
            ))

    edges.sort(key=lambda e: (e.program, e.dataset, e.direction.value))
    coverage = Coverage(
        programs_scanned=len(programs),
        programs_with_file_control=len([p for p in programs if p.has_file_control]),
        programs_with_open=programs_with_open,
        edges_recovered=len(edges),
        datasets_reached=len({e.dataset for e in edges}),
        files_without_dd=len([
            r for r in inventory.records
            if r.select_evidence is not None and r.jcl_evidence is None
        ]),
        dd_without_select=len([
            r for r in inventory.records
            if r.jcl_evidence is not None and r.select_evidence is None
        ]),
    )
    return LineageGraph(edges=tuple(edges), coverage=coverage)


#: Words a lineage section may never apply to itself. Checked by
#: :func:`lint_completeness_claim` and by the report lint.
COMPLETENESS_VOCABULARY: Tuple[str, ...] = (
    "complete lineage",
    "full lineage",
    "every data flow",
    "all data flows",
    "exhaustive lineage",
    "complete data flow",
    "entire data flow",
    "all dataset dependencies",
)


def lint_completeness_claim(text: str) -> List[str]:
    """Return every completeness claim in ``text``. Empty list means clean.

    A lineage graph that a customer reads as complete will be acted on as
    complete, and the four invisible categories above are exactly the flows
    that break a migration when nobody knew to look for them.
    """
    lowered = text.lower()
    return [
        f"lineage output claims completeness: {phrase!r}"
        for phrase in COMPLETENESS_VOCABULARY
        if phrase in lowered
    ]


def render_markdown(graph: LineageGraph) -> str:
    """Render the lineage section, coverage statement first."""
    lines = [
        "## Data lineage (observed)",
        "",
        graph.coverage.statement(),
        "",
        "### What this method cannot see",
        "",
    ]
    lines.extend(f"* {item}" for item in graph.limitations)
    lines.extend([
        "",
        "### Observed program-to-dataset edges",
        "",
        "| Program | Direction | Dataset | DD | OPEN at | JCL at |",
        "|---|---|---|---|---|---|",
    ])
    for edge in graph.edges:
        lines.append(
            f"| {edge.program} | {edge.direction.value} | {edge.dataset} | "
            f"{edge.ddname or '—'} | {edge.open_evidence} | "
            f"{edge.jcl_evidence or '—'} |"
        )
    pairs = graph.inferred_program_pairs()
    if pairs:
        lines.extend([
            "",
            "### Program-to-program relationships (INFERRED, not observed)",
            "",
            "One program writes a dataset and another reads it. That the two "
            "run in that order, in the same cycle, over the same generation is "
            "an inference this analysis did not measure.",
            "",
            "| Producer | Consumer | Through dataset | Basis |",
            "|---|---|---|---|",
        ])
        for pair in pairs:
            lines.append(
                f"| {pair.producer} | {pair.consumer} | {pair.dataset} | "
                f"{pair.relationship} |"
            )
    return "\n".join(lines) + "\n"
