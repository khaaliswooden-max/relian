"""WP-2.4 D34/R12 — ``src/discovery/`` reads source, never data.

A customer who is handed a document headed "target schema" reasonably wonders
whether we looked at their data to produce it. The answer is no, and in a
HIPAA/CJIS-adjacent environment that answer is a selling point rather than a
footnote — so it is asserted here rather than promised in prose.

Same shape as D15's compiler-free guard, aimed at a different verb. D15 asks
*can this package start a process*; this file asks *can this package open a
dataset*.

(a) **Runtime, and this is the load-bearing one.** A subprocess builds a tree
    containing real source artifacts — a copybook, a COBOL program, a JCL
    member — alongside decoy **datasets**: an EBCDIC record dump, a `.dat`
    extract, a VSAM export. It installs a `sys.addaudithook` that records
    every `open` the interpreter performs, then runs the entire WP-2.4
    pipeline: JCL scan, program scan, the three-source join, the cross-check,
    lineage, DDL generation. Every decoy must be absent from the recorded
    opens.

    An audit hook is not defeatable by a string trick or an indirect import.
    It sees the syscall-level event whichever module made it, including one
    reached transitively through `src.assessment.intake`. That is why it, and
    not the static scan, is the measurement.

    It carries a **converse control**: at least one genuine source file must
    have been opened. Without it, a pipeline that silently did nothing at all
    would pass this test perfectly.

(b) **Static.** The package may not import the machinery whose only purpose is
    reading mainframe data — `struct` and `mmap` for record unpacking, and any
    EBCDIC codec (`cp037`, `ibm037`, `cp1047`, …). A static text analyser has
    no use for either, so their presence is a finding on its own.

(c) **The file-selection constants.** Every extension this package will open
    is enumerated in module constants. None of them may name a data format —
    an over-broad glob is the realistic way a scan starts reading `.dat` files
    without anybody deciding that it should.

(d) **Negative controls.** Each detector is shown to bite on a planted module,
    because a guard that has never failed is a guard nobody has measured.
"""

from __future__ import annotations

import ast
import subprocess          # this TEST may spawn; the package under test may not
import sys
from pathlib import Path
from typing import List, Tuple

import pytest

REPO_ROOT = Path(__file__).resolve().parents[1]
PACKAGE_DIR = REPO_ROOT / "src" / "discovery"

#: Modules whose purpose is unpacking or decoding binary records. There is no
#: legitimate use inside a package that reads COBOL, JCL and copybook text.
FORBIDDEN_MODULES: Tuple[str, ...] = ("struct", "mmap")

#: EBCDIC codecs. A source file is ASCII/UTF-8; decoding EBCDIC is what you do
#: to a dataset, and to nothing else.
EBCDIC_CODECS: Tuple[str, ...] = (
    "cp037", "ibm037", "cp273", "cp500", "ibm500", "cp1047", "ibm1047",
    "cp875", "cp1140", "ibm1140", "ebcdic",
)

#: Extensions that name customer DATA rather than customer SOURCE. None of
#: these may appear in this package's file-selection constants.
DATA_SUFFIXES: Tuple[str, ...] = (
    ".dat", ".data", ".ebc", ".ebcdic", ".vsam", ".ksds", ".esds", ".rrds",
    ".seq", ".ps", ".gdg", ".unl", ".unload", ".extract", ".bin",
)


def package_modules() -> List[Path]:
    return sorted(
        (p for p in PACKAGE_DIR.glob("*.py")), key=lambda p: p.as_posix()
    )


def test_the_package_is_not_empty():
    """A guard over zero files is a guard that passes for the wrong reason."""
    assert len(package_modules()) >= 8


# --------------------------------------------------------------------------
# (a) Runtime — the audit-hook probe
# --------------------------------------------------------------------------

_PROBE = r'''
import json, sys, os
from pathlib import Path

# Run as a script file rather than `-c`, so the repository root is passed in
# rather than inherited from cwd via sys.path[0].
sys.path.insert(0, sys.argv[2])

root = Path(sys.argv[1])
(root / "src").mkdir(parents=True, exist_ok=True)

# --- source artifacts the pipeline is SUPPOSED to read --------------------
(root / "src" / "PROG.cbl").write_text(
    "       IDENTIFICATION DIVISION.\n"
    "       PROGRAM-ID. PROG.\n"
    "       ENVIRONMENT DIVISION.\n"
    "       FILE-CONTROL.\n"
    "           SELECT CUST-FILE ASSIGN TO CUSTDD.\n"
    "       DATA DIVISION.\n"
    "       FILE SECTION.\n"
    "       FD CUST-FILE.\n"
    "       01 CUST-REC.\n"
    "          05 CUST-ID   PIC 9(8).\n"
    "          05 CUST-NAME PIC X(30).\n"
    "       PROCEDURE DIVISION.\n"
    "           OPEN INPUT CUST-FILE.\n",
    encoding="utf-8",
)
(root / "src" / "JOB.jcl").write_text(
    "//JOB1   JOB (ACCT),'X'\n"
    "//STEP1  EXEC PGM=PROG\n"
    "//CUSTDD DD DSN=PROD.CUSTOMER.MASTER,DISP=SHR,\n"
    "//          DCB=(LRECL=38,RECFM=FB)\n",
    encoding="utf-8",
)

# --- DECOYS: customer DATA. Opening any of these is an R12 violation. -----
decoys = {}
for name, payload in (
    ("CUSTOMER.MASTER.dat", b"\xc3\xa4\xa2\xa3\x96\x94\x85\x99" * 64),
    ("acctdata.ebc",        b"\xf0\xf1\xf2\xf3\xf4\xf5" * 64),
    ("extract.vsam",        b"\x00\x01\x02\x03" * 64),
    ("unload.seq",          b"\x40" * 256),
):
    path = root / name
    path.write_bytes(payload)
    decoys[name] = os.path.realpath(str(path))

# --- install the audit hook AFTER the tree exists ------------------------
opened = []

def _hook(event, args):
    if event == "open":
        try:
            opened.append(os.path.realpath(str(args[0])))
        except Exception:
            pass

sys.addaudithook(_hook)

# --- the entire WP-2.4 pipeline ------------------------------------------
from src.discovery.jcl import scan as scan_jcl
from src.discovery.files import scan_programs, build_inventory
from src.discovery.lineage import build_graph, render_markdown
from src.discovery import ddl

jcl = scan_jcl(root / "src")
programs = scan_programs(root / "src")
inventory = build_inventory(programs, jcl)
graph = build_graph(programs, inventory)
render_markdown(graph)

tables = []
for record in inventory.records:
    if record.layout is not None:
        tables.append(ddl.generate_table(record.layout))
if tables:
    ddl.render_schema(tables, schema="probe")

result = {
    "opened": opened,
    "decoys": decoys,
    "touched_decoys": sorted(
        name for name, real in decoys.items() if real in opened
    ),
    "touched_source": sorted(
        p for p in set(opened)
        if p.endswith(".cbl") or p.endswith(".jcl") or p.endswith(".cpy")
    ),
    "dd_count": len(jcl.dd_statements),
    "records": len(inventory.records),
    "edges": graph.coverage.edges_recovered,
    "tables": len(tables),
}
print("RESULT " + json.dumps(result))
'''


@pytest.fixture(scope="module")
def probe_result(tmp_path_factory):
    """Run the pipeline under an audit hook once, share the result."""
    workspace = tmp_path_factory.mktemp("r12probe")
    script = workspace / "probe.py"
    script.write_text(_PROBE, encoding="utf-8")
    proc = subprocess.run(
        [sys.executable, str(script), str(workspace / "tree"), str(REPO_ROOT)],
        cwd=str(REPO_ROOT), capture_output=True, text=True, timeout=180,
    )
    assert proc.returncode == 0, (
        "the R12 probe did not run to completion.\n"
        f"--- stdout ---\n{proc.stdout}\n--- stderr ---\n{proc.stderr}"
    )
    line = next(
        (l for l in proc.stdout.splitlines() if l.startswith("RESULT ")), None
    )
    assert line is not None, proc.stdout
    import json

    return json.loads(line[len("RESULT "):])


def test_no_dataset_is_opened_by_the_whole_pipeline(probe_result):
    """R12, measured: four decoy datasets present, none opened."""
    assert probe_result["touched_decoys"] == [], (
        "src/discovery opened a customer DATA file: "
        f"{probe_result['touched_decoys']}"
    )
    assert len(probe_result["decoys"]) == 4


def test_the_probe_actually_did_the_work(probe_result):
    """Converse control. A pipeline that did nothing would pass the test above
    perfectly, so the run is required to have produced real output."""
    assert probe_result["dd_count"] >= 1
    assert probe_result["records"] >= 1
    assert probe_result["edges"] >= 1
    assert probe_result["tables"] >= 1


def test_the_audit_hook_would_catch_a_dataset_open(tmp_path):
    """NEGATIVE CONTROL for the probe itself.

    The three tests above are only worth their runtime if the hook would
    actually have seen a dataset open. This runs the same probe with one line
    added — a read of a decoy — and requires that it be reported. Without this,
    a hook that silently recorded nothing would make R12 look measured while
    measuring nothing at all.
    """
    import json

    script = tmp_path / "probe_red.py"
    script.write_text(
        _PROBE.replace(
            'print("RESULT " + json.dumps(result))',
            'open(list(decoys.values())[0], "rb").close()\n'
            'result["touched_decoys"] = sorted(\n'
            '    name for name, real in decoys.items() if real in opened\n'
            ')\n'
            'print("RESULT " + json.dumps(result))',
        ),
        encoding="utf-8",
    )
    proc = subprocess.run(
        [sys.executable, str(script), str(tmp_path / "tree"), str(REPO_ROOT)],
        cwd=str(REPO_ROOT), capture_output=True, text=True, timeout=180,
    )
    assert proc.returncode == 0, proc.stderr
    line = next(l for l in proc.stdout.splitlines() if l.startswith("RESULT "))
    result = json.loads(line[len("RESULT "):])

    assert result["touched_decoys"], (
        "the audit hook did not report a dataset open that definitely "
        "happened, so it cannot be relied on to report one that should not"
    )


def test_the_probe_did_open_the_source_artifacts(probe_result):
    """The second half of the converse control: the audit hook is wired up and
    would have seen a dataset open, because it saw the source opens."""
    touched = probe_result["touched_source"]
    assert any(p.endswith("PROG.cbl") for p in touched), touched
    assert any(p.endswith("JOB.jcl") for p in touched), touched


# --------------------------------------------------------------------------
# (b) Static — no record-unpacking or EBCDIC machinery
# --------------------------------------------------------------------------

def _imported_names(tree: ast.AST) -> List[str]:
    names: List[str] = []
    for node in ast.walk(tree):
        if isinstance(node, ast.Import):
            names.extend(alias.name for alias in node.names)
        elif isinstance(node, ast.ImportFrom) and node.module:
            names.append(node.module)
    return names


@pytest.mark.parametrize(
    "module", package_modules(), ids=lambda p: p.name
)
def test_no_record_unpacking_module_is_imported(module):
    tree = ast.parse(module.read_text(encoding="utf-8"))
    for name in _imported_names(tree):
        assert name.split(".")[0] not in FORBIDDEN_MODULES, (
            f"{module.name} imports {name}, whose only use in this package "
            f"would be unpacking binary records"
        )


def _string_literals(tree: ast.AST) -> List[str]:
    """Every string constant that is NOT a docstring.

    Docstrings are exempt for the same reason D15 exempts them: a docstring
    cannot be executed, and forbidding the word would push the reasoning about
    why this package avoids EBCDIC out of the modules that most need to carry
    it.
    """
    docstrings = set()
    for node in ast.walk(tree):
        if isinstance(node, (ast.Module, ast.ClassDef, ast.FunctionDef,
                             ast.AsyncFunctionDef)):
            body = getattr(node, "body", None)
            if (body and isinstance(body[0], ast.Expr)
                    and isinstance(body[0].value, ast.Constant)
                    and isinstance(body[0].value.value, str)):
                docstrings.add(id(body[0].value))
    return [
        node.value for node in ast.walk(tree)
        if isinstance(node, ast.Constant)
        and isinstance(node.value, str)
        and id(node) not in docstrings
    ]


@pytest.mark.parametrize(
    "module", package_modules(), ids=lambda p: p.name
)
def test_no_ebcdic_codec_is_named(module):
    tree = ast.parse(module.read_text(encoding="utf-8"))
    for literal in _string_literals(tree):
        lowered = literal.lower()
        for codec in EBCDIC_CODECS:
            assert codec not in lowered, (
                f"{module.name} names the EBCDIC codec {codec!r}. Source files "
                f"are not EBCDIC; decoding EBCDIC is what one does to a "
                f"dataset."
            )


# --------------------------------------------------------------------------
# (c) The file-selection constants name source, never data
# --------------------------------------------------------------------------

def test_the_suffix_allowlists_contain_no_data_format():
    """An over-broad glob is how a scan starts reading .dat files without
    anybody deciding that it should."""
    from src.discovery.files import PROGRAM_SUFFIXES
    from src.discovery.jcl import JCL_SUFFIXES, PROC_SUFFIXES

    for constant, name in (
        (JCL_SUFFIXES, "jcl.JCL_SUFFIXES"),
        (PROC_SUFFIXES, "jcl.PROC_SUFFIXES"),
        (PROGRAM_SUFFIXES, "files.PROGRAM_SUFFIXES"),
    ):
        for suffix in constant:
            assert suffix.lower() not in DATA_SUFFIXES, (
                f"{name} includes {suffix!r}, which names customer data "
                f"rather than customer source"
            )


def test_the_scan_ignores_a_data_file_sitting_beside_the_source(tmp_path):
    """The allowlist, exercised rather than merely inspected."""
    from src.discovery.jcl import scan

    (tmp_path / "JOB.jcl").write_text(
        "//J JOB (A),'X'\n//S EXEC PGM=P\n//D DD DSN=A.B,DISP=SHR\n",
        encoding="utf-8",
    )
    (tmp_path / "CUSTOMER.dat").write_bytes(b"\xc3\xa4\xa2" * 100)

    inventory = scan(tmp_path)
    assert [m.path for m in inventory.members] == ["JOB.jcl"]


# --------------------------------------------------------------------------
# (d) Negative controls — every detector is shown to bite
# --------------------------------------------------------------------------

_PLANTED_IMPORTS = {
    "import struct": "import struct\n",
    "from struct import unpack": "from struct import unpack\n",
    "import mmap": "import mmap\n",
}


@pytest.mark.parametrize("label", sorted(_PLANTED_IMPORTS))
def test_the_import_detector_bites(label):
    tree = ast.parse(_PLANTED_IMPORTS[label])
    names = [n.split(".")[0] for n in _imported_names(tree)]
    assert any(n in FORBIDDEN_MODULES for n in names), label


_PLANTED_CODECS = {
    "cp037 literal": "ENCODING = 'cp037'\n",
    "ibm037 literal": "ENCODING = 'IBM037'\n",
    "cp1047 in a call": "text = raw.decode('cp1047')\n",
    "ebcdic in a name": "CODEC = 'ebcdic-cp-us'\n",
}


@pytest.mark.parametrize("label", sorted(_PLANTED_CODECS))
def test_the_codec_detector_bites(label):
    tree = ast.parse(_PLANTED_CODECS[label])
    literals = [s.lower() for s in _string_literals(tree)]
    assert any(
        codec in literal for literal in literals for codec in EBCDIC_CODECS
    ), label


def test_the_codec_detector_exempts_docstrings():
    """Converse control. Without the exemption the guard would forbid
    explaining itself, and the explanation is the part that survives review."""
    module = ast.parse('"""We never decode cp037 here."""\nX = 1\n')
    literals = [s.lower() for s in _string_literals(module)]
    assert not any(
        codec in literal for literal in literals for codec in EBCDIC_CODECS
    )


def test_the_suffix_detector_bites():
    planted = (".cbl", ".dat")
    assert any(s.lower() in DATA_SUFFIXES for s in planted)


def test_the_data_suffix_list_is_not_empty():
    """A detector with an empty forbidden set passes everything."""
    assert len(DATA_SUFFIXES) >= 10
    assert len(EBCDIC_CODECS) >= 5
    assert len(FORBIDDEN_MODULES) >= 2
