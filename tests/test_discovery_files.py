"""WP-2.4 §3.2 — SELECT, FD, the three-source join, and the LRECL cross-check.

The load-bearing tests in this file are the ones about `DISAGREE`:

* all four D29 outcomes are exercised by fixtures, including a deliberately
  mismatched pair — an untested finding path is a finding path that has never
  fired, and `DISAGREE` is the outcome this work package exists to produce;
* both numbers survive into the report and neither is dropped; and
* no accessor exists that would let a caller ask for "the" record length,
  because the moment one does, some report will call it and the finding
  evaporates into a single confident number.

The RDW adjustment is tested both ways — `RECFM=FB` (no adjustment) and
`RECFM=VB` (four bytes, stated in the row) — plus the case the corpus will
eventually produce and nobody plans for: a DD whose `DCB=` declares an `LRECL`
and no `RECFM` at all, where assuming "fixed" would silently turn an unknown
into a claim.
"""

from __future__ import annotations

import textwrap
from pathlib import Path

import pytest

from src.discovery.files import (
    CrossCheck,
    CrossCheckOutcome,
    FindingKind,
    build_inventory,
    cross_check,
    parse_file_control,
    parse_file_section,
    parse_opens,
    parse_program,
    scan_programs,
)
from src.discovery.jcl import DCB, DDStatement, Evidence, parse_text

REPO_ROOT = Path(__file__).resolve().parents[1]
DEMO = REPO_ROOT / "examples" / "demo"


def cobol(body: str) -> str:
    """Indent a fixture into fixed-format columns 8+ (area A)."""
    return "\n".join(
        "       " + line if line.strip() else ""
        for line in textwrap.dedent(body).strip("\n").split("\n")
    ) + "\n"


# --------------------------------------------------------------------------
# SELECT
# --------------------------------------------------------------------------

SELECT_SRC = cobol("""
    IDENTIFICATION DIVISION.
    PROGRAM-ID. CBACT01C.
    ENVIRONMENT DIVISION.
    INPUT-OUTPUT SECTION.
    FILE-CONTROL.
        SELECT ACCTFILE-FILE ASSIGN TO ACCTFILE
               ORGANIZATION IS INDEXED
               ACCESS MODE  IS SEQUENTIAL
               RECORD KEY   IS FD-ACCT-ID
               FILE STATUS  IS ACCTFILE-STATUS.
        SELECT OPTIONAL OUT-FILE ASSIGN TO OUTFILE
               ORGANIZATION IS SEQUENTIAL.
""")


def test_select_is_read_across_all_of_its_lines():
    """A SELECT spans five lines here. A per-line scan finds ASSIGN and stops."""
    selects = parse_file_control(SELECT_SRC, "CBACT01C.cbl")
    first = selects[0]

    assert first.file_name == "ACCTFILE-FILE"
    assert first.assign_to == "ACCTFILE"
    assert first.organization == "INDEXED"
    assert first.access_mode == "SEQUENTIAL"
    assert first.record_key == "FD-ACCT-ID"
    assert first.file_status == "ACCTFILE-STATUS"


def test_select_optional_is_recorded():
    selects = parse_file_control(SELECT_SRC, "CBACT01C.cbl")
    assert selects[1].optional is True
    assert selects[1].file_name == "OUT-FILE"
    assert selects[0].optional is False


def test_select_evidence_names_the_line_it_started_on():
    selects = parse_file_control(SELECT_SRC, "CBACT01C.cbl")
    assert selects[0].evidence == Evidence("CBACT01C.cbl", 6)


def test_alternate_record_keys_are_all_collected():
    src = cobol("""
        FILE-CONTROL.
            SELECT F1 ASSIGN TO DD1
                   RECORD KEY IS K-PRIMARY
                   ALTERNATE RECORD KEY IS K-ALT1
                   ALTERNATE RECORD KEY IS K-ALT2.
    """)
    select = parse_file_control(src, "P.cbl")[0]
    assert select.record_key == "K-PRIMARY"
    assert select.alternate_keys == ("K-ALT1", "K-ALT2")


def test_assign_to_environment_prefix_yields_the_dd_name():
    """``ASSIGN TO UT-S-ACCTFILE`` names DD ACCTFILE; the prefix is a device
    hint. Joining on the prefixed string matches no DD in any job stream."""
    src = cobol("""
        FILE-CONTROL.
            SELECT F1 ASSIGN TO UT-S-ACCTFILE.
    """)
    assert parse_file_control(src, "P.cbl")[0].assign_to == "ACCTFILE"


def test_assign_to_literal_is_flagged_rather_than_joined():
    """A quoted target names a file path, not a DD. Silently treating it as a
    DD name produces a join that looks like it failed rather than one that was
    never applicable."""
    src = cobol("""
        FILE-CONTROL.
            SELECT F1 ASSIGN TO 'customer.dat'.
    """)
    select = parse_file_control(src, "P.cbl")[0]
    assert select.assign_literal is True
    assert select.assign_to == "customer.dat"


def test_absent_clauses_are_none_not_defaults():
    src = cobol("""
        FILE-CONTROL.
            SELECT F1 ASSIGN TO DD1.
    """)
    select = parse_file_control(src, "P.cbl")[0]
    assert select.organization is None
    assert select.access_mode is None
    assert select.record_key is None
    assert select.alternate_keys == ()


# --------------------------------------------------------------------------
# FD
# --------------------------------------------------------------------------

FD_SRC = cobol("""
    DATA DIVISION.
    FILE SECTION.
    FD  ACCTFILE-FILE.
    01  FD-ACCTFILE-REC.
        05 FD-ACCT-ID                        PIC 9(11).
        05 FD-ACCT-DATA                      PIC X(289).
    FD OUT-FILE
       RECORDING MODE IS F.
    01 OUT-ACCT-REC.
       05  OUT-ACCT-ID                PIC 9(11).
       05  OUT-ACCT-STATUS            PIC X(01).
    SD SORT-FILE.
    01 SORT-REC.
       05 SORT-KEY PIC X(10).
    WORKING-STORAGE SECTION.
    01 WS-NOT-A-FILE-RECORD PIC X(4).
""")


def test_fd_records_are_bound_to_their_file():
    fds = {f.file_name: f for f in parse_file_section(FD_SRC, "P.cbl")}
    assert fds["ACCTFILE-FILE"].record_names == ("FD-ACCTFILE-REC",)
    assert fds["OUT-FILE"].record_names == ("OUT-ACCT-REC",)


def test_recording_mode_is_read_off_a_continuation_line():
    fds = {f.file_name: f for f in parse_file_section(FD_SRC, "P.cbl")}
    assert fds["OUT-FILE"].recording_mode == "F"


def test_sort_file_is_marked_as_such():
    fds = {f.file_name: f for f in parse_file_section(FD_SRC, "P.cbl")}
    assert fds["SORT-FILE"].is_sort is True
    assert fds["ACCTFILE-FILE"].is_sort is False


def test_working_storage_records_are_not_file_records():
    """The FILE SECTION ends at WORKING-STORAGE. Absorbing the next 01 would
    attach a working variable to a file as its record description."""
    names = {n for f in parse_file_section(FD_SRC, "P.cbl") for n in f.record_names}
    assert "WS-NOT-A-FILE-RECORD" not in names


def test_fd_supplied_by_copy_records_the_member():
    src = cobol("""
        FILE SECTION.
        FD  ACCTFILE-FILE.
        01  FD-ACCT-REC.
            COPY CVACT01Y.
    """)
    fd = parse_file_section(src, "P.cbl")[0]
    assert fd.copy_members == ("CVACT01Y",)


# --------------------------------------------------------------------------
# OPEN
# --------------------------------------------------------------------------

def test_open_attributes_each_file_to_the_mode_in_force():
    """``OPEN INPUT A B OUTPUT C`` is two modes. Attributing C to INPUT would
    reverse the direction of a data-flow edge."""
    src = cobol("""
        PROCEDURE DIVISION.
            OPEN INPUT ACCTFILE-FILE CUSTFILE-FILE OUTPUT REPORT-FILE.
    """)
    opens = {(o.file_name, o.mode) for o in parse_opens(src, "P.cbl")}
    assert opens == {
        ("ACCTFILE-FILE", "INPUT"),
        ("CUSTFILE-FILE", "INPUT"),
        ("REPORT-FILE", "OUTPUT"),
    }


@pytest.mark.parametrize("mode", ["INPUT", "OUTPUT", "I-O", "EXTEND"])
def test_every_open_mode_is_recognised(mode):
    src = cobol(f"""
        PROCEDURE DIVISION.
            OPEN {mode} TRANFILE.
    """)
    opens = parse_opens(src, "P.cbl")
    assert [(o.file_name, o.mode) for o in opens] == [("TRANFILE", mode)]


# --------------------------------------------------------------------------
# The join (D28) — three sources kept apart
# --------------------------------------------------------------------------

JOINED_PROGRAM = cobol("""
    IDENTIFICATION DIVISION.
    PROGRAM-ID. CBACT01C.
    ENVIRONMENT DIVISION.
    FILE-CONTROL.
        SELECT OUT-FILE ASSIGN TO OUTFILE
               ORGANIZATION IS SEQUENTIAL.
    DATA DIVISION.
    FILE SECTION.
    FD OUT-FILE.
    01 OUT-ACCT-REC.
       05  OUT-ACCT-ID                PIC 9(11).
       05  OUT-ACCT-STATUS            PIC X(01).
""")

JOINED_JCL = (
    "//JOB1     JOB (ACCT),'X'\n"
    "//STEP05   EXEC PGM=CBACT01C\n"
    "//OUTFILE  DD DSN=AWS.M2.CARDDEMO.ACCTDATA.PSCOMP,\n"
    "//            DCB=(LRECL=12,RECFM=FB,DSORG=PS,BLKSIZE=0)\n"
)


def _inventory(program_src: str, jcl_src: str, tmp_path: Path):
    source = tmp_path / "P.cbl"
    source.write_text(program_src, encoding="utf-8")
    program = parse_program(source, tmp_path)
    member = parse_text(jcl_src, "J.jcl")
    from src.discovery.jcl import JclInventory

    return build_inventory([program], JclInventory(root=".", members=(member,)))


def test_all_three_evidences_are_present_and_distinct(tmp_path):
    inventory = _inventory(JOINED_PROGRAM, JOINED_JCL, tmp_path)
    record = next(r for r in inventory.records if r.ddname == "OUTFILE")

    assert record.select_evidence is not None
    assert record.fd_evidence is not None
    assert record.jcl_evidence is not None
    assert record.select_evidence.file == "P.cbl"
    assert record.jcl_evidence.file == "J.jcl"
    assert record.sources == ("SELECT", "FD", "JCL")


def test_a_dataset_only_jcl_knows_is_a_finding(tmp_path):
    """A DD the job stream allocates that no SELECT in scope declares."""
    jcl = (
        "//STEP05 EXEC PGM=CBACT01C\n"
        "//ORPHAN DD DSN=AWS.M2.CARDDEMO.NOBODY.DECLARES,DISP=SHR\n"
    )
    inventory = _inventory(JOINED_PROGRAM, jcl, tmp_path)
    record = next(r for r in inventory.records if r.ddname == "ORPHAN")

    assert record.sources == ("JCL",)
    assert record.select_evidence is None
    assert record.fd_evidence is None
    assert [f.kind for f in record.findings] == [FindingKind.DATASET_NOT_DECLARED]


def test_a_select_the_job_stream_never_supplies_is_a_finding(tmp_path):
    jcl = "//STEP05 EXEC PGM=CBACT01C\n//OTHER DD DSN=A.B,DISP=SHR\n"
    inventory = _inventory(JOINED_PROGRAM, jcl, tmp_path)
    record = next(r for r in inventory.records if r.ddname == "OUTFILE")

    assert record.jcl_evidence is None
    assert record.select_evidence is not None
    assert FindingKind.FILE_NOT_SUPPLIED in {f.kind for f in record.findings}


def test_a_select_with_no_fd_is_a_finding(tmp_path):
    src = cobol("""
        PROGRAM-ID. P1.
        FILE-CONTROL.
            SELECT LONELY-FILE ASSIGN TO LONELY.
    """)
    inventory = _inventory(src, "//STEP1 EXEC PGM=P1\n", tmp_path)
    record = next(r for r in inventory.records if r.file_name == "LONELY-FILE")

    assert record.fd_evidence is None
    assert FindingKind.SELECT_WITHOUT_FD in {f.kind for f in record.findings}


def test_an_fd_with_no_select_is_a_finding(tmp_path):
    src = cobol("""
        PROGRAM-ID. P1.
        DATA DIVISION.
        FILE SECTION.
        FD UNBOUND-FILE.
        01 UNBOUND-REC.
           05 A PIC X(4).
    """)
    inventory = _inventory(src, "//STEP1 EXEC PGM=P1\n", tmp_path)
    record = next(r for r in inventory.records if r.file_name == "UNBOUND-FILE")

    assert record.select_evidence is None
    assert record.fd_evidence is not None
    assert FindingKind.FD_WITHOUT_SELECT in {f.kind for f in record.findings}


def test_merging_the_sources_is_not_how_the_record_is_built(tmp_path):
    """Each source is independently nullable — that is what makes the two
    findings above expressible at all."""
    inventory = _inventory(JOINED_PROGRAM, JOINED_JCL, tmp_path)
    combinations = {r.sources for r in inventory.records}
    assert ("SELECT", "FD", "JCL") in combinations
    for record in inventory.records:
        for name in ("select_evidence", "fd_evidence", "jcl_evidence"):
            value = getattr(record, name)
            assert value is None or isinstance(value, Evidence)


# --------------------------------------------------------------------------
# D29 — the four outcomes
# --------------------------------------------------------------------------

def _dd(lrecl=None, recfm=None, ddname="OUTFILE", dsn="A.B.C"):
    dcb = None
    if lrecl is not None or recfm is not None:
        dcb = DCB(lrecl=lrecl, recfm=recfm, raw="(fixture)")
    return DDStatement(
        ddname=ddname, step="STEP1", program="P1", dsn=dsn, dcb=dcb,
        evidence=Evidence("J.jcl", 3),
    )


def test_outcome_agree():
    check = cross_check(computed_length=300, dd=_dd(lrecl=300, recfm="FB"))
    assert check.outcome is CrossCheckOutcome.AGREE
    assert check.delta == 0
    assert check.is_finding is False


def test_outcome_disagree():
    check = cross_check(computed_length=300, dd=_dd(lrecl=350, recfm="FB"))
    assert check.outcome is CrossCheckOutcome.DISAGREE
    assert check.is_finding is True


def test_outcome_no_lrecl():
    """A layout with nothing external to check it against."""
    check = cross_check(computed_length=300, dd=_dd())
    assert check.outcome is CrossCheckOutcome.NO_LRECL
    assert check.computed_length == 300
    assert check.declared_lrecl is None
    assert "unverified against any source outside the copybook" in check.basis


def test_outcome_no_layout():
    """A dataset with a declared length and no record description."""
    check = cross_check(
        computed_length=None, dd=_dd(lrecl=350, recfm="FB"),
        layout_reason="COPY CVACT01Y is unresolved",
    )
    assert check.outcome is CrossCheckOutcome.NO_LAYOUT
    assert check.declared_lrecl == 350
    assert check.computed_length is None
    assert "COPY CVACT01Y is unresolved" in check.basis


def test_outcome_when_neither_side_is_known_says_it_checked_nothing():
    check = cross_check(computed_length=None, dd=None)
    assert check.outcome is CrossCheckOutcome.NO_LAYOUT
    assert "checks nothing" in check.basis


# --------------------------------------------------------------------------
# D29 — a disagreement is never resolved
# --------------------------------------------------------------------------

MISMATCHED_PROGRAM = cobol("""
    IDENTIFICATION DIVISION.
    PROGRAM-ID. MISMATCH.
    ENVIRONMENT DIVISION.
    FILE-CONTROL.
        SELECT OUT-FILE ASSIGN TO OUTFILE.
    DATA DIVISION.
    FILE SECTION.
    FD OUT-FILE.
    01 OUT-REC.
       05  OUT-ID     PIC 9(11).
       05  OUT-STATUS PIC X(01).
""")

#: The copybook computes 12 bytes. The JCL declares 107. They are both from the
#: customer, and neither is negotiable from source.
MISMATCHED_JCL = (
    "//STEP05  EXEC PGM=MISMATCH\n"
    "//OUTFILE DD DSN=AWS.M2.CARDDEMO.ACCTDATA.PSCOMP,\n"
    "//           DCB=(LRECL=107,RECFM=FB,DSORG=PS,BLKSIZE=0)\n"
)


def test_a_deliberately_mismatched_pair_reports_disagree(tmp_path):
    inventory = _inventory(MISMATCHED_PROGRAM, MISMATCHED_JCL, tmp_path)
    record = next(r for r in inventory.records if r.ddname == "OUTFILE")

    assert record.check is not None
    assert record.check.outcome is CrossCheckOutcome.DISAGREE
    assert inventory.disagreements() == (record,)


def test_both_numbers_survive_into_the_report_and_neither_is_dropped(tmp_path):
    """The deliverable. Both figures reach the row, the delta is signed, and
    the conflict names what disagrees."""
    inventory = _inventory(MISMATCHED_PROGRAM, MISMATCHED_JCL, tmp_path)
    check = next(r for r in inventory.records if r.ddname == "OUTFILE").check
    assert check is not None

    assert check.computed_length == 12
    assert check.declared_lrecl == 107
    assert check.delta == -95

    row = check.to_dict()
    assert row["computed_length"] == 12
    assert row["declared_lrecl"] == 107
    assert row["conflict"] is not None
    assert "12" in row["conflict"] and "107" in row["conflict"]


def test_the_conflict_says_relian_does_not_choose(tmp_path):
    inventory = _inventory(MISMATCHED_PROGRAM, MISMATCHED_JCL, tmp_path)
    check = next(r for r in inventory.records if r.ddname == "OUTFILE").check
    assert check is not None and check.conflict is not None
    assert "does not choose between these two numbers" in check.conflict


def test_no_resolving_accessor_exists():
    """A guard, not a style note.

    The moment `CrossCheck` grows a `record_length` — or a `preferred`, or a
    `resolved`, or a `best` — some report will render it, and a DISAGREE
    becomes one confident number with the finding filed off. The forbidden
    names are checked by shape rather than by review.
    """
    forbidden = {
        "record_length", "length", "resolved", "resolve", "preferred",
        "best", "chosen", "reconcile", "reconciled", "authoritative",
        "actual", "true_length", "effective_length",
    }
    present = {n for n in dir(CrossCheck) if not n.startswith("_")}
    assert not (present & forbidden), (
        f"CrossCheck grew a resolving accessor: {sorted(present & forbidden)}. "
        f"The cross-check must report both numbers and pick neither."
    )


def test_disagreement_is_not_silently_absorbed_by_a_tolerance():
    """An off-by-one is still a disagreement. A 'close enough' band is how a
    reconciliation gets reintroduced without anyone naming it."""
    check = cross_check(computed_length=300, dd=_dd(lrecl=301, recfm="FB"))
    assert check.outcome is CrossCheckOutcome.DISAGREE
    assert check.delta == -1


# --------------------------------------------------------------------------
# D29 — the RDW adjustment, both ways, and stated
# --------------------------------------------------------------------------

def test_fixed_records_are_compared_without_an_rdw_adjustment():
    check = cross_check(computed_length=300, dd=_dd(lrecl=300, recfm="FB"))
    assert check.rdw_bytes == 0
    assert check.adjusted_computed == 300
    assert check.outcome is CrossCheckOutcome.AGREE
    assert "no Record Descriptor Word" in check.basis


def test_variable_records_are_compared_with_the_four_byte_rdw():
    """Computed 80 + 4 == declared 84. This is CardDemo's VBRCFILE."""
    check = cross_check(computed_length=80, dd=_dd(lrecl=84, recfm="VB"))
    assert check.rdw_bytes == 4
    assert check.adjusted_computed == 84
    assert check.outcome is CrossCheckOutcome.AGREE


def test_the_rdw_adjustment_is_stated_in_the_row_not_folded_in():
    """An unexplained ±4 reads as a defect to anyone checking by hand."""
    check = cross_check(computed_length=80, dd=_dd(lrecl=84, recfm="VB"))
    assert "Record Descriptor Word" in check.basis
    assert "80 + 4 = 84" in check.basis
    assert "RECFM=VB" in check.basis


def test_without_the_rdw_adjustment_the_same_pair_would_read_as_a_disagreement():
    """The converse control: the adjustment is doing work, not decorating."""
    unadjusted = cross_check(computed_length=80, dd=_dd(lrecl=84, recfm="FB"))
    assert unadjusted.outcome is CrossCheckOutcome.DISAGREE
    assert unadjusted.delta == -4


def test_variable_records_that_still_disagree_report_the_adjusted_delta():
    check = cross_check(computed_length=80, dd=_dd(lrecl=100, recfm="VB"))
    assert check.outcome is CrossCheckOutcome.DISAGREE
    assert check.adjusted_computed == 84
    assert check.delta == -16


def test_undeclared_recfm_is_not_assumed_fixed():
    """No RECFM means no adjustment AND a row that says why, plus what the
    answer would have been had the dataset been variable-length."""
    check = cross_check(computed_length=80, dd=_dd(lrecl=84))
    assert check.recfm is None
    assert check.rdw_bytes == 0
    assert check.outcome is CrossCheckOutcome.DISAGREE
    assert "RECFM is not declared" in check.basis
    assert "would exceed the computed length by 4" in check.basis


# --------------------------------------------------------------------------
# The committed demo corpus (V8)
# --------------------------------------------------------------------------

def test_demo_program_reproduces_its_recorded_select_and_fd_counts():
    """V8: MUBPOST.cbl — 1 FILE-CONTROL, 4 SELECT, 1 FILE SECTION, 4 FD."""
    programs = scan_programs(DEMO / "src")
    mubpost = next(p for p in programs if p.program == "MUBPOST")

    assert mubpost.has_file_control is True
    assert len(mubpost.selects) == 4
    assert len(mubpost.fds) == 4


def test_demo_join_binds_every_select_to_its_dd():
    from src.discovery.jcl import scan

    programs = scan_programs(DEMO / "src")
    inventory = build_inventory(programs, scan(DEMO / "jcl"))
    bound = {
        r.ddname for r in inventory.records
        if r.select_evidence is not None and r.jcl_evidence is not None
    }
    assert "CUSTMAST" in bound
    assert "SUSPENSE" in bound


# --------------------------------------------------------------------------
# A cross-check row must be able to name the record it checked
# --------------------------------------------------------------------------

def test_a_copy_supplied_record_is_named_from_the_computed_layout(tmp_path):
    """An FD whose record arrives through COPY names no 01 of its own.

    Falling back to the layout's group is what stops such a row reading
    "record: (unnamed)". Measured on CardDemo: four of the thirteen AGREE rows
    are COPY-supplied, so without this a third of the strongest evidence the
    cross-check produces could not say what it was about.
    """
    from src.discovery.copybook import resolve
    from src.discovery.files import _record_name
    from src.discovery.jcl import JclInventory

    book = tmp_path / "CVACT01Y.cpy"
    book.write_text(cobol("""
        01  ACCOUNT-RECORD.
            05  ACCT-ID    PIC 9(11).
            05  ACCT-NAME  PIC X(19).
    """), encoding="utf-8")
    source = tmp_path / "P.cbl"
    source.write_text(cobol("""
        IDENTIFICATION DIVISION.
        PROGRAM-ID. CBIMPORT.
        ENVIRONMENT DIVISION.
        FILE-CONTROL.
            SELECT ACCOUNT-OUTPUT ASSIGN TO ACCTOUT.
        DATA DIVISION.
        FILE SECTION.
        FD ACCOUNT-OUTPUT.
        01 ACCT-OUT-REC.
           COPY CVACT01Y.
    """), encoding="utf-8")

    program = parse_program(source, tmp_path)
    jcl = JclInventory(root=".", members=(parse_text(
        "//STEP1  EXEC PGM=CBIMPORT\n"
        "//ACCTOUT DD DSN=A.B,DCB=(LRECL=30,RECFM=FB)\n", "J.jcl"),))
    inventory = build_inventory([program], jcl, resolve(tmp_path))

    record = next(r for r in inventory.records if r.ddname == "ACCTOUT")
    assert record.fd is not None and record.fd.copy_members == ("CVACT01Y",)
    assert record.fd.record_names == ("ACCT-OUT-REC",)
    assert record.check is not None
    assert record.check.record_name is not None
    assert record.check.outcome is CrossCheckOutcome.AGREE

    # And the fallback itself, directly: no FD 01 name, layout supplies it.
    assert _record_name(None, record.layout) == "ACCOUNT-RECORD"
    assert _record_name(None, None) is None


def test_the_conflict_text_names_the_record_rather_than_unnamed(tmp_path):
    """A DISAGREE a customer cannot trace to a record is a finding they cannot
    act on."""
    inventory = _inventory(MISMATCHED_PROGRAM, MISMATCHED_JCL, tmp_path)
    check = next(r for r in inventory.records if r.ddname == "OUTFILE").check
    assert check is not None and check.conflict is not None
    assert check.record_name == "OUT-REC"
    assert "(unnamed)" not in check.conflict
