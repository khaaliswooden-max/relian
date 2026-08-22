"""WP-2.4 §3.1 — the JCL parser, and the continuation trap it exists to survive.

**The planted red is the first two tests in this file.** `test_naive_line_scan_
truncates_a_split_dcb` implements the obvious approach — scan each line for
``DCB=(...)`` — and asserts that it comes back with a parameter set that is
*wrong and looks right*: ``LRECL`` and ``RECFM`` present, ``DSORG`` and
``BLKSIZE`` silently absent. `test_parser_recovers_the_full_split_dcb` then
asserts this parser recovers all four from the same bytes.

Keeping the naive scan here, permanently, is the point. A guard whose failure
path has never fired is a guard nobody has measured, and the specific danger of
this one is that the truncated answer is indistinguishable from a DD that
genuinely declares only two sub-parameters. There is no downstream check that
would catch it: ``LRECL`` — the field the §3.2 cross-check turns on — is
exactly the one that survives the truncation.

Everything else here is the surrounding surface: card geometry (the
identification field in columns 73-80 is not operand text), both continuation
rules, the three ways a continuation can be interrupted, in-stream data under
``DD *`` and ``DD DATA``, concatenated DDs, qualified ``procstep.ddname``
overrides, and the DCB sub-parameters themselves.

No CardDemo test lives in this file. CardDemo is cloned outside the repository
(no third-party bytes are committed here, per docs/dryruns/README.md), so a
test needing it would skip on every runner and drift EXPECTED_SKIPS. The
CardDemo measurement is a dry run, recorded in docs/dryruns/carddemo_jcl/.
"""

from __future__ import annotations

import re
import textwrap
from pathlib import Path

import pytest

from src.discovery.jcl import (
    DCB,
    RDW_BYTES,
    Evidence,
    continuation_flag,
    parse_dcb,
    parse_text,
    scan,
    split_operands,
    statement_field,
)

REPO_ROOT = Path(__file__).resolve().parents[1]
DEMO_JCL = REPO_ROOT / "examples" / "demo" / "jcl"


# --------------------------------------------------------------------------
# The fixture the planted red runs on
# --------------------------------------------------------------------------

#: A DD whose ``DCB=`` sub-parameter list is split across three records, with
#: a sequence number in the identification field on two of them. Both hazards
#: at once, because in real JCL that is how they arrive -- CardDemo's
#: `app/app-authorization-ims-db2-mq/jcl/UNLDPADB.JCL` carries `00640019` in
#: columns 73-80 of a `DCB=` record.
def card(statement: str, identification: str = "") -> str:
    """Build one 80-column JCL card.

    Columns 1-71 carry ``statement``, column 72 is left blank, and columns
    73-80 carry ``identification``. Written as a helper rather than inline
    padding because adjacent Python string literals concatenate *before* a
    trailing ``.ljust()`` binds -- which silently produced a fixture with no
    identification field at all, and therefore a fixture that did not test the
    thing it was named for.
    """
    assert len(statement) <= 71, statement
    assert len(identification) <= 8, identification
    return statement.ljust(71) + " " + identification


SPLIT_DCB = "\n".join([
    card("//SPLITJOB JOB (ACCT),'SPLIT DCB',CLASS=A"),
    card("//STEP01   EXEC PGM=CBACT01C"),
    card("//OUTFILE  DD DSN=AWS.M2.CARDDEMO.ACCTDATA.PSCOMP,"),
    card("//            DISP=(NEW,CATLG,DELETE),"),
    card("//            DCB=(LRECL=107,RECFM=FB,", "00010001"),
    card("//            DSORG=PS,BLKSIZE=0),", "00020001"),
    card("//            UNIT=SYSAD,SPACE=(CYL,(1,2),RLSE)"),
]) + "\n"

#: The four sub-parameters the split fixture declares. All four, or the parse
#: is wrong.
FULL_DCB = {"lrecl": 107, "recfm": "FB", "dsorg": "PS", "blksize": 0}


def naive_line_scan(text: str) -> dict:
    """The approach this module exists to be better than.

    One pass, one line at a time, a regex for ``DCB=(...)``. This is not a
    strawman: it is what you write first, it works on every single-record
    ``DCB=`` in CardDemo (39 of the 43), and nothing about its output says it
    failed on the other four.
    """
    for line in text.splitlines():
        match = re.search(r"DCB=\(([^)]*)\)?", line)
        if not match:
            continue
        found = {}
        for item in match.group(1).split(","):
            key, _, value = item.partition("=")
            key = key.strip().lower()
            if key in {"lrecl", "blksize"}:
                try:
                    found[key] = int(value)
                except ValueError:
                    pass
            elif key in {"recfm", "dsorg"}:
                found[key] = value.strip()
        return found
    return {}


def test_naive_line_scan_truncates_a_split_dcb():
    """PLANTED RED. The obvious parser returns a plausible, wrong answer.

    It recovers LRECL and RECFM and drops DSORG and BLKSIZE, and its output
    carries no marker distinguishing that from a DD which declares only two
    sub-parameters. The wrongness is invisible at the call site, which is why
    the real parser may not be written this way.
    """
    truncated = naive_line_scan(SPLIT_DCB)

    assert truncated != FULL_DCB, (
        "the naive scan was expected to FAIL on the split fixture. If it now "
        "succeeds, the fixture stopped exercising the continuation trap and "
        "this file no longer proves anything."
    )
    # And precisely how it fails: the two that survive are the two that make
    # the answer look complete.
    assert truncated.get("lrecl") == 107
    assert truncated.get("recfm") == "FB"
    assert "dsorg" not in truncated
    assert "blksize" not in truncated


def test_parser_recovers_the_full_split_dcb():
    """The guard. Same bytes, all four sub-parameters."""
    member = parse_text(SPLIT_DCB, "SPLITJOB.jcl")

    dds = member.dd_by_name("OUTFILE")
    assert len(dds) == 1
    dcb = dds[0].dcb
    assert dcb is not None
    assert {
        "lrecl": dcb.lrecl,
        "recfm": dcb.recfm,
        "dsorg": dcb.dsorg,
        "blksize": dcb.blksize,
    } == FULL_DCB
    assert dcb.unparsed == ()


def test_split_dcb_statement_records_every_record_it_spanned():
    """The evidence must point at the statement, and name its whole extent."""
    member = parse_text(SPLIT_DCB, "SPLITJOB.jcl")
    dd = member.dd_by_name("OUTFILE")[0]

    assert dd.evidence == Evidence("SPLITJOB.jcl", 3)
    assert dd.lines == (3, 4, 5, 6, 7)
    assert dd.continued is True


def test_sequence_numbers_never_enter_the_operand():
    """Columns 73-80 are an identification field, not operand text.

    Without the column-71 truncation the DCB list would carry `00010001` as a
    positional sub-parameter and BLKSIZE would be read out of a different
    record's sequence number.
    """
    member = parse_text(SPLIT_DCB, "SPLITJOB.jcl")
    dd = member.dd_by_name("OUTFILE")[0]

    assert "00010001" not in dd.operand
    assert "00020001" not in dd.operand
    assert dd.dcb is not None and dd.dcb.blksize == 0


# --------------------------------------------------------------------------
# Card geometry
# --------------------------------------------------------------------------

def test_statement_field_stops_at_column_71():
    record = "//DD1 DD DSN=A.B,".ljust(71) + "X" + "SEQ00001"
    assert statement_field(record) == "//DD1 DD DSN=A.B,"


def test_continuation_flag_reads_column_72_only():
    continued = "//DD1 DD DSN=A.B".ljust(71) + "X"
    plain = card("//DD1 DD DSN=A.B", "SEQ00001")
    assert continuation_flag(continued) is True
    assert continuation_flag(plain) is False


def test_short_record_has_no_continuation_flag():
    assert continuation_flag("//DD1 DD DSN=A.B") is False


# --------------------------------------------------------------------------
# The two continuation rules
# --------------------------------------------------------------------------

def test_comma_rule_continues_a_statement():
    text = (
        "//STEP1 EXEC PGM=P1\n"
        "//IN    DD DSN=A.B,\n"
        "//         DISP=SHR\n"
    )
    dd = parse_text(text, "M.jcl").dd_by_name("IN")[0]
    assert dd.dsn == "A.B"
    assert dd.disp == "SHR"


def test_column_72_rule_continues_a_statement_without_a_comma():
    text = "\n".join([
        card("//STEP1 EXEC PGM=P1"),
        "//IN    DD DSN=A.B".ljust(71) + "X",
        card("//         ,DISP=SHR"),
    ]) + "\n"
    dd = parse_text(text, "M.jcl").dd_by_name("IN")[0]
    assert dd.dsn == "A.B"
    assert dd.disp == "SHR"


def test_continuation_joins_mid_token_without_inserting_whitespace():
    """A column-72 break inside a DSN must rejoin as ONE name.

    This is the case the comma rule cannot cover: there is no comma, so the
    only signal is column 72, and joining with a space would yield the dataset
    ``AWS.M2.CARD DEMO.ACCTDATA`` -- which matches nothing and is not obviously
    wrong in a report.
    """
    text = "\n".join([
        card("//STEP1 EXEC PGM=P1"),
        "//IN    DD DSN=AWS.M2.CARD".ljust(71) + "X",
        card("//         DEMO.ACCTDATA"),
    ]) + "\n"
    dd = parse_text(text, "M.jcl").dd_by_name("IN")[0]
    assert dd.dsn == "AWS.M2.CARDDEMO.ACCTDATA"


# --------------------------------------------------------------------------
# The three ways a continuation is interrupted -- reported, never guessed
# --------------------------------------------------------------------------

def test_continuation_interrupted_by_a_comment_is_reported():
    text = (
        "//STEP1 EXEC PGM=P1\n"
        "//IN    DD DSN=A.B,\n"
        "//* a comment where an operand was promised\n"
        "//         DISP=SHR\n"
    )
    member = parse_text(text, "M.jcl")
    dd = member.dd_by_name("IN")[0]

    assert dd.dsn == "A.B"
    assert dd.disp is None, "DISP was never joined; it must not be invented"
    assert any("interrupted by a comment" in n for n in member.notes)


def test_continuation_interrupted_by_a_new_statement_is_reported():
    text = (
        "//STEP1 EXEC PGM=P1\n"
        "//IN    DD DSN=A.B,\n"
        "//OUT   DD DSN=C.D,DISP=SHR\n"
    )
    member = parse_text(text, "M.jcl")

    assert member.dd_by_name("IN")[0].disp is None
    assert member.dd_by_name("OUT")[0].dsn == "C.D"
    assert any("found a new statement" in n for n in member.notes)


def test_continuation_running_off_the_end_of_the_member_is_reported():
    text = (
        "//STEP1 EXEC PGM=P1\n"
        "//IN    DD DSN=A.B,\n"
    )
    member = parse_text(text, "M.jcl")

    assert member.dd_by_name("IN")[0].dsn == "A.B"
    assert any("the member ends here" in n for n in member.notes)


def test_orphaned_continuation_is_not_reported_as_an_operation():
    """A stranded operand is a stranded operand, not a JCL verb.

    Measured at `app/jcl/CREASTMT.JCL:88`, where the record above it is
    corrupted in the corpus itself. Calling `DSN=AWS.M2...` an unmodelled
    OPERATION would be nonsense dressed as a finding.
    """
    text = (
        "//STEP1 EXEC PGM=P1\n"
        "//IN    DD DSN=A.B\n"
        "//         DSN=C.D\n"
    )
    member = parse_text(text, "M.jcl")

    assert any("orphaned continuation" in n for n in member.notes)
    assert not any("is not modelled" in n for n in member.notes)
    assert len(member.dd_statements) == 1


# --------------------------------------------------------------------------
# In-stream data
# --------------------------------------------------------------------------

def test_in_stream_data_is_not_parsed_as_jcl():
    text = (
        "//STEP1 EXEC PGM=IDCAMS\n"
        "//SYSIN DD *\n"
        "  DELETE AWS.M2.CARDDEMO.X\n"
        "//NOTADD DD DSN=PHANTOM.DATASET\n"
    )
    member = parse_text(text, "M.jcl")
    assert [d.ddname for d in member.dd_statements] == ["SYSIN", "NOTADD"]


def test_dd_star_data_ends_at_the_next_jcl_statement():
    text = (
        "//STEP1 EXEC PGM=IDCAMS\n"
        "//SYSIN DD *\n"
        "  DELETE X\n"
        "//OUT   DD DSN=REAL.DATASET\n"
    )
    member = parse_text(text, "M.jcl")
    assert member.dd_by_name("OUT")[0].dsn == "REAL.DATASET"
    assert member.in_stream_records == 1


def test_dd_data_keeps_slash_slash_records_as_data():
    """Under ``DD DATA`` a ``//`` record is data, terminated only by ``/*``.

    Getting this backwards turns embedded JCL into phantom DD statements
    pointing at datasets no job ever allocates.
    """
    text = (
        "//STEP1 EXEC PGM=IEBGENER\n"
        "//SYSUT1 DD DATA\n"
        "//INNER  DD DSN=NOT.A.REAL.DD\n"
        "/*\n"
        "//OUT    DD DSN=REAL.DATASET\n"
    )
    member = parse_text(text, "M.jcl")
    assert [d.ddname for d in member.dd_statements] == ["SYSUT1", "OUT"]
    assert member.in_stream_records == 1


def test_unterminated_in_stream_block_is_reported():
    text = (
        "//STEP1 EXEC PGM=IKJEFT01\n"
        "//SYSTSIN DD *\n"
        "  RUN PROGRAM(COBTUPDT)\n"
    )
    member = parse_text(text, "M.jcl")
    assert any("without a delimiter" in n for n in member.notes)


# --------------------------------------------------------------------------
# DD shapes
# --------------------------------------------------------------------------

def test_dd_with_no_dsn_reports_none_not_empty_string():
    text = "//STEP1 EXEC PGM=P1\n//SYSOUT DD SYSOUT=*\n"
    dd = parse_text(text, "M.jcl").dd_by_name("SYSOUT")[0]
    assert dd.dsn is None
    assert dd.dataset is None
    assert dd.sysout == "*"


def test_dummy_dd_is_flagged():
    text = "//STEP1 EXEC PGM=P1\n//FSVSAMP DD DUMMY\n"
    dd = parse_text(text, "M.jcl").dd_by_name("FSVSAMP")[0]
    assert dd.dummy is True
    assert dd.dsn is None


def test_concatenated_dds_stay_separate_rows_under_one_name():
    """One DD name, several datasets. Merging them would lose a dataset."""
    text = (
        "//STEP1 EXEC PGM=P1\n"
        "//STEPLIB DD DISP=SHR,DSN=IMS.SDFSRESL\n"
        "//         DD DISP=SHR,DSN=AWS.M2.CARDDEMO.LOADLIB\n"
    )
    dds = parse_text(text, "M.jcl").dd_by_name("STEPLIB")
    assert [d.concatenation_index for d in dds] == [0, 1]
    assert [d.dsn for d in dds] == [
        "IMS.SDFSRESL", "AWS.M2.CARDDEMO.LOADLIB",
    ]


def test_qualified_dd_name_keeps_the_ddname_joinable():
    """``//PRC001.FILEIN DD`` overrides FILEIN inside procedure step PRC001.

    The COBOL says ``ASSIGN TO FILEIN``. Storing the qualified form as the DD
    name would match no SELECT anywhere -- measured on CardDemo's
    `app/jcl/PRTCATBL.jcl:31`, where an unqualified name pattern failed to
    recognise the record as a statement at all and stranded the record after
    it.
    """
    text = (
        "//STEP05R EXEC PROC=REPROC\n"
        "//PRC001.FILEIN  DD DISP=SHR,\n"
        "//        DSN=AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS\n"
    )
    member = parse_text(text, "M.jcl")
    dd = member.dd_by_name("FILEIN")[0]
    assert dd.ddname == "FILEIN"
    assert dd.proc_step == "PRC001"
    assert dd.dsn == "AWS.M2.CARDDEMO.TCATBALF.VSAM.KSDS"


def test_exec_pgm_ties_every_dd_in_the_step_to_a_program():
    """This is the join key for §3.2 -- without it a DD reaches no COBOL."""
    text = (
        "//STEP01 EXEC PGM=CBACT01C\n"
        "//ACCTFILE DD DISP=SHR,DSN=AWS.M2.ACCT.KSDS\n"
        "//STEP02 EXEC PGM=CBACT02C\n"
        "//CARDFILE DD DISP=SHR,DSN=AWS.M2.CARD.KSDS\n"
    )
    member = parse_text(text, "M.jcl")
    assert member.dd_by_name("ACCTFILE")[0].program == "CBACT01C"
    assert member.dd_by_name("CARDFILE")[0].program == "CBACT02C"
    assert member.programs() == ("CBACT01C", "CBACT02C")


def test_dd_before_any_exec_has_no_program():
    """JOBLIB belongs to the job, not to a step. `None`, not the first step."""
    text = (
        "//JOBLIB DD DSN=MMUD.PROD.LOADLIB,DISP=SHR\n"
        "//STEP1  EXEC PGM=P1\n"
    )
    dd = parse_text(text, "M.jcl").dd_by_name("JOBLIB")[0]
    assert dd.program is None
    assert dd.step is None


def test_exec_of_a_bare_procedure_records_the_proc_not_a_program():
    text = "//CICSCMP EXEC BLDCIDB2,MEMNAME=X\n//IN DD DSN=A.B\n"
    member = parse_text(text, "M.jcl")
    assert member.steps[0].proc == "BLDCIDB2"
    assert member.steps[0].program is None
    assert member.dd_by_name("IN")[0].program is None


def test_gdg_relative_reference_is_recorded_and_base_dsn_exposed():
    text = "//STEP1 EXEC PGM=P1\n//BKUP DD DSN=AWS.M2.TRANSACT.BKUP(+1)\n"
    dd = parse_text(text, "M.jcl").dd_by_name("BKUP")[0]
    assert dd.gdg_relative == "+1"
    assert dd.dsn == "AWS.M2.TRANSACT.BKUP(+1)"
    assert dd.dataset == "AWS.M2.TRANSACT.BKUP"


def test_pds_member_is_recorded_and_base_dsn_exposed():
    text = "//STEP1 EXEC PGM=P1\n//CNTL DD DSN=AWS.M2.CARDDEMO.CNTL(SORTCARD)\n"
    dd = parse_text(text, "M.jcl").dd_by_name("CNTL")[0]
    assert dd.member == "SORTCARD"
    assert dd.dataset == "AWS.M2.CARDDEMO.CNTL"


# --------------------------------------------------------------------------
# DCB sub-parameters
# --------------------------------------------------------------------------

def test_parse_dcb_reads_every_modelled_sub_parameter():
    dcb = parse_dcb("(LRECL=107,RECFM=FB,DSORG=PS,BLKSIZE=0)")
    assert (dcb.lrecl, dcb.recfm, dcb.dsorg, dcb.blksize) == (107, "FB", "PS", 0)


def test_blksize_zero_is_a_declared_value_not_a_missing_one():
    """CardDemo writes BLKSIZE=0 meaning 'system-determined'. That is a value."""
    dcb = parse_dcb("(LRECL=107,RECFM=FB,BLKSIZE=0)")
    assert dcb.blksize == 0
    assert dcb.dsorg is None


def test_dcb_referback_is_recorded_and_not_followed():
    """``DCB=(*.SORTIN)`` inherits from another DD. Copying those attributes
    here would present an inference as a declaration."""
    dcb = parse_dcb("(*.SORTIN)")
    assert dcb.referback == "*.SORTIN"
    assert dcb.lrecl is None
    assert dcb.recfm is None


def test_non_integer_lrecl_is_reported_unparsed_and_stays_none():
    """A symbolic LRECL is not a number. `None`, never 0 (R1)."""
    dcb = parse_dcb("(LRECL=&LEN,RECFM=FB)")
    assert dcb.lrecl is None
    assert dcb.recfm == "FB"
    assert any("LRECL" in u for u in dcb.unparsed)


def test_unmodelled_dcb_sub_parameter_is_reported_not_dropped():
    dcb = parse_dcb("(LRECL=80,RECFM=FB,BUFNO=5)")
    assert dcb.lrecl == 80
    assert any("BUFNO" in u for u in dcb.unparsed)


@pytest.mark.parametrize(
    "recfm, variable, rdw",
    [
        ("FB", False, 0),
        ("F", False, 0),
        ("FBA", False, 0),
        ("V", True, RDW_BYTES),
        ("VB", True, RDW_BYTES),
        ("VBS", True, RDW_BYTES),
    ],
)
def test_rdw_bytes_follow_recfm(recfm, variable, rdw):
    dcb = DCB(recfm=recfm)
    assert dcb.is_variable is variable
    assert dcb.rdw_bytes == rdw


def test_unknown_recfm_is_not_treated_as_fixed():
    """`rdw_bytes` is 0, but `recfm` is None -- and §3.2 must look at the
    second before it trusts the first."""
    dcb = DCB(lrecl=80)
    assert dcb.recfm is None
    assert dcb.is_variable is False
    assert dcb.rdw_bytes == 0


# --------------------------------------------------------------------------
# Operand splitting
# --------------------------------------------------------------------------

def test_split_operands_respects_nested_parentheses():
    items, problems = split_operands("DISP=(NEW,CATLG,DELETE),SPACE=(CYL,(1,2),RLSE)")
    assert items == ("DISP=(NEW,CATLG,DELETE)", "SPACE=(CYL,(1,2),RLSE)")
    assert problems == ()


def test_split_operands_respects_quoted_commas():
    items, problems = split_operands("(ACCT),'NIGHTLY,CYCLE',CLASS=A")
    assert items == ("(ACCT)", "'NIGHTLY,CYCLE'", "CLASS=A")
    assert problems == ()


def test_split_operands_reports_an_unclosed_paren():
    items, problems = split_operands("DCB=(LRECL=107,RECFM=FB")
    assert any("unclosed" in p for p in problems)
    assert items


def test_split_operands_reports_an_unbalanced_close_paren():
    """Measured on `app/jcl/CREASTMT.JCL:87`, corrupted in the corpus itself."""
    _items, problems = split_operands("SPACE=(CYL,(1,1),RLSE), 00,RECFM=FB), X")
    assert any("unbalanced" in p for p in problems)


# --------------------------------------------------------------------------
# The committed demo corpus (V7/V8) -- real files, in this repository
# --------------------------------------------------------------------------

def test_demo_jcl_reproduces_the_recorded_counts():
    """V7: 12 DD statements, 7 with DSN=, 1 with DCB=. Measured here."""
    inventory = scan(DEMO_JCL)
    counts = inventory.counts()

    assert counts["members"] == 1
    assert counts["dd_statements"] == 12
    assert counts["dd_with_dsn"] == 7
    assert counts["dd_with_dcb"] == 1


def test_demo_jcl_dcb_is_the_recorded_one():
    inventory = scan(DEMO_JCL)
    dcbs = [d.dcb for d in inventory.dd_statements if d.dcb]
    assert len(dcbs) == 1
    assert (dcbs[0].recfm, dcbs[0].lrecl, dcbs[0].blksize) == ("FB", 80, 8000)


def test_demo_jcl_dcb_survives_its_real_continuation():
    """SUSPENSE's DCB sits on a continuation record in the committed corpus."""
    inventory = scan(DEMO_JCL)
    dd = inventory.members[0].dd_by_name("SUSPENSE")[0]
    assert dd.continued is True
    assert dd.dcb is not None and dd.dcb.lrecl == 80
    assert dd.dsn == "MMUD.PROD.SUSPENSE"


def test_demo_jcl_binds_every_dd_to_its_step_program():
    inventory = scan(DEMO_JCL)
    member = inventory.members[0]
    assert member.dd_by_name("CUSTMAST")[0].program == "MUBPOST"
    assert member.dd_by_name("HISTIN")[0].program == "MUBSURC"
    assert member.dd_by_name("JOBLIB")[0].program is None


def test_scan_of_a_missing_root_raises_rather_than_reporting_zero():
    """Zero members for a path that does not exist is a fabricated measurement."""
    with pytest.raises(FileNotFoundError):
        scan(REPO_ROOT / "no" / "such" / "directory")
