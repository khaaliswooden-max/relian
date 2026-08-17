"""What the demo runs over.

Two sources of programs:

**The committed corpus** (``bench/corpus/*/program.cbl``), read strictly
read-only. Its PUBLIC vectors supply the inputs — and only the inputs. The
``expected`` field is never used to decide whether the migration is correct;
that verdict comes from executing the COBOL in this run. The sealed expectation
is used for one narrow, clearly-labelled purpose: corroborating that the local
GnuCOBOL build reproduces the recorded oracle. If it does not, the demo says so
rather than quietly scoring against a different oracle than the benchmark did.

Held-out vectors are never touched here (CLAUDE.md rule 1, rule R3).

**The demo's own program** (``demo/programs/CUSTUPD.cbl``), which exists to be
refused. It is valid COBOL-85 that GnuCOBOL compiles and runs, using verbs
outside the committed subset. It carries no vectors because it never reaches
validation — the point is that Relian stops before then.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Sequence, Tuple

ROOT = Path(__file__).resolve().parents[1]
CORPUS = ROOT / "bench" / "corpus"
PROGRAMS = Path(__file__).resolve().parent / "programs"


@dataclass(frozen=True)
class SealedExpectation:
    """A recorded oracle result, used only to corroborate the live oracle."""

    stdout: str
    exit_code: int


@dataclass(frozen=True)
class Case:
    case_id: str
    title: str
    source: Path
    main_class: str
    inputs: Tuple[str, ...]
    sealed: Optional[Tuple[SealedExpectation, ...]]
    note: str

    @property
    def has_sealed(self) -> bool:
        return self.sealed is not None


# Human-readable one-liners for the corpus programs. These describe what the
# COBOL does; they make no claim about Relian and are not measurements.
_CORPUS_NOTES = {
    "P01_payroll": "Gross-to-net payroll: ROUNDED half-up, overtime branching, "
                   "graduated withholding, COMP-3 packed decimal.",
    "P02_interest": "Compound interest accrual over a term, with edited output "
                    "pictures.",
    "P03_eligibility": "Benefit eligibility decision logic — nested EVALUATE over "
                       "income, household size and category.",
    "P04_taxtable": "Graduated tax table lookup via OCCURS + SEARCH, including the "
                    "AT END exhaustion path.",
    "P05_validate": "Field validation and error-code assembly with INSPECT and "
                    "88-level condition names.",
    "P06_valinit": "VALUE-clause initialisation across numeric, alphanumeric and "
                   "group items.",
    "P07_exitflow": "Termination flow: RETURN-CODE, GOBACK, EXIT PROGRAM and "
                    "nonzero process exits.",
}

_MAINS = ROOT / "bench" / "harness" / "mains.json"


def _load_public_vectors(program: str) -> List[dict]:
    path = CORPUS / program / "vectors" / "public.jsonl"
    return [json.loads(line) for line in path.read_text().splitlines() if line.strip()]


def corpus_cases(limit: Optional[int] = None) -> List[Case]:
    """One case per committed corpus program, inputs from the PUBLIC split."""
    mains = json.loads(_MAINS.read_text())
    cases: List[Case] = []
    for program in sorted(mains):
        vectors = _load_public_vectors(program)
        if limit is not None:
            vectors = vectors[:limit]
        cases.append(
            Case(
                case_id=program,
                title=program.split("_", 1)[1].replace("-", " ").title(),
                source=CORPUS / program / "program.cbl",
                main_class=mains[program],
                inputs=tuple(v["input"] for v in vectors),
                sealed=tuple(
                    SealedExpectation(stdout=v["expected"],
                                      exit_code=int(v.get("expected_exit", 0)))
                    for v in vectors
                ),
                note=_CORPUS_NOTES.get(program, ""),
            )
        )
    return cases


# Inputs for the refusal case. They exercise the credit path, the open path and
# the high-ratecode fee doubling — enough that a reader can see the program is
# a real one. They are never executed against Java, because no Java is produced.
_REFUSAL_INPUTS = {
    "CUSTUPD": ("1500.00,200.00,12", "1500.00,2000.00,03", "8250.75,125.25,11"),
    "LEDGRPST": ("450.25,004711,06", "1200.00,009002,13", "0.75,000001,01"),
}

_REFUSAL_NOTES = {
    "CUSTUPD": "Valid COBOL-85 that GnuCOBOL compiles and runs, using SUBTRACT, "
               "MULTIPLY, DIVIDE, STRING, GO TO, performed paragraphs and paragraph "
               "EXIT — all outside the committed subset. The transpiler stops at the "
               "first one and names it.",
    "LEDGRPST": "Valid COBOL-85 whose only out-of-subset construct is PERFORM of a "
                "named paragraph. Included because the transpiler does not diagnose "
                "that one — it crashes instead of refusing. Still safe (no output, no "
                "attestation), but a defect rather than designed behavior.",
}


def refusal_cases(limit: Optional[int] = None) -> List[Case]:
    """Real COBOL outside the committed subset.

    ``CUSTUPD`` exercises the designed path: a diagnosed refusal naming the verb
    and line. ``LEDGRPST`` exercises the one construct that is *not* diagnosed —
    it is here so the demo shows that gap instead of hiding it.
    """
    out: List[Case] = []
    for case_id in sorted(_REFUSAL_INPUTS):
        inputs = _REFUSAL_INPUTS[case_id]
        if limit is not None:
            inputs = inputs[:limit]
        out.append(
            Case(
                case_id=case_id,
                title=case_id.title(),
                source=PROGRAMS / f"{case_id}.cbl",
                main_class=case_id.capitalize(),
                inputs=tuple(inputs),
                sealed=None,
                note=_REFUSAL_NOTES[case_id],
            )
        )
    return out


def all_cases(limit: Optional[int] = None) -> List[Case]:
    return corpus_cases(limit) + refusal_cases(limit)


def select(names: Sequence[str], limit: Optional[int] = None) -> List[Case]:
    """Filter cases by id (case-insensitive substring match)."""
    everything = all_cases(limit)
    if not names:
        return everything
    wanted = [n.lower() for n in names]
    return [c for c in everything if any(w in c.case_id.lower() for w in wanted)]
