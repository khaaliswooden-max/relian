"""WP-2.3 §3.1 — the canonical Data Discovery report.

**Canonical first, human-readable second.** The signed object is
``report.json``: sorted keys, ``separators=(",", ":")``, UTF-8, one trailing
newline, and **no timestamp anywhere**. The single ``generated_at`` this
deliverable carries lives in the manifest (``src.discovery.signing``), which is
where the signature is taken, so two runs of the same version over the same
tree emit byte-identical report bytes rather than two artifacts that differ
only in when they were produced. Byte-identity is asserted on the *emitted
bytes* in ``tests/test_discovery_report.py``, never on the dict — a dict that
serialises correctly under one code path and not another is exactly the defect
that assertion exists to see.

Markdown (:func:`render_markdown`) is **derived and unsigned**, and says so at
the top of its own output: it names ``report.json`` as the signed artifact and
``tools/verify_report.py`` as the way to check it. A rendered document that
disagreed with the signed one would be a support incident, so only one of them
is authoritative and the other admits it.

SECTION ORDER IS CARRIED BY A LIST, NOT BY A DICT
--------------------------------------------------
Canonical JSON sorts keys, so a mapping cannot express "identification before
scope". ``sections`` is therefore an ordered array of objects, each of whose
keys is sorted. The order is fixed by :data:`SECTION_ORDER` and asserted.

The order itself is a decision (D26): the **missing-copybook table leads the
findings**. It is the one result a customer cannot get from their own team
without weeks of work, and it reframes the engagement from "you gave us
incomplete code" to "here is what your migration is missing and who depends on
it". Burying it in an appendix would be a rendering choice that costs the
deliverable its point.

WHAT THIS MODULE MAY NOT SAY (D25 / R11)
-----------------------------------------
Exhibit D's open gaps are not repaired by this work package and must not be
implied by it, including by a well-meaning sentence in a template. There is no
ML risk score (``_score_risk`` returns ``None``/``None`` permanently), no
ledger-anchoring language beyond what the ledger actually is (Ed25519
hash-chained), and no claim about a language pair that has no benchmark and no
transpiler. :data:`FORBIDDEN_VOCABULARY` is linted against the templates *and*
against generated output by :func:`lint_report_text`, with each term carrying
its own negative control in the suite.

Every quantitative field is a :class:`src.assessment.models.Measured` — value,
provenance and Trutina grade — or it is ``null``. A count that was not taken is
absent, never ``0`` (R1). :func:`lint_measured_fields` asserts that on the
built object rather than trusting the builder.
"""

from __future__ import annotations

import hashlib
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Dict, List, Mapping, Optional, Sequence, Tuple

from src.assessment.models import Grade, Measured, canonical_json

from .copybook import Resolution
from .layout import (
    COMPILER_BASIS,
    IBM_EQUIVALENCE_LIMITATION,
    LAYOUT_GRADE,
    VERIFIED_AGAINST,
    Layout,
)

#: The report format this module emits. A verifier that meets a schema it does
#: not know refuses rather than guessing which fields moved.
REPORT_SCHEMA = "relian-discovery-report/1"

#: Sections, in the order they appear in ``sections[]`` and in the rendered
#: Markdown. D26 puts ``missing_copybooks`` ahead of everything except the
#: identification and scope a reader needs to know what they are looking at.
SECTION_ORDER: Tuple[str, ...] = (
    "identification",
    "scope",
    "missing_copybooks",
    "record_layouts",
    "fan_in",
    "limitations_and_grades",
    "verification",
)

#: R11 lint vocabulary. Each term is a capability this product does not have,
#: or a word that would smuggle an unmeasured number past R1:
#:
#: * ``solana`` / ``on-chain`` — the ledger is Ed25519 hash-chained. The
#:   contract's reference is unresolved, and a deliverable is not the place to
#:   resolve it.
#: * ``ml risk`` / ``machine learning`` — not built. The ML limb was deleted
#:   under R1 (docs/R1_ML_DISPOSITION_2026-08.md) and nothing replaced it.
#: * ``predicted`` / ``estimated`` — both name a number that was not measured.
#:   R1 says such a number is ``None``, and a word that dresses it as an
#:   output is the failure mode this project keeps deleting.
#:
#: Matched case-insensitively against templates AND against generated output,
#: because a term that only ever reaches the page through an f-string would
#: survive a template-only lint.
FORBIDDEN_VOCABULARY: Tuple[str, ...] = (
    "solana",
    "on-chain",
    "ml risk",
    "machine learning",
    "predicted",
    "estimated",
)

#: Grade for a number this run counted directly off the customer's tree.
#: Resolution is dialect-independent — it reads ``COPY`` directives, not
#: storage — so unlike a layout there is no compiler whose behaviour the count
#: is contingent on. VERIFIED here means "this run counted it", and the
#: provenance says which command did.
COUNT_GRADE: Grade = "VERIFIED"

#: Named once so the report, the Markdown banner and the verifier's own help
#: text cannot drift apart.
VERIFIER_PATH = "tools/verify_report.py"
SIGNED_ARTIFACT = "report.json"
MARKDOWN_ARTIFACT = "report.md"
MANIFEST_ARTIFACT = "report.manifest.json"
COUNTERSIG_ARTIFACT = "report.countersig.json"

#: The sentence that keeps the instance layer from being read as more than it
#: is (escalation trigger 4). Visionblox never holds the instance key, so the
#: layer establishes integrity and provenance-of-tool — this report came out of
#: this Relian installation over inputs with these digests and has not changed
#: since — and NOT identity of signer. Saying so in the artifact is the whole
#: reason the artifact may carry a signature at all.
INSTANCE_LAYER_CLAIM = (
    "The instance signature proves integrity and provenance-of-tool: this "
    "report was produced by this Relian installation over inputs with the "
    "digests recorded in the manifest, and has not changed since. It does NOT "
    "prove identity of signer. The instance key is generated on this machine "
    "and never leaves it; Visionblox has never held it and cannot vouch for "
    "it. A report carrying only this layer is VALID AND UNATTESTED. Only the "
    "Visionblox countersignature attests to the report, and it is a separate, "
    "detached file added by the operator on delivery."
)

#: How the instance key comes to exist, stated in the artifact rather than in a
#: commit message. WP-2.1 found ``bench/harness/commit.py`` generating a
#: signing key silently and called it a defect, because it produced a manifest
#: under a fingerprint nobody had ever seen. Generation here is the intended
#: behaviour and this is the disclosure that makes it acceptable — if this
#: sentence is not in the artifact, the behaviour is a defect again.
INSTANCE_KEY_DISCLOSURE = (
    "This installation's Ed25519 instance key was generated by Relian on "
    "first use, at ~/.relian/instance-ed25519.pem with mode 0600, and is "
    "recorded here rather than left implicit. If the key cannot be created or "
    "loaded, Relian emits no report at all: an unsigned artifact that looks "
    "signed is worse than no artifact."
)

#: R12, stated where the customer reads it rather than only in the design note.
PERIMETER_DISCLOSURE = (
    "Customer source never leaves this machine. The report and its manifest "
    "are produced here; the manifest records SHA-256 digests of the inputs, "
    "never their content. The only value that crosses the perimeter to obtain "
    "a Visionblox countersignature is the 64-character manifest hash, together "
    "with the report id and the instance key fingerprint — all three of which "
    "are hexadecimal digests, not text drawn from your code."
)


class ReportError(RuntimeError):
    """The report cannot be built honestly, so it is not built."""


# --------------------------------------------------------------------------
# Inputs
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class ReportMeta:
    """Engagement identification. Free text supplied by the operator.

    None of these fields reaches the countersignature request — they are
    customer identifiers, and ``tests/test_discovery_report.py`` seeds them
    with distinctive strings and asserts none appears in the request line.
    """

    customer: str
    engagement: str
    contract_vehicle: str
    relian_version: str
    root_label: str

    def to_dict(self) -> Dict[str, Any]:
        return {
            "customer": self.customer,
            "engagement": self.engagement,
            "contract_vehicle": self.contract_vehicle,
            "relian_version": self.relian_version,
            "root_label": self.root_label,
        }


#: Modules whose bytes produced this report. Recorded so a reader can tell
#: which build of the engine emitted the numbers, and so a re-run that
#: disagrees can be attributed to a code change rather than to the tree.
TOOL_MODULES: Tuple[str, ...] = (
    "src/assessment/models.py",
    "src/discovery/__init__.py",
    "src/discovery/cli.py",
    "src/discovery/copybook.py",
    "src/discovery/layout.py",
    "src/discovery/report.py",
    "src/discovery/signing.py",
)


def tool_digests(repo_root: Path) -> Tuple[Dict[str, str], ...]:
    """``[{path, sha256}, …]`` over :data:`TOOL_MODULES`, sorted by posix path.

    A module that is missing is recorded as missing rather than skipped. An
    absent tool digest is a fact about the installation; silently shortening
    the list would make two different installations look identical.
    """
    out: List[Dict[str, str]] = []
    for rel in sorted(TOOL_MODULES):
        path = repo_root / rel
        if path.is_file():
            out.append(
                {"path": rel, "sha256": hashlib.sha256(path.read_bytes()).hexdigest()}
            )
        else:
            out.append({"path": rel, "sha256": "", "absent": "not present in this installation"})
    return tuple(out)


# --------------------------------------------------------------------------
# The report
# --------------------------------------------------------------------------


@dataclass(frozen=True)
class Report:
    """A canonical Data Discovery report. ``sections`` is ORDERED."""

    schema: str
    report_id: str
    sections: Tuple[Dict[str, Any], ...]

    # -- construction ------------------------------------------------------

    @staticmethod
    def build(
        resolution: Resolution,
        layouts: Sequence[Layout],
        meta: ReportMeta,
        *,
        repo_root: Path,
        instance_fingerprint: str,
        instance_public_key_hex: str,
        layouts_skipped_reason: Optional[str] = None,
    ) -> "Report":
        """Assemble the report from measurements taken in THIS run.

        ``layouts_skipped_reason`` is required whenever ``layouts`` is empty
        and the tree contains resolvable copybooks. R2: a section that is empty
        because a decision was taken says which decision, and an empty section
        with no reason is refused rather than rendered as a clean result.
        """
        resolvable = [
            n for n in resolution.referenced_names() if n in resolution.records
        ]
        if not layouts and resolvable and not layouts_skipped_reason:
            raise ReportError(
                "record_layouts would be empty over a tree with "
                f"{len(resolvable)} resolvable copybook(s) and no reason given. "
                "An empty section that does not say why it is empty reads as a "
                "clean result (R2). Pass layouts, or pass a reason."
            )

        sections: List[Dict[str, Any]] = [
            _identification_section(
                meta,
                repo_root=repo_root,
                instance_fingerprint=instance_fingerprint,
                instance_public_key_hex=instance_public_key_hex,
            ),
            _scope_section(resolution, meta),
            _missing_copybooks_section(resolution),
            _record_layouts_section(layouts, layouts_skipped_reason),
            _fan_in_section(resolution),
            _limitations_section(layouts),
            _verification_section(),
        ]
        ids = tuple(s["id"] for s in sections)
        if ids != SECTION_ORDER:
            raise ReportError(f"section order drifted: {ids} != {SECTION_ORDER}")

        # report_id is CONTENT-DERIVED, never random and never time-derived:
        # two runs over the same tree must produce the same id, and an id that
        # carried entropy would break the byte-identity this whole design rests
        # on. It is a digest, so it also cannot carry a customer string into
        # the countersignature request line.
        body = {"schema": REPORT_SCHEMA, "sections": sections}
        report_id = hashlib.sha256(
            canonical_json(body).encode("utf-8")
        ).hexdigest()[:32]
        return Report(schema=REPORT_SCHEMA, report_id=report_id, sections=tuple(sections))

    # -- serialisation -----------------------------------------------------

    def to_dict(self) -> Dict[str, Any]:
        return {
            "schema": self.schema,
            "report_id": self.report_id,
            "sections": list(self.sections),
        }

    def to_canonical_json(self) -> bytes:
        """The signed bytes. Sorted keys, no whitespace, UTF-8, one newline.

        Returned as BYTES rather than str because bytes are what gets hashed
        and what gets written, and a caller that re-encodes with a different
        setting is a caller that produces a different digest for the same
        report.
        """
        return canonical_json(self.to_dict()).encode("utf-8") + b"\n"

    def section(self, name: str) -> Dict[str, Any]:
        for s in self.sections:
            if s["id"] == name:
                return s
        raise KeyError(name)


# --------------------------------------------------------------------------
# Sections
# --------------------------------------------------------------------------


def _identification_section(
    meta: ReportMeta,
    *,
    repo_root: Path,
    instance_fingerprint: str,
    instance_public_key_hex: str,
) -> Dict[str, Any]:
    return {
        "id": "identification",
        "title": "Identification",
        "engagement": meta.to_dict(),
        "tool_digests": [dict(d) for d in tool_digests(repo_root)],
        "instance_key": {
            "fingerprint": instance_fingerprint,
            "public_key_hex": instance_public_key_hex,
            "algorithm": "Ed25519",
            "disclosure": INSTANCE_KEY_DISCLOSURE,
        },
        "attestation_model": {
            "instance_layer": INSTANCE_LAYER_CLAIM,
            "countersignature_layer": (
                "A Visionblox countersignature, when present, is a detached "
                f"{COUNTERSIG_ARTIFACT} produced by the Visionblox release key "
                "and verified against a fingerprint you pin yourself. It "
                "attests that Visionblox stands behind this report. It is a "
                "separate claim class from the key that seals Relian's "
                "benchmarks, and the two keys are deliberately different."
            ),
            "perimeter": PERIMETER_DISCLOSURE,
        },
        # The WP-2.2 limitation travels into the TOP MATTER, not only onto
        # individual records. A limitation a reader scrolls past is not
        # disclosed.
        "top_matter_limitation": IBM_EQUIVALENCE_LIMITATION,
    }


def _count(value: int, what: str, root_label: str) -> Dict[str, Any]:
    """A count taken in this run, wrapped with its provenance and grade.

    Every number in the scope section goes through here. There is no path that
    puts a bare integer in the report, which is what makes ⑦ checkable rather
    than a matter of reviewer attention.
    """
    return Measured(
        value,
        f"counted by src.discovery.copybook.resolve over {root_label} in this "
        f"run ({what})",
        COUNT_GRADE,
    ).to_dict()


def _scope_section(resolution: Resolution, meta: ReportMeta) -> Dict[str, Any]:
    label = meta.root_label
    referenced = resolution.referenced_names()
    resolvable = [n for n in referenced if n in resolution.records]
    replacing_sites = [d for d in resolution.directives if d.replacing]
    fan_out = resolution.fan_out()
    top = max(((c, n) for n, c in fan_out.items()), default=None)
    return {
        "id": "scope",
        "title": "Scope",
        "root_label": label,
        "search_paths": list(resolution.search_paths),
        "counts": {
            "source_files_scanned": _count(
                resolution.files_scanned, "source files walked", label
            ),
            "files_with_at_least_one_copy": _count(
                len({d.referrer for d in resolution.directives}),
                "distinct referrers", label,
            ),
            "copy_directive_sites": _count(
                len(resolution.directives), "COPY directive sites", label
            ),
            "distinct_names_referenced": _count(
                len(referenced), "distinct copybook names referenced", label
            ),
            "resolvable": _count(
                len(resolvable), "names resolved to a member in the tree", label
            ),
            "unresolvable": _count(
                len(resolution.unresolved), "names no search path could satisfy", label
            ),
            "edges": _count(
                len(resolution.edges), "distinct (referrer, copybook) pairs", label
            ),
            "copy_replacing_sites": _count(
                len(replacing_sites), "COPY ... REPLACING sites", label
            ),
            "cycles": _count(len(resolution.cycles), "COPY cycles detected", label),
            "members_present_but_unreferenced": _count(
                len(resolution.unreferenced),
                "members on disk that nothing COPYs", label,
            ),
        },
        # None, not 0, when there is no edge at all: "the maximum fan-out is
        # zero" and "nothing references anything" are different sentences and
        # only one of them is true here (R1/D19).
        "max_fan_out": (
            {
                "referrer": top[1],
                "distinct_copybooks": _count(
                    top[0], "distinct copybook names referenced by one file", label
                ),
            }
            if top is not None
            else None
        ),
        "note": (
            "Copybook resolution reads COPY directives, not storage, so it is "
            "independent of the COBOL dialect and no compiler-dialect "
            "limitation applies to the counts above. Record layouts are a "
            "different claim and carry their own limitation."
        ),
    }


def _missing_copybooks_section(resolution: Resolution) -> Dict[str, Any]:
    """D26 — the finding, placed where a finding belongs."""
    rows = [
        {
            "name": u.name,
            "referenced_by_count": len(u.referenced_by),
            "referenced_by": list(u.referenced_by),
            "copy_sites": u.reference_count,
            "paths_searched_count": len(u.paths_searched),
            "reason": u.reason,
        }
        for u in sorted(
            resolution.unresolved, key=lambda u: (-len(u.referenced_by), u.name)
        )
    ]
    return {
        "id": "missing_copybooks",
        "title": "Missing copybooks",
        "lead": (
            "Every COPY target below was referenced by the code you supplied "
            "and could not be found under any directory this run searched. "
            "Each row carries the full referrer list and the number of "
            "directories searched, because \"missing\" without \"we looked "
            "here\" is an accusation rather than a finding. A copybook that "
            "resolves at your site and not at ours is the normal condition of "
            "real mainframe code; it is not a defect in your submission, and "
            "it is the part of your migration that has to be sourced before a "
            "record layout can be computed for the members that reference it."
        ),
        "rows": rows,
        "count": Measured(
            len(rows),
            "counted from src.discovery.copybook.resolve unresolved set in this run",
            COUNT_GRADE,
        ).to_dict(),
        # Deliberately NOT a "likely source" guess per row. Naming CICS or IBM
        # MQ for a member this engine never saw would be an inference wearing
        # the costume of a finding (R1). The referrer list is measured; the
        # provenance of an absent member is not.
        "note": (
            "This report does not guess where a missing member comes from. "
            "The referrer list and the search-path count are measured; the "
            "origin of a member this run never saw is not, and an inference "
            "printed beside measurements would read as one."
        ),
    }


def _record_layouts_section(
    layouts: Sequence[Layout], skipped_reason: Optional[str]
) -> Dict[str, Any]:
    if not layouts:
        return {
            "id": "record_layouts",
            "title": "Record layouts",
            "included": False,
            "reason_not_included": skipped_reason or "no resolvable copybook in scope",
            "records": [],
            "count": Measured(
                0, "no layout was computed in this run; see reason_not_included",
                COUNT_GRADE,
            ).to_dict(),
        }
    return {
        "id": "record_layouts",
        "title": "Record layouts",
        "included": True,
        "reason_not_included": None,
        "records": [layout.to_dict() for layout in sorted(layouts, key=lambda l: l.group)],
        "count": Measured(
            len(layouts),
            "records computed by src.discovery.layout in this run",
            COUNT_GRADE,
        ).to_dict(),
    }


def _fan_in_section(resolution: Resolution) -> Dict[str, Any]:
    fan_in = resolution.fan_in()
    rows = sorted(
        ({"name": n, "referenced_by_count": c} for n, c in fan_in.items()),
        key=lambda r: (-r["referenced_by_count"], r["name"]),
    )
    return {
        "id": "fan_in",
        "title": "Copybook fan-in",
        "rows": rows,
        "edges": [list(e) for e in resolution.edges],
        "cycles": [list(c) for c in resolution.cycles],
        "unreferenced_members": list(resolution.unreferenced),
        "count": Measured(
            len(rows), "distinct copybook names with at least one referrer, "
                       "counted in this run", COUNT_GRADE,
        ).to_dict(),
        "note": (
            "Fan-in is the number of DISTINCT files that reference a member. "
            "A file that COPYs the same member twice is two directive sites "
            "and one edge; conflating them would make two different counts "
            "look like the same measurement."
        ),
    }


def _limitations_section(layouts: Sequence[Layout]) -> Dict[str, Any]:
    limitations: List[str] = [IBM_EQUIVALENCE_LIMITATION]
    for layout in sorted(layouts, key=lambda l: l.group):
        for reason in layout.reasons:
            entry = f"{layout.group}: {reason}"
            if entry not in limitations:
                limitations.append(entry)
    return {
        "id": "limitations_and_grades",
        "title": "Limitations and grades",
        "limitations": limitations,
        "grades": {
            "counts": {
                "grade": COUNT_GRADE,
                "basis": (
                    "counted directly off the supplied tree by this run; "
                    "resolution is dialect-independent"
                ),
            },
            "record_layouts": {
                "grade": LAYOUT_GRADE,
                "basis": (
                    f"statically computed and verified against {COMPILER_BASIS} "
                    f"on {VERIFIED_AGAINST}; IBM Enterprise COBOL equivalence "
                    f"is UNMEASURED"
                ),
            },
        },
        "not_claimed": [
            "No risk score of any kind is computed. Relian has no scoring "
            "model, and a number produced without one would be a number "
            "without a measurement behind it.",
            "No file inventory, data lineage, or target-schema DDL. Those are "
            "later work and are absent here rather than approximated.",
            "No claim about any language pair other than COBOL. There is no "
            "benchmark and no transpiler for one, so there is nothing to "
            "claim.",
            "No claim of equivalence with IBM Enterprise COBOL. Everything the "
            f"layout engine is verified against is {COMPILER_BASIS}.",
        ],
    }


def _verification_section() -> Dict[str, Any]:
    return {
        "id": "verification",
        "title": "How to verify this report",
        "signed_artifact": SIGNED_ARTIFACT,
        "manifest": MANIFEST_ARTIFACT,
        "countersignature": COUNTERSIG_ARTIFACT,
        "verifier": VERIFIER_PATH,
        "derived_unsigned_artifacts": [MARKDOWN_ARTIFACT],
        "layers": [
            {
                "layer": "FILES",
                "checks": "every path recorded in the manifest exists and "
                          "hashes to its recorded SHA-256",
            },
            {
                "layer": "MANIFEST",
                "checks": "the manifest hash, recomputed from the manifest "
                          "minus its signature block, equals the value the "
                          "signature was taken over",
            },
            {
                "layer": "INSTANCE",
                "checks": "the Ed25519 instance signature verifies over that "
                          "manifest hash, under the fingerprint the manifest "
                          "records",
            },
            {
                "layer": "COUNTERSIGNATURE",
                "checks": "the detached Visionblox countersignature verifies "
                          "over the same manifest hash, under the release-key "
                          "fingerprint YOU pin on the command line. The pin is "
                          "required: an unpinned check would accept a report "
                          "re-signed under any key at all.",
            },
        ],
        "instructions": (
            f"Run: python3 {VERIFIER_PATH} --report-dir <this directory> "
            f"--pin-fingerprint <the Visionblox release key fingerprint from "
            f"the Technical Delivery Sheet>. The verifier is standalone: "
            f"standard library plus `cryptography`, no network, no "
            f"credentials, and no import from the Relian source tree. It "
            f"prints its own SHA-256 so you can confirm it is the tool that "
            f"was shipped to you. Exit 0 means all four layers passed. Exit 3 "
            f"means the report is VALID AND UNATTESTED — the instance layer "
            f"passed and no Visionblox countersignature is present. Any other "
            f"non-zero exit means a named layer failed."
        ),
        "instance_layer_claim": INSTANCE_LAYER_CLAIM,
    }


# --------------------------------------------------------------------------
# Lints (D25 / R1)
# --------------------------------------------------------------------------


def lint_report_text(text: str) -> List[Tuple[str, str]]:
    """``[(term, excerpt), …]`` for every forbidden term in ``text``.

    Case-insensitive, and reported with a surrounding excerpt so a failure
    tells the author which sentence to fix rather than only that one exists.
    """
    found: List[Tuple[str, str]] = []
    lowered = text.lower()
    for term in FORBIDDEN_VOCABULARY:
        start = 0
        while True:
            at = lowered.find(term, start)
            if at < 0:
                break
            found.append((term, text[max(0, at - 60): at + len(term) + 60]))
            start = at + len(term)
    return found


#: Every template string this module can put on a page. Linted directly, so a
#: forbidden term cannot enter through a constant that no fixture happens to
#: exercise.
TEMPLATES: Dict[str, str] = {
    "instance_layer_claim": INSTANCE_LAYER_CLAIM,
    "instance_key_disclosure": INSTANCE_KEY_DISCLOSURE,
    "perimeter_disclosure": PERIMETER_DISCLOSURE,
    "ibm_equivalence_limitation": IBM_EQUIVALENCE_LIMITATION,
}


_MEASURED_KEYS = frozenset({"value", "provenance", "grade"})


def lint_measured_fields(obj: Any, path: str = "$") -> List[str]:
    """Every bare number that reached the report without provenance (R1/⑦).

    Walks the built object and reports any integer or float that is not the
    ``value`` of a :class:`Measured` dict and not on the small allowlist of
    structural numbers below. The allowlist exists because a layout row's
    ``offset`` and ``length`` are already covered by the layout's own grade and
    limitation block, and wrapping each of several thousand of them would make
    the artifact unreadable without making it more honest.
    """
    #: Structural coordinates, not measurements. A layout row's ``offset``,
    #: ``length`` and OCCURS ``subscripts`` are already covered by the
    #: layout's own grade and limitation block, and wrapping each of several
    #: thousand of them would make the artifact unreadable without making it
    #: more honest.
    #:
    #: ``subscripts`` is on this list because the lint CAUGHT it: the first
    #: run of this gate over the sealed 12-axis corpus refused to write a
    #: report, naming every OCCURS index vector as an ungraded number. The
    #: demo tree has no OCCURS, so a narrower fixture would have shipped the
    #: refusal to the first customer with a table in a copybook.
    #: ``test_the_r1_gate_is_clean_over_the_sealed_twelve_axis_corpus`` is
    #: that regression, and it runs over every construct the bench covers
    #: rather than over the three copybooks that happen to be convenient.
    structural = {
        "level", "offset", "length", "occurs", "occurs_min", "odo_value",
        "subscripts",
        "referenced_by_count", "copy_sites", "paths_searched_count",
        "group_length", "distinct_copybooks",
    }
    findings: List[str] = []

    def walk(node: Any, at: str, parent_key: Optional[str]) -> None:
        if isinstance(node, dict):
            if _MEASURED_KEYS <= set(node.keys()):
                if node.get("value") is None:
                    findings.append(f"{at}: Measured with a null value (R1)")
                if not node.get("provenance"):
                    findings.append(f"{at}: Measured with no provenance (R9)")
                if node.get("grade") not in ("VERIFIED", "PLAUSIBLE", "SPECULATIVE"):
                    findings.append(f"{at}: Measured with grade {node.get('grade')!r}")
                return
            for key in sorted(node):
                walk(node[key], f"{at}.{key}", key)
        elif isinstance(node, list):
            for i, item in enumerate(node):
                walk(item, f"{at}[{i}]", parent_key)
        elif isinstance(node, bool):
            return
        elif isinstance(node, (int, float)):
            if parent_key not in structural:
                findings.append(
                    f"{at}: bare number {node!r} with no grade or provenance (R1/R9)"
                )

    walk(obj, path, None)
    return findings


# --------------------------------------------------------------------------
# Markdown — derived, unsigned, and it says so
# --------------------------------------------------------------------------

_BANNER = (
    "> **This document is derived and unsigned.** The signed artifact is "
    "`{signed}`; this file is a rendering of it. If the two ever disagree, "
    "`{signed}` is authoritative and this rendering is the defect. Verify with "
    "`python3 {verifier} --report-dir <directory> --pin-fingerprint <release "
    "key fingerprint>`.\n"
)


def _md_table(headers: Sequence[str], rows: Sequence[Sequence[Any]]) -> str:
    out = ["| " + " | ".join(str(h) for h in headers) + " |",
           "| " + " | ".join("---" for _ in headers) + " |"]
    out.extend("| " + " | ".join(str(c) for c in r) + " |" for r in rows)
    return "\n".join(out) + "\n"


def _m(cell: Optional[Mapping[str, Any]]) -> str:
    """Render a Measured dict as ``value`` or an em dash. Never ``0``."""
    if not cell:
        return "—"
    return str(cell.get("value"))


def render_markdown(report: Report) -> str:
    """A reading copy. Derived from the signed object, never the other way."""
    ident = report.section("identification")
    scope = report.section("scope")
    missing = report.section("missing_copybooks")
    layouts = report.section("record_layouts")
    fan_in = report.section("fan_in")
    limits = report.section("limitations_and_grades")
    verify = report.section("verification")
    eng = ident["engagement"]

    parts: List[str] = []
    parts.append(f"# Relian Data Discovery report — {eng['customer']}\n")
    parts.append(_BANNER.format(signed=SIGNED_ARTIFACT, verifier=VERIFIER_PATH))
    parts.append(f"\n**Report id:** `{report.report_id}`\n")

    parts.append("\n## 1. Identification\n\n")
    parts.append(_md_table(
        ["Field", "Value"],
        [
            ["Customer", eng["customer"]],
            ["Engagement", eng["engagement"]],
            ["Contract vehicle", eng["contract_vehicle"]],
            ["Relian version", eng["relian_version"]],
            ["Scanned tree", eng["root_label"]],
            ["Instance key fingerprint", f"`{ident['instance_key']['fingerprint']}`"],
        ],
    ))
    parts.append(f"\n{ident['instance_key']['disclosure']}\n")
    parts.append(f"\n{ident['attestation_model']['instance_layer']}\n")
    parts.append(f"\n{ident['attestation_model']['perimeter']}\n")
    parts.append(f"\n**Limitation, stated up front:** {ident['top_matter_limitation']}\n")

    parts.append("\n## 2. Scope\n\n")
    parts.append(f"**Grade:** {COUNT_GRADE} · **Provenance:** every row below was "
                 f"counted by this run over `{scope['root_label']}`.\n\n")
    parts.append(_md_table(
        ["Measure", "Value"],
        [[k.replace("_", " "), _m(v)] for k, v in sorted(scope["counts"].items())],
    ))
    parts.append(f"\n{scope['note']}\n")

    parts.append("\n## 3. Missing copybooks\n\n")
    parts.append(f"{missing['lead']}\n\n")
    if missing["rows"]:
        parts.append(_md_table(
            ["Missing copybook", "Referenced by (files)", "COPY sites", "Directories searched"],
            [[f"`{r['name']}`", r["referenced_by_count"], r["copy_sites"],
              r["paths_searched_count"]] for r in missing["rows"]],
        ))
    else:
        parts.append("No unresolvable `COPY` target was found in this tree. "
                     "Every referenced member resolved.\n")
    parts.append(f"\n{missing['note']}\n")

    parts.append("\n## 4. Record layouts\n\n")
    if not layouts["included"]:
        parts.append(f"Not included in this report. {layouts['reason_not_included']}\n")
    else:
        parts.append(f"**Grade:** {LAYOUT_GRADE} · **Provenance:** statically "
                     f"computed by this run; see section 6 for the basis and "
                     f"its limits.\n\n")
        parts.append(_md_table(
            ["Record", "Status", "Length (bytes)", "Field rows", "Gap bytes"],
            [[f"`{r['group']}`", r["status"],
              _m(r["summary"].get("group_length")),
              _m(r["summary"].get("field_rows")),
              _m(r["summary"].get("gap_bytes"))] for r in layouts["records"]],
        ))

    parts.append("\n## 5. Copybook fan-in\n\n")
    if fan_in["rows"]:
        parts.append(_md_table(
            ["Copybook", "Referenced by (distinct files)"],
            [[f"`{r['name']}`", r["referenced_by_count"]] for r in fan_in["rows"]],
        ))
    else:
        parts.append("No `COPY` edge was found in this tree.\n")
    parts.append(f"\n{fan_in['note']}\n")

    parts.append("\n## 6. Limitations and grades\n\n")
    for entry in limits["limitations"]:
        parts.append(f"* {entry}\n")
    parts.append("\n**What this report does not claim**\n\n")
    for entry in limits["not_claimed"]:
        parts.append(f"* {entry}\n")

    parts.append("\n## 7. How to verify this report\n\n")
    parts.append(f"{verify['instructions']}\n\n")
    parts.append(_md_table(
        ["Layer", "What it checks"],
        [[r["layer"], r["checks"]] for r in verify["layers"]],
    ))
    parts.append(f"\n{verify['instance_layer_claim']}\n")
    return "".join(parts)
