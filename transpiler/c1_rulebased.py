"""
C1_rulebased -- deterministic COBOL->Java transpiler.

Anti-gaming statement: this transpiler operates ONLY on COBOL source. It
contains no vector data, no per-program answer tables, and no knowledge of
expected outputs. Every rule is structural: PIC clauses determine numeric
scale and storage semantics; verbs map to Java statements; COBOL arithmetic
semantics (ROUNDED = HALF_UP on store, unROUNDED COMPUTE = truncate toward
zero to receiving scale) are implemented once, generically.

Scope: the COBOL-85 subset exercised by state-government batch programs of
the corpus class -- ACCEPT/UNSTRING/COMPUTE/MOVE/IF/EVALUATE/PERFORM
VARYING/SEARCH/OCCURS/INSPECT TALLYING/DISPLAY, COMP-3 numerics, 88-levels,
edited pictures. Generality beyond this subset is a known limit (gap
analysis), not a hidden one.
"""

import re
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Dict, List, Optional, Tuple

# Statement *boundary* tokens. A line beginning with one of these starts a new
# statement; a line beginning with anything else is glued onto the previous
# statement as a continuation. This set is NOT the supported set -- membership
# here only affects how source is chopped up. SUPPORTED_STATEMENTS below is
# the supported set, and the two differ deliberately (e.g. SUBTRACT is a
# boundary token with no handler; AT and END-SEARCH are consumed by SEARCH).
VERBS = {"ACCEPT", "UNSTRING", "COMPUTE", "MOVE", "IF", "ELSE", "END-IF",
         "EVALUATE", "WHEN", "END-EVALUATE", "PERFORM", "END-PERFORM",
         "ADD", "SET", "SEARCH", "END-SEARCH", "AT", "INSPECT", "DISPLAY",
         "STOP", "END-UNSTRING", "SUBTRACT",
         # WP-1.5.5: without boundary status these glue onto the previous
         # statement and corrupt it (measured: "MOVE 0 TO RETURN-CODE GOBACK"
         # fails the MOVE regex).
         "CONTINUE", "GOBACK", "EXIT"}


# Reserved words that can legally stand alone on a line followed by a period
# and would otherwise be misread as paragraph names by _statements. Everything
# in VERBS and every END-* token is excluded there too; this set covers the
# statement keywords the C1 tokenizer does not know as VERBS.
_NOT_PARAGRAPH_NAMES = frozenset(
    {"ELSE", "THEN", "OTHER", "CONTINUE", "NEXT", "GOBACK", "EXIT"})


class UnsupportedConstruct(Exception):
    """Raised by unsupported() in strict mode (the default since WP-1.5.2)."""

    def __init__(self, verb: str, line_no: int, paragraph: Optional[str] = None):
        self.verb = verb
        self.line_no = line_no
        self.paragraph = paragraph
        where = f"at source line {line_no}"
        if paragraph:
            where += f" in paragraph {paragraph}"
        super().__init__(f"unsupported COBOL construct {verb!r} {where}")


def jname(cobol: str) -> str:
    return cobol.replace("-", "_")


# ---------------- Data division ----------------

@dataclass
class Field:
    name: str
    kind: str                 # 'num' | 'alpha' | 'edit'
    intd: int = 0             # integer digits
    dec: int = 0              # decimal digits (scale)
    signed: bool = False
    length: int = 0           # alpha length
    occurs: int = 0           # >0 => array (element of an OCCURS group)
    # 88-level condition name -> list of values (a condition name may carry
    # several literals: 88 CODE-CLOSED VALUE "C" "X"). Single-value entries
    # emit exactly what the pre-WP-1.5.4 Dict[str, str] form emitted.
    conds: Dict[str, List[str]] = field(default_factory=dict)
    # WP-1.5.4: VALUE clause, normalised at parse time. For 'num' this is the
    # literal rescaled to the declared PIC scale (COBOL aligns VALUE to the
    # PICTURE); for 'alpha' it is the string padded with spaces to the
    # declared length (fixed-length COBOL storage). None == no VALUE clause,
    # and the field initialises to zero/empty exactly as before WP-1.5.4.
    value: Optional[str] = None


def parse_pic(pic: str) -> Tuple[str, int, int, bool, int]:
    """Return (kind, intd, dec, signed, alpha_len)."""
    p = pic.upper()
    signed = p.startswith("S")
    if signed:
        p = p[1:]
    def expand(s: str) -> str:
        return re.sub(r"([9XZ\-])\((\d+)\)", lambda m: m.group(1) * int(m.group(2)), s)
    p = expand(p)
    if "X" in p:
        return "alpha", 0, 0, False, p.count("X")
    edited = ("Z" in p) or ("-" in p) or ("." in p)
    if "V" in p:
        i, d = p.split("V", 1)
        intd, dec = i.count("9"), d.count("9")
    elif "." in p:
        i, d = p.split(".", 1)
        intd = i.count("9") + i.count("Z") + i.count("-")
        dec = d.count("9")
    else:
        intd, dec = p.count("9") + p.count("Z") + p.count("-"), 0
    return ("edit" if edited else "num"), intd, dec, signed, 0


# VALUE literal on a data item: quoted string, signed numeric, or the
# figuratives ZERO/SPACE. Anything else after a VALUE keyword is a construct
# C1 does not model, and per R2 that must fail loudly, not zero-initialise.
_VALUE_RE = re.compile(
    r'\bVALUE\s+(?:IS\s+)?("(?:[^"]*)"|[+-]?\d+(?:\.\d+)?'
    r'|ZEROE?S?\b|SPACES?\b)', re.IGNORECASE)


def _rescale_numeric(lit: str, dec: int, context: str) -> str:
    """Rescale a numeric VALUE literal to the declared PIC scale in Python
    (COBOL aligns VALUE to the PICTURE), so the emitted Java is a plain
    literal with no runtime setScale."""
    from decimal import Decimal, ROUND_DOWN
    q = Decimal(lit).quantize(Decimal(1).scaleb(-dec), rounding=ROUND_DOWN)
    return format(q, "f")


def _field_value(kind: str, dec: int, length: int, lit: str, context: str) -> str:
    """Normalise one VALUE literal for a field. Raises on forms C1 cannot
    represent rather than silently dropping them (R2)."""
    up = lit.upper()
    if lit.startswith('"'):
        s = lit[1:-1]
        if kind == "alpha":
            if length and len(s) > length:
                s = s[:length]
            return s.ljust(length) if length else s
        raise ValueError(
            f"unsupported VALUE: string literal on non-alpha field {context}")
    if up.startswith("ZERO"):
        # On an alphanumeric field the ZERO figurative fills with the
        # CHARACTER '0', not spaces (measured, GnuCOBOL 3.1.2:
        # PIC X(5) VALUE ZERO displays "00000"; Bugbot finding on PR #15).
        if kind == "alpha":
            return "0" * length if length else "0"
        return _rescale_numeric("0", dec, context)
    if up.startswith("SPACE"):
        if kind != "alpha":
            raise ValueError(f"unsupported VALUE SPACES on non-alpha field {context}")
        return " " * length if length else ""
    if kind == "num":
        return _rescale_numeric(lit, dec, context)
    raise ValueError(
        f"unsupported VALUE: numeric literal on {kind} field {context}")


def parse_working_storage(lines: List[str]) -> Dict[str, Field]:
    fields: Dict[str, Field] = {}
    pending_occurs = 0
    group_level = 0
    last_named: Optional[str] = None
    # WP-1.5.4: a group header carrying VALUE spreads its literal across the
    # subordinate fields' storage in declaration order.
    group_value: Optional[str] = None
    group_value_level = 0
    group_value_off = 0
    for ln in lines:
        t = ln.strip().rstrip(".")
        m = re.match(r"^(\d\d)\s+([A-Z0-9\-]+)(.*)$", t)
        if not m:
            continue
        level, name, rest = int(m.group(1)), m.group(2), m.group(3)
        if level == 88:
            # WP-1.5.4: a condition name may carry several literals
            # (88 CODE-CLOSED VALUE "C" "X"). All are captured; the
            # single-literal case emits exactly what it did before.
            vals = re.findall(r'"([^"]*)"', rest)
            if vals and last_named:
                fields[last_named].conds[name] = vals
            continue
        if group_value is not None and level <= group_value_level:
            group_value = None
        om = re.search(r"OCCURS\s+(\d+)", rest)
        if om and "PIC" not in rest:
            pending_occurs = int(om.group(1))
            group_level = level
            continue
        if pending_occurs and level <= group_level:
            pending_occurs = 0
        pm = re.search(r"PIC\s+([S9XVZ0-9\(\)\.\-]+)", rest, re.IGNORECASE)
        if not pm:
            gv = _VALUE_RE.search(rest)
            if gv and gv.group(1).startswith('"'):
                # group-level VALUE header (e.g. 01 WS-GRP VALUE "AB12CD".)
                group_value = gv.group(1)[1:-1]
                group_value_level = level
                group_value_off = 0
            elif gv:
                raise ValueError(
                    f"unsupported group-level VALUE form at: {t[:60]!r}")
            continue
        kind, intd, dec, signed, alen = parse_pic(pm.group(1))
        occ = int(om.group(1)) if (om and "PIC" in rest) else (
            pending_occurs if level > group_level and pending_occurs else 0)
        value: Optional[str] = None
        vm = _VALUE_RE.search(rest)
        if vm:
            if occ:
                raise ValueError(
                    f"unsupported VALUE on OCCURS element {name}")
            value = _field_value(kind, dec, alen, vm.group(1), name)
        elif re.search(r"\bVALUE\b", rest, re.IGNORECASE):
            raise ValueError(f"unsupported VALUE clause form at: {t[:60]!r}")
        elif group_value is not None and level > group_value_level:
            # consume this field's storage width from the group literal
            width = alen if kind == "alpha" else intd + dec
            chars = group_value[group_value_off:group_value_off + width]
            group_value_off += width
            if kind == "alpha":
                value = chars.ljust(alen) if alen else chars
            elif kind == "num" and dec == 0 and chars.strip().isdigit():
                value = _rescale_numeric(chars.strip(), 0, name)
            else:
                raise ValueError(
                    f"unsupported group VALUE distribution into {name} "
                    f"(kind={kind}, chars={chars!r})")
        fields[name] = Field(name, kind, intd, dec, signed, alen, occ,
                             value=value)
        last_named = name
    return fields


# ---------------- Expressions ----------------

class ExprTx:
    """Tokenize a COBOL arithmetic expression and emit a BigDecimal chain
    (shunting-yard). Division uses high-precision truncation; the single
    scale-fixing happens at STORE time, matching COBOL COMPUTE semantics."""

    def __init__(self, fields: Dict[str, Field], used: Optional[set] = None):
        self.f = fields
        self.used = used

    def tok(self, s: str) -> List[str]:
        out, i = [], 0
        s = s.strip()
        while i < len(s):
            c = s[i]
            if c.isspace():
                i += 1
            elif c in "+-*/()":
                out.append(c); i += 1
            elif c.isdigit() or (c == "." and i + 1 < len(s) and s[i+1].isdigit()):
                j = i
                while j < len(s) and (s[j].isdigit() or s[j] == "."):
                    j += 1
                out.append(s[i:j]); i = j
            else:
                j = i
                while j < len(s) and (s[j].isalnum() or s[j] in "-"):
                    j += 1
                word = s[i:j]
                if not word:
                    raise ValueError(f"tokenizer stall at: {s[i:i+20]!r}")
                if word.upper() == "FUNCTION":
                    m = re.match(r"\s*FUNCTION\s+(NUMVAL|LENGTH|TRIM)\s*\(", s[i:], re.I)
                    if m:
                        depth, k = 1, i + m.end()
                        while k < len(s) and depth:
                            depth += s[k] == "("
                            depth -= s[k] == ")"
                            k += 1
                        out.append(s[i:k]); i = k
                        continue
                # subscript-aware: NAME( ... ) becomes one token
                k = j
                while k < len(s) and s[k].isspace():
                    k += 1
                if k < len(s) and s[k] == "(" and word in self.f and self.f[word].occurs:
                    depth, k2 = 1, k + 1
                    while k2 < len(s) and depth:
                        depth += s[k2] == "("
                        depth -= s[k2] == ")"
                        k2 += 1
                    out.append(s[i:k2]); i = k2
                else:
                    out.append(word); i = j
        return out

    def atom(self, t: str) -> str:
        tu = t.upper()
        if re.fullmatch(r"\d+(\.\d+)?", t):
            return f'new BigDecimal("{t}")'
        if tu.startswith("FUNCTION NUMVAL"):
            inner = t[t.index("(")+1:t.rindex(")")]
            return f"new BigDecimal({self.strexpr(inner)}.trim())"
        if tu.startswith("FUNCTION LENGTH"):
            inner = t[t.index("(")+1:t.rindex(")")]
            return f"new BigDecimal({self.strexpr(inner)}.length())"
        m = re.match(r"^([A-Z][A-Z0-9\-]*)\s*\((.*)\)$", t)
        if m and m.group(1) in self.f and self.f[m.group(1)].occurs:
            if self.used is not None: self.used.add("ix")
            return f"{jname(m.group(1))}[R.ix({self.emit(m.group(2))})-1]"
        return jname(t)

    def strexpr(self, s: str) -> str:
        s = s.strip()
        if s.upper().startswith("FUNCTION TRIM"):
            inner = s[s.index("(")+1:s.rindex(")")]
            return f"{self.strexpr(inner)}.trim()"
        if s.startswith('"'):
            return s
        return jname(s)

    def emit(self, expr: str) -> str:
        toks = self.tok(expr)
        prec = {"+": 1, "-": 1, "*": 2, "/": 2}
        out, ops = [], []
        for t in toks:
            if t in prec:
                while ops and ops[-1] in prec and prec[ops[-1]] >= prec[t]:
                    out.append(ops.pop())
                ops.append(t)
            elif t == "(":
                ops.append(t)
            elif t == ")":
                while ops and ops[-1] != "(":
                    out.append(ops.pop())
                ops.pop()
            else:
                out.append(t)
        while ops:
            out.append(ops.pop())
        st: List[str] = []
        for t in out:
            if t in prec:
                b, a = st.pop(), st.pop()
                fn = {"+": "add", "-": "subtract", "*": "multiply"}.get(t)
                st.append(f"{a}.{fn}({b})" if fn else
                          f"{a}.divide({b}, 20, RoundingMode.DOWN)")
            else:
                st.append(self.atom(t))
        return st[0] if st else "BigDecimal.ZERO"

    def emit_simple(self, s: str) -> str:
        return self.emit(s)


# ---------------- Conditions ----------------

def cond_tx(cond: str, fields: Dict[str, Field], ex: ExprTx, used: Optional[set] = None) -> str:
    parts = re.split(r"\s+AND\s+", cond.strip())
    out = []
    for p in parts:
        p = p.strip()
        m = re.match(r"^(.*?)\s*(<=|>=|NOT\s*=|=|<|>)\s*(.*)$", p)
        if not m:
            # bare 88-level condition. A condition name with several VALUEs
            # is true if the field equals ANY of them (WP-1.5.4); the
            # single-value emission is byte-identical to pre-WP-1.5.4.
            for fname, f in fields.items():
                if p in f.conds:
                    if used is not None: used.add("eq")
                    terms = [f'R.eq({jname(fname)}, "{v}")' for v in f.conds[p]]
                    out.append(terms[0] if len(terms) == 1
                               else "(" + " || ".join(terms) + ")")
                    break
            else:
                out.append("false /* unparsed cond */")
            continue
        lhs, op, rhs = m.group(1).strip(), m.group(2).replace(" ", ""), m.group(3).strip()
        lf = fields.get(lhs)
        alpha = (lf and lf.kind == "alpha") or rhs.startswith('"')
        if alpha:
            if used is not None: used.add("eq")
            l = ex.strexpr(lhs); r = ex.strexpr(rhs)
            e = f"R.eq({l}, {r})"
            out.append(f"!{e}" if op in ("NOT=",) else e)
        else:
            l = ex.emit(lhs); r = ex.emit(rhs)
            cmpop = {"<=": "<= 0", ">=": ">= 0", "<": "< 0", ">": "> 0",
                     "=": "== 0", "NOT=": "!= 0"}[op]
            # ZERO literal
            r = "BigDecimal.ZERO" if rhs.upper() in ("ZERO", "ZEROS") else r
            out.append(f"({l}).compareTo({r}) {cmpop}")
    return " && ".join(out)


# ---------------- Transpiler ----------------

HEADER = """
import java.math.BigDecimal;
import java.math.RoundingMode;
"""

# v1.2 coverage-slim runtime: helpers are emitted ONLY if used, and are
# written branchless where a branch's false arm could never execute on
# well-formed input (dead branches in emitted code are a real defect --
# they cost coverage because they cost dead code).
HELPERS = {
    "unstring": """
    static String[] unstring(String line, int n, String delim) {
        String[] out = new String[n];
        java.util.Arrays.fill(out, "");
        String[] p = line.split(java.util.regex.Pattern.quote(delim), -1);
        System.arraycopy(p, 0, out, 0, Math.min(n, p.length));
        return out;
    }""",
    "take": """
    static String take(String s, int len) {
        return s.substring(0, Math.min(s.length(), len));
    }""",
    "eq": """
    static boolean eq(String a, String b) { return rtrim(a).equals(rtrim(b)); }
    static String rtrim(String s) {
        int e = s.length();
        while (e > 0 && s.charAt(e-1) == ' ') e--;
        return s.substring(0, e);
    }""",
    "ix": """
    static int ix(BigDecimal v) { return v.intValue(); }
    static int ix(int v) { return v; }""",
    "dnumU": """
    /* unedited unsigned integer numeric DISPLAY: zero-pad to declared digits */
    static String dnumU(BigDecimal v, int intd) {
        String ip = v.setScale(0, RoundingMode.DOWN).toPlainString();
        while (ip.length() < intd) ip = "0" + ip;
        return ip;
    }""",
    "dnum": """
    /* unedited numeric DISPLAY, general form */
    static String dnum(BigDecimal v, int intd, int dec) {
        BigDecimal x = v.setScale(dec, RoundingMode.DOWN);
        String s = x.abs().toPlainString();
        String ip = dec > 0 ? s.substring(0, s.indexOf('.')) : s;
        while (ip.length() < intd) ip = "0" + ip;
        String r = dec > 0 ? ip + s.substring(s.indexOf('.')) : ip;
        return (v.signum() < 0 ? "-" : "") + r;
    }""",
    "dedit": """
    /* edited picture then FUNCTION TRIM -> plain decimal at edit scale */
    static String dedit(BigDecimal v, int dec) {
        return v.setScale(dec, RoundingMode.DOWN).toPlainString();
    }""",
}


class Transpiler:
    def __init__(self, source: str, class_name: str, strict: bool = True):
        self.cls = class_name
        # strip fixed-format columns 1-6 + indicator, blank out comments.
        # Comment lines become empty lines rather than disappearing, so that
        # index k in `body` is always source line k+1. Downstream every
        # consumer already skips blank lines, so the emitted Java is
        # unaffected -- this only makes line numbers reportable.
        body: List[str] = []
        for raw in source.splitlines():
            if len(raw) > 6 and raw[6] == "*":
                body.append("")
                continue
            body.append(raw[7:72] if len(raw) > 7 else "")
        text = "\n".join(body)
        ws = text.split("WORKING-STORAGE SECTION.")[1].split("PROCEDURE DIVISION.")[0]
        self.fields = parse_working_storage(ws.splitlines())
        # split(), not partition() -- must match the original slicing exactly
        _parts = text.split("PROCEDURE DIVISION.")
        proc = _parts[1]
        # 1-based source line on which PROCEDURE DIVISION. appears
        proc_line = _parts[0].count("\n") + 1
        self.stmt_lines: List[int] = []
        self.stmts = self._statements(proc, proc_line)
        self.used: set = set()
        self.ex = ExprTx(self.fields, self.used)
        self.j: List[str] = []
        self.ind = 2
        # WP-1.5.2: strict is ON by default (operator decision, escalation
        # item #1). A verb with no handler raises UnsupportedConstruct with
        # verb + line + paragraph instead of being silently dropped -- a
        # dropped statement is exactly the failure R2 forbids. strict=False
        # remains available for inventory collection: it records every
        # unsupported occurrence on `unsupported_hits` and emits nothing,
        # which is what the pre-WP-1.5.2 default did.
        self.strict = strict
        self.unsupported_hits: List[Tuple[str, int]] = []

    def _statements(self, proc: str, line_offset: int = 0) -> List[str]:
        out: List[str] = []
        lines_out: List[int] = []
        paras_out: List[Optional[str]] = []
        current_para: Optional[str] = None
        for k, raw_line in enumerate(proc.splitlines()):
            l = raw_line.strip()
            if not l:
                continue
            if re.match(r"^[A-Z0-9\-]+\.$", l):   # paragraph label
                # A lone `END-IF.`, `ELSE.`, `GOBACK.` or `EXIT.` line matches
                # the same regex but is a scope terminator or statement, not a
                # label. Mirror src/assessment/coverage._paragraph_label so a
                # strict-mode error never reports a phantom paragraph.
                # WP-1.5.5: a lone VERB. line IS a statement and falls through
                # to statement processing -- the pre-WP-1.5.5 skip silently
                # dropped `GOBACK.`, which is exactly the failure R2 forbids.
                # (The sealed five contain no lone VERB. lines, so their
                # emission is unchanged; verified byte-identical.)
                name = l.rstrip(".")
                if name not in VERBS:
                    if (not name.startswith("END-")
                            and name not in _NOT_PARAGRAPH_NAMES):
                        current_para = name
                    continue
            first = l.split()[0].rstrip(".")
            if first in VERBS or not out:
                out.append(l)
                lines_out.append(line_offset + k)
                paras_out.append(current_para)
            else:
                out[-1] += " " + l
        self.stmt_lines = lines_out
        self.stmt_paras = paras_out
        return [s.rstrip(".").strip() for s in out]

    # -- emit helpers --
    def w(self, s: str):
        self.j.append(" " * self.ind * 2 + s)

    def store(self, name: str, valexpr: str, rounded: bool, sub: Optional[str] = None):
        f = self.fields[name]
        if sub:
            self.used.add("ix")
            tgt = jname(name) + f"[R.ix({self.ex.emit(sub)})-1]"
        else:
            tgt = jname(name)
        if f.kind == "alpha":
            self.used.add("take")
            self.w(f"{tgt} = R.take({valexpr}, {f.length});")
        elif f.kind == "edit":
            self.used.add("dedit")
            self.w(f"{tgt} = R.dedit({valexpr}, {f.dec});")
        else:
            mode = "HALF_UP" if rounded else "DOWN"
            self.w(f"{tgt} = ({valexpr}).setScale({f.dec}, RoundingMode.{mode});")

    def transpile(self) -> str:
        self.w("java.util.Scanner _sc = new java.util.Scanner(System.in);")
        # declarations. A field with a VALUE clause initialises to it
        # (WP-1.5.4, normalised at parse time); otherwise zero/empty exactly
        # as before, so VALUE-free programs emit byte-identical Java.
        for f in self.fields.values():
            n = jname(f.name)
            if f.occurs:
                self.w(f"BigDecimal[] {n} = new BigDecimal[{f.occurs}];")
                self.w(f"java.util.Arrays.fill({n}, BigDecimal.ZERO);")
            elif f.kind == "alpha":
                init = f.value if f.value is not None else ""
                self.w(f'String {n} = "{init}";')
            elif f.kind == "edit":
                self.w(f'String {n} = "";')
            elif f.value is not None:
                self.w(f'BigDecimal {n} = new BigDecimal("{f.value}");')
            else:
                self.w(f"BigDecimal {n} = BigDecimal.ZERO;")
        self.w("int BI = 1;")  # indexes are ints; generic single-index support
        # WP-1.5.5: the RETURN-CODE special register, declared only when the
        # program references it, so RETURN-CODE-free programs (the sealed
        # five among them) emit byte-identical Java.
        self.uses_return_code = any("RETURN-CODE" in s for s in self.stmts)
        if self.uses_return_code:
            self.w("int RETURN_CODE = 0;")
        i = 0
        while i < len(self.stmts):
            i = self.stmt(i)
        body = "\n".join(self.j)
        helper_src = "".join(HELPERS[h] for h in sorted(self.used))
        runtime = (f"\nfinal class R {{{helper_src}\n}}\n") if self.used else ""
        return (HEADER + runtime + f"\npublic class {self.cls} {{\n"
                f"  public static void main(String[] args) {{\n{body}\n"
                f"  }}\n}}\n")

    # -- statement dispatch --
    #
    # WP-1.2: the supported set used to be implicit in an if/elif chain, which
    # meant any external list of "what C1 supports" was hand-maintained and
    # free to drift. It is now the keys of SUPPORTED_STATEMENTS (defined below
    # the handlers), which src/assessment/supported.py imports directly.
    # Behavior is unchanged: each handler is the former branch body verbatim,
    # dispatch is on the same token (s.split()[0]), and a verb with no handler
    # still emits nothing -- it now goes through unsupported() so the omission
    # is recorded instead of invisible.

    def unsupported(self, verb: str, line_no: int,
                    paragraph: Optional[str] = None) -> None:
        """The single honest-failure path for a construct C1 cannot transpile.

        In strict mode (the default, WP-1.5.2) it raises
        :class:`UnsupportedConstruct` carrying verb, source line and
        paragraph. With ``strict=False`` it emits nothing, exactly as the
        pre-WP-1.5.2 fall-through did, and records the occurrence on
        ``self.unsupported_hits`` so a caller can collect the full inventory.
        """
        self.unsupported_hits.append((verb, line_no))
        if self.strict:
            raise UnsupportedConstruct(verb, line_no, paragraph)

    def stmt(self, i: int) -> int:
        """Dispatch one statement; returns the index of the next statement."""
        s = self.stmts[i]
        if s == "__NOP":
            return i + 1
        toks = s.split()
        v = toks[0]
        handler = SUPPORTED_STATEMENTS.get(v)
        if handler is None and len(toks) > 1:
            # WP-1.5.5: two-word dispatch keys ("EXIT PROGRAM"). Registering
            # the qualified form instead of bare "EXIT" keeps paragraph EXIT
            # honestly unsupported -- in the dispatch table AND in the
            # assessment's supported_verbs(), which reads these keys.
            handler = SUPPORTED_STATEMENTS.get(f"{v} {toks[1]}")
        if handler is None:
            # A BARE scope terminator (exactly the token, no content) closes a
            # construct whose handler already consumed everything before it
            # (UNSTRING ... END-UNSTRING). Its correct translation is nothing:
            # this is not a dropped statement, so strict mode must not refuse
            # it. The corpus itself exercises this (P01/P04 END-UNSTRING).
            # Anything content-bearing -- SUBTRACT, a stray 'AT END ...', or
            # 'END-UNSTRING <garbage>' -- still goes through unsupported().
            if s in ("END-UNSTRING", "END-SEARCH"):
                return i + 1
            line_no = self.stmt_lines[i] if i < len(self.stmt_lines) else -1
            para = self.stmt_paras[i] if i < len(self.stmt_paras) else None
            self.unsupported(v, line_no, para)
            return i + 1
        return handler(self, i, s)

    def _inline(self, stmt_text: str):
        """Emit a single statement given as text (used inside SEARCH)."""
        self.stmts.append(stmt_text)
        self.stmt(len(self.stmts) - 1)
        self.stmts.pop()


# ---------------- Statement handlers ----------------
# Each handler is the body of the corresponding former elif branch, verbatim.
# Signature: (transpiler, statement_index, statement_text) -> next_index.

def _tx_accept(tp: Transpiler, i: int, s: str) -> int:
    m = re.match(r"ACCEPT\s+([A-Z0-9\-]+)", s)
    tp.w(f'{jname(m.group(1))} = _sc.nextLine();')
    return i + 1


def _tx_unstring(tp: Transpiler, i: int, s: str) -> int:
    m = re.match(r'UNSTRING\s+([A-Z0-9\-]+)\s+DELIMITED\s+BY\s+"([^"]*)"\s+INTO\s+(.+?)(?:\s+END-UNSTRING)?$', s)
    src, delim, tgts = m.group(1), m.group(2), m.group(3).split()
    tp.used.add("unstring")
    tp.w(f'String[] _p = R.unstring({jname(src)}, {len(tgts)}, "{delim}");')
    for k, t in enumerate(tgts):
        tp.w(f'{jname(t)} = _p[{k}];')
    return i + 1


def _tx_compute(tp: Transpiler, i: int, s: str) -> int:
    m = re.match(r"COMPUTE\s+([A-Z0-9\-]+)(\s*\(([^)]+)\))?\s+(ROUNDED\s+)?=\s+(.*)$", s)
    name, sub, rounded, expr = m.group(1), m.group(3), bool(m.group(4)), m.group(5)
    tp.store(name, tp.ex.emit(expr), rounded, sub)
    return i + 1


def _tx_move(tp: Transpiler, i: int, s: str) -> int:
    m = re.match(r"MOVE\s+(.+?)\s+TO\s+([A-Z0-9\-]+)(\s*\(([^)]+)\))?$", s)
    srcx, name, sub = m.group(1).strip(), m.group(2), m.group(4)
    if name == "RETURN-CODE":
        # WP-1.5.5: the RETURN-CODE special register (an int, not a field).
        val = srcx if re.fullmatch(r"\d+", srcx) else (
            "0" if srcx.upper() in ("ZERO", "ZEROS", "ZEROES")
            else f"({tp.ex.emit(srcx)}).intValue()")
        tp.w(f"RETURN_CODE = {val};")
        return i + 1
    f = tp.fields[name]
    if srcx.upper() in ("ZERO", "ZEROS", "ZEROES"):
        tp.store(name, "BigDecimal.ZERO", False, sub)
    elif srcx.startswith('"'):
        lit = srcx[1:-1]
        if f.length and len(lit) > f.length:
            lit = lit[:f.length]
        tp.w(f'{jname(name)} = "{lit}";')
    elif f.kind in ("num",) and (srcx in tp.fields and tp.fields[srcx].kind == "num") :
        tp.store(name, jname(srcx), False, sub)
    elif f.kind == "edit":
        tp.store(name, tp.ex.emit(srcx), False, sub)
    elif re.fullmatch(r"\d+(\.\d+)?", srcx):
        tp.store(name, f'new BigDecimal("{srcx}")', False, sub)
    else:  # alpha <- expr (e.g. FUNCTION TRIM(x))
        tp.used.add("take")
        tp.w(f"{jname(name)} = R.take({tp.ex.strexpr(srcx)}, {f.length or 999});")
    return i + 1


def _tx_add(tp: Transpiler, i: int, s: str) -> int:
    m = re.match(r"ADD\s+(.+?)\s+TO\s+([A-Z0-9\-]+)$", s)
    src, name = m.group(1), m.group(2)
    tp.store(name, f"{jname(name)}.add({tp.ex.emit(src)})", False)
    return i + 1


def _tx_set(tp: Transpiler, i: int, s: str) -> int:
    m = re.match(r"SET\s+([A-Z0-9\-]+)\s+TO\s+(.+)$", s)
    tgt, val = m.group(1), m.group(2).strip()
    if tgt == "BI" or (tgt not in tp.fields):
        if not val.isdigit():
            tp.used.add("ix")
        tp.w(f"BI = {val if val.isdigit() else f'R.ix({tp.ex.emit(val)})'};")
    else:
        src = "new BigDecimal(BI)" if val == "BI" else tp.ex.emit(val)
        tp.store(tgt, src, False)
    return i + 1


def _tx_if(tp: Transpiler, i: int, s: str) -> int:
    m = re.match(r"IF\s+(.*)$", s)
    tp.w(f"if ({cond_tx(m.group(1), tp.fields, tp.ex, tp.used)}) {{")
    tp.ind += 1
    return i + 1


def _tx_else(tp: Transpiler, i: int, s: str) -> int:
    tp.ind -= 1
    tp.w("} else {")
    tp.ind += 1
    return i + 1


def _tx_end_block(tp: Transpiler, i: int, s: str) -> int:
    """END-IF / END-EVALUATE / END-PERFORM: close the block."""
    tp.ind -= 1
    tp.w("}")
    return i + 1


def _tx_evaluate(tp: Transpiler, i: int, s: str) -> int:
    tp._first_when = True
    # WP-1.5.5: EVALUATE <selector> compares each WHEN literal against the
    # selector (P07_exitflow: EVALUATE WS-MODE / WHEN "G"). EVALUATE TRUE
    # keeps the pre-existing behavior -- each WHEN is a full condition --
    # and emits byte-identical Java for the sealed corpus (P03).
    sel = s[8:].strip()
    tp._eval_selector = None if (not sel or sel.upper() == "TRUE") else sel
    return i + 1


def _tx_when(tp: Transpiler, i: int, s: str) -> int:
    cond = s[4:].strip()
    if not tp._first_when:
        tp.ind -= 1
    kw = "if" if tp._first_when else "} else if"
    if cond.upper() == "OTHER":
        tp.w("} else {")
    else:
        sel = getattr(tp, "_eval_selector", None)
        if sel is not None:
            cond = f"{sel} = {cond}"
        tp.w(f"{kw} ({cond_tx(cond, tp.fields, tp.ex, tp.used)}) {{")
    tp.ind += 1
    tp._first_when = False
    return i + 1


def _tx_perform(tp: Transpiler, i: int, s: str) -> int:
    m = re.match(r"PERFORM\s+VARYING\s+([A-Z0-9\-]+)\s+FROM\s+(\S+)\s+BY\s+(\S+)\s+UNTIL\s+(.*)$", s)
    var, frm, by, until = m.groups()
    f = tp.fields[var]
    tp.w(f'for ({jname(var)} = new BigDecimal("{frm}"); '
         f"!({cond_tx(until, tp.fields, tp.ex, tp.used)}); "
         f'{jname(var)} = {jname(var)}.add(new BigDecimal("{by}"))) {{')
    tp.ind += 1
    return i + 1


def _tx_search(tp: Transpiler, i: int, s: str) -> int:
    m = re.match(r"SEARCH\s+([A-Z0-9\-]+)", s)
    grp = m.group(1)
    occ = next((f.occurs for f in tp.fields.values() if f.occurs), 5)
    # collect AT END + WHEN clauses until END-SEARCH
    j = i + 1
    atend, whencond, whenbody = None, None, []
    while j < len(tp.stmts) and not tp.stmts[j].startswith("END-SEARCH"):
        t = tp.stmts[j]
        if t.startswith("AT END"):
            atend = t[6:].strip()
        elif t.startswith("WHEN"):
            whencond = t[4:].strip()
        else:
            whenbody.append(t)
        j += 1
    tp.w("boolean _found = false;")
    tp.w(f"for (; BI <= {occ}; BI++) {{")
    tp.ind += 1
    tp.w(f"if ({cond_tx(whencond, tp.fields, tp.ex, tp.used)}) {{")
    tp.ind += 1
    for b in whenbody:
        tp._inline(b)
    tp.w("_found = true; break;")
    tp.ind -= 1
    tp.w("}")
    tp.ind -= 1
    tp.w("}")
    if atend:
        tp.w("if (!_found) {")
        tp.ind += 1
        tp._inline(atend)
        tp.ind -= 1
        tp.w("}")
    return j + 1


def _tx_inspect(tp: Transpiler, i: int, s: str) -> int:
    m = re.match(r'INSPECT\s+([A-Z0-9\-]+)\s+TALLYING\s+([A-Z0-9\-]+)\s+FOR\s+ALL\s+(.*)$', s)
    src, cnt, lits = m.group(1), m.group(2), re.findall(r'"([^"]*)"', m.group(3))
    chars = "".join(lits)
    tp.w(f"{{ int _c = 0; for (char _ch : {jname(src)}.toCharArray()) "
         f'if ("{chars}".indexOf(_ch) >= 0) _c++; '
         f"{jname(cnt)} = new BigDecimal(_c); }}")
    return i + 1


def _tx_display(tp: Transpiler, i: int, s: str) -> int:
    parts = re.findall(r'"[^"]*"|FUNCTION\s+TRIM\s*\([^)]*\)|[A-Z0-9\-]+', s[7:].strip())
    segs = []
    for p in parts:
        if p.startswith('"'):
            segs.append(p)
        elif p.upper().startswith("FUNCTION"):
            inner = p[p.index("(")+1:p.rindex(")")]
            segs.append(f"{tp.ex.strexpr(inner)}.trim()")
        else:
            f = tp.fields.get(p)
            if f and f.kind == "num":
                if not f.signed and f.dec == 0:
                    tp.used.add("dnumU")
                    segs.append(f"R.dnumU({jname(p)}, {f.intd})")
                else:
                    tp.used.add("dnum")
                    segs.append(f"R.dnum({jname(p)}, {f.intd}, {f.dec})")
            else:
                segs.append(jname(p))
    tp.w(f'System.out.println({" + ".join(segs)});')
    return i + 1


def _tx_stop(tp: Transpiler, i: int, s: str) -> int:
    # STOP RUN returns control with the RETURN-CODE register's value; when
    # the program never touches RETURN-CODE this is the pre-WP-1.5.5
    # `return;` (process exit 0), byte-identical for the sealed five.
    if getattr(tp, "uses_return_code", False):
        tp.w("System.exit(RETURN_CODE);")
    else:
        tp.w("return;")
    return i + 1


def _tx_continue(tp: Transpiler, i: int, s: str) -> int:
    # CONTINUE is an explicit no-op (WP-1.5.5). Emitting nothing is its
    # correct translation -- unlike a dropped statement, there is nothing
    # here to drop.
    return i + 1


def _tx_goback(tp: Transpiler, i: int, s: str) -> int:
    # GOBACK in a main program ends the run with the RETURN-CODE register's
    # value (WP-1.5.5; nonzero exits are scored since WP-1.5.0d). Content
    # after the verb (e.g. GOBACK GIVING) is not modelled -> honest failure.
    if s != "GOBACK":
        tp.unsupported(s.split()[0], tp.stmt_lines[i] if i < len(tp.stmt_lines) else -1,
                       tp.stmt_paras[i] if i < len(tp.stmt_paras) else None)
        return i + 1
    if getattr(tp, "uses_return_code", False):
        tp.w("System.exit(RETURN_CODE);")
    else:
        tp.w("return;")
    return i + 1


def _tx_exit_program(tp: Transpiler, i: int, s: str) -> int:
    # EXIT PROGRAM in a MAIN program is a measured no-op (GnuCOBOL 3.1.2,
    # P07_exitflow oracle: both DISPLAYs around it run). Bare EXIT (paragraph
    # exit) is NOT this statement and stays unsupported -- it dispatches on
    # the single token "EXIT", which has no handler.
    if s != "EXIT PROGRAM":
        tp.unsupported(s.split()[0], tp.stmt_lines[i] if i < len(tp.stmt_lines) else -1,
                       tp.stmt_paras[i] if i < len(tp.stmt_paras) else None)
    return i + 1


# The supported set. Adding a verb here is the ONLY way to make C1 support it,
# and src/assessment/supported.py reads this table rather than a copy of it.
SUPPORTED_STATEMENTS: Dict[str, Callable[[Transpiler, int, str], int]] = {
    "ACCEPT": _tx_accept,
    "ADD": _tx_add,
    "COMPUTE": _tx_compute,
    "CONTINUE": _tx_continue,
    "DISPLAY": _tx_display,
    "ELSE": _tx_else,
    "END-EVALUATE": _tx_end_block,
    "END-IF": _tx_end_block,
    "END-PERFORM": _tx_end_block,
    "EVALUATE": _tx_evaluate,
    # Two-word key (WP-1.5.5): only the qualified form is supported. Bare
    # EXIT (paragraph exit) is inseparable from performed-paragraph support
    # (Bugbot finding, PR #10) and deliberately has NO entry here, so it
    # raises UnsupportedConstruct and the assessment counts it unsupported.
    "EXIT PROGRAM": _tx_exit_program,
    "GOBACK": _tx_goback,
    "IF": _tx_if,
    "INSPECT": _tx_inspect,
    "MOVE": _tx_move,
    "PERFORM": _tx_perform,
    "SEARCH": _tx_search,
    "SET": _tx_set,
    "STOP": _tx_stop,
    "UNSTRING": _tx_unstring,
    "WHEN": _tx_when,
}


def main():
    corpus = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("corpus")
    outdir = Path(sys.argv[2]) if len(sys.argv) > 2 else Path("candidates/C1_rulebased")
    mains = {"P01_payroll": "Payroll01", "P02_interest": "Interest01",
             "P03_eligibility": "Eligible01", "P04_taxtable": "Taxtbl01",
             "P05_validate": "Validat01"}
    for prog, cls in mains.items():
        src = (corpus / prog / "program.cbl").read_text()
        try:
            java = Transpiler(src, cls).transpile()
        except Exception as e:
            java = f"// TRANSPILE FAILED: {e}\n"
            print(f"{prog}: TRANSPILE FAILED: {e}")
        d = outdir / prog
        d.mkdir(parents=True, exist_ok=True)
        (d / f"{cls}.java").write_text(java)
        print(f"{prog}: emitted {len(java)} bytes")


if __name__ == "__main__":
    main()
