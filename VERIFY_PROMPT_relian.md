# Claude Code verification prompt — `relian` repo

Run this in the relian repo root after pushing the tree.

---

You are verifying the RELIAN-BENCH v1.1 integration and the Stage 1-3 honesty
patches in this repository. Operate under CLAUDE.md rules (read it first):
never touch heldout vectors, never assign unmeasured metric values, never
edit bench/. Execute each step and report PASS/FAIL per assertion. Do not
"fix" a failing assertion by changing bench/ or test expectations — report it.

## 0. Toolchain
sudo apt-get update -qq && sudo apt-get install -y gnucobol default-jdk-headless
pip install cryptography --break-system-packages -q
ASSERT: cobc --version prints GnuCOBOL 3.x; javac -version prints 17+.

## 1. Leakage audit (must be zero)
find . -name 'heldout*' | wc -l          # ASSERT: 0
find . -name 'gen_vectors*' | wc -l      # ASSERT: 0
find . -name '*.pem' | wc -l             # ASSERT: 0
grep -rl "20260715" --include='*.py' . | wc -l   # ASSERT: 0 (generator seed)

## 2. Ledger signature
python3 - <<'PY'
import json, sys; sys.path.insert(0,'bench')
from harness.commit import verify
m = json.load(open('bench/LEDGER_relian-bench-v1.1.json'))
assert verify(m), "LEDGER SIGNATURE INVALID — benchmark tampered"
assert m['signature']['key_fingerprint'] == '233bb4406e2de606'
print('PASS ledger:', m['tag'], m['signature']['manifest_sha256'][:16])
PY

## 3. Oracle + public-split scoring (expected exact numbers)
for d in bench/corpus/P*/; do (cd $d && cobc -x program.cbl -o oracle); done
python3 - <<'PY'
import json, sys; sys.path.insert(0,'bench')
from harness.runner import run_candidate
from pathlib import Path
mains = json.loads(Path('bench/harness/mains.json').read_text())
exp = {'B0_null': 0.0, 'C1_rulebased': 1.0}
for name, want in exp.items():
    rep = run_candidate(name, Path(f'bench/candidates/{name}'), mains, split='public')
    assert rep.ber_overall == want, f"{name}: BER {rep.ber_overall} != {want}"
    assert rep.valid, f"{name}: anti-gaming tripped: {rep.invalid_reason}"
    print(f'PASS {name}: public BER={rep.ber_overall} cov={rep.coverage_branch}')
PY

## 4. Honesty gates in the live pipeline
python3 - <<'PY'
import asyncio, sys, types
sys.path.insert(0,'.')
for m in ['openai','anthropic','xgboost','neo4j']:
    sys.modules[m]=types.ModuleType(m)
sys.modules['openai'].AsyncOpenAI=object
sys.modules['anthropic'].AsyncAnthropic=object
from src.core.orchestrator import MigrationOrchestrator, MigrationConfig
async def main():
    o=MigrationOrchestrator()
    r1=await o.migrate(MigrationConfig('bench/corpus/P01_payroll/program.cbl','cobol','java','/tmp/v1'))
    assert r1.semantic_score == 0.0, f"in-corpus must be MEASURED 0.0, got {r1.semantic_score}"
    assert r1.test_coverage is None, "coverage must be None (no tool ran)"
    r2=await o.migrate(MigrationConfig('examples/cobol/banking-system.cbl','cobol','java','/tmp/v2'))
    assert r2.semantic_score is None, "out-of-corpus must be None (unmeasured)"
    assert r2.attestation_tx is None, "attestation MUST refuse unmeasured runs"
    assert any('refusing to attest' in w for w in r2.warnings)
    print('PASS honesty gates: measured 0.0 in-corpus; None + attestation refusal out-of-corpus')
asyncio.run(main())
PY

## 5. Transpiler regeneration is deterministic
python3 transpiler/c1_rulebased.py bench/corpus /tmp/c1_regen
diff -rq /tmp/c1_regen bench/candidates/C1_rulebased --exclude=Cobol.java | grep -v '^Only in bench.*Cobol' ; echo "diff-exit:$?"
ASSERT: no content differences in generated *.java for the five programs.

## 6. Fabrication grep (none of these may exist in src/)
grep -rn "min(80.0" src/ | wc -l                    # ASSERT: 0
grep -rn '"validation_score": 95.0' src/ | wc -l    # ASSERT: 0
grep -rn '"confidence": 0.85' src/ | wc -l          # ASSERT: 0
grep -rn "Connected to Solana" src/ | wc -l         # ASSERT: 0

## Report
Output a table: step | assertion | PASS/FAIL, then an overall verdict.
If ANY step fails, stop and report — do not remediate bench/ or heldout paths.
