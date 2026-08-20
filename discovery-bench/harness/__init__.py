"""RELIAN-DISCOVERY-BENCH v0.1 harness.

Two modules, run in a fixed order that is part of the contract:

    ``oracle_layouts``  Route A -- ``cobc -x -t sym.lst -ftsymbols`` over a
                        driver that does nothing but COPY the copybook. Yields
                        per-field SIZE, the declaration structure, and the
                        GROUP TOTAL. Route B cannot start without that total,
                        because the total is what sizes the probe's byte
                        window.
    ``gen_probe``       Route B -- emits a self-contained COBOL program that
                        measures each field's offset and length by writing
                        HIGH-VALUES into it and scanning the group's bytes.

Nothing here imports from ``src/``. The benchmark's answers must exist before
the engine that is graded against them does (R7).
"""
