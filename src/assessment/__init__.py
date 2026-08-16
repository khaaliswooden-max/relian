"""Relian Phase 1 — Legacy Code Assessment engine.

Read-only analysis of a customer codebase. Every number this package emits is
either a measurement wrapped in `models.Measured` (with provenance and a
Trutina grade) or `None`. Nothing here calls a network service or a language
model, and nothing here writes to the codebase under assessment.
"""
