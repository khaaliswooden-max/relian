# Relian layout verification kit

**For your mainframe team. Ten minutes. No data is read.**

Relian computes COBOL record layouts — every field's byte offset and length —
without invoking a compiler. Those layouts are verified byte-for-byte against
**GnuCOBOL 3.1.2.0** (186 of 186 comparisons, tolerance zero, against a
benchmark sealed before the engine was written).

You run **IBM Enterprise COBOL**. We do not have an IBM system, and we will not
pretend our GnuCOBOL result transfers to yours. This kit lets you settle it on
your own hardware.

## What the program does, and what it cannot do

`IBMLAYOUT.cbl` **declares** a copybook and **reports storage**. It:

* reads no data — no file is opened, no dataset is read, no VSAM cluster is touched;
* connects to nothing;
* writes one JSON document to `SYSOUT`.

The only thing that can leave your perimeter is a description of **your own
copybook** — field names, offsets and lengths — and only if you choose to send
it back. Nothing else is collected and there is nothing to configure.

## How it works

For each field: clear the record to `LOW-VALUE`, write `HIGH-VALUE` into that
one field, then scan the record and report the first `FF`, the last `FF`, and
how many `FF` bytes there were. First `FF` is the field's 1-based offset;
`last − first + 1` is its length. The count is reported separately so a
non-contiguous field shows up as a finding rather than being averaged away.

## Run it

```
1. Compile and run the specimen first, to check the toolchain:
      IBMLAYOUT.cbl as shipped declares SAMPLE.cpy, which contains
      no customer data and no customer field names.

2. Compile with your normal options. Please record them — see step 4.

3. Run it. It writes a JSON document to SYSOUT. Capture that to a file.

4. Send back THREE things:
      - the JSON document
      - your compiler version   (the IGYC banner line, e.g. 5655-EC6 V6 R3 M0)
      - the compile options in force (e.g. LP(64),ARCH(12),TRUNC(STD))

   The last two are not paperwork. A layout is a fact about a compiler AT
   SETTINGS, so a returned run with no version and no options cannot be
   recorded as a measurement of anything, and we will have to ask again.
```

We will normally send you an `IBMLAYOUT.cbl` already generated for *your*
copybook, so step 1 is the only place `SAMPLE.cpy` appears.

## One thing your compiler will probably reject, and what to do about it

IBM restricts reference modification:

> "You can refer to a substring of a data item that has USAGE DISPLAY,
> DISPLAY-1, or NATIONAL by using a reference modifier."
> — *IBM Enterprise COBOL for z/OS Language Reference* 6.3, "Reference
> modification"

The probe marks a field with `MOVE HIGH-VALUES TO field (1:)`. On GnuCOBOL that
is legal for every category and is how our sealed benchmark was measured. On
IBM Enterprise COBOL it is legal only for `USAGE DISPLAY`, `DISPLAY-1` and
`NATIONAL` — so the paragraphs commented `IBM-REFMOD-INELIGIBLE`, which are the
`COMP`, `COMP-3` and `COMP-5` fields, are expected to be **rejected at compile
time** on your system.

We have no IBM compiler to test against, so we are telling you this rather than
discovering it with you on a call.

**For those fields, use IBM's own output instead.** Compile the copybook with
the `MAP` option and return the listing, or transcribe it into this shape:

```json
{
  "schema": "relian-discovery-verify/normalised/v1",
  "fields": [
    { "name": "WS-CTR", "offset": 11, "length": 2 }
  ]
}
```

`MAP` is authoritative, needs no program to run, reads no data, and covers every
category. If it is easier for your team, **use the `MAP` path for everything and
skip the program entirely** — we accept either.

## What happens to what you send back

`ingest_verification.py` compares it against what we projected and reports, per
construct, **confirm**, **contradict** or **unknown**.

A **contradict** is the most useful outcome for both of us: it means our rule
table is wrong for your compiler, and we fix the rule rather than the report.

Everything you return is recorded as a measurement of **your compiler at your
settings** — tagged with the version and options you sent — and is never
generalised into a claim about "IBM". If your shop runs two compilers or two
option sets, they are two measurements.

## Files

| File | What it is |
|---|---|
| `IBMLAYOUT.cbl` | The probe program, as shipped, declaring `SAMPLE.cpy`. |
| `SAMPLE.cpy` | Synthetic specimen copybook. No customer data. |
| `gen_ibmlayout.py` | Generates `IBMLAYOUT.cbl` for a given copybook. Runs on our side. |
| `ingest_verification.py` | Reads your returned run; reports confirm/contradict/unknown. |

## Questions your team may reasonably ask

**Does it modify our copybook?** No. It `COPY`s it, or declares it inline.

**Does it need authorisation, an APF library, or a special region?** No. It is a
batch COBOL program that does arithmetic on its own `WORKING-STORAGE`.

**Why not just send us your layout so we can eyeball it?** You can, and that is
useful. But a diff of two documents produced by two different methods is
evidence; a person reading a table is not, and offsets are what data gets loaded
with.
