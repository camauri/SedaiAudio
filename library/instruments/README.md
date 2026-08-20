# Instruments written in SedaiBasic MODERN

A `.patch` is a table. It diffs and versions like source, which is most of why
it exists — but a table cannot say *"and nine of these, each a bit louder than
the last"*. This is where that is said instead.

    sb library/instruments/hammond.bas > library/patches/hammond.patch

That is the whole mechanism, and it is worth being precise about what it is
not. **This is not a translation. It is an execution.** The `.bas` is a real
program that really runs: the loops loop, the arithmetic is done, a child class
overrides a method and the override is really dispatched. The `.patch` is the
program's *side effect* — the objects in `saf.bas` do not make sound, they take
notes, and at the end they print what they were told.

Which means you are not writing a description in disguise. You have the whole
language: functions, conditionals, inheritance, computed values, a file read if
you want one. The `.patch` that comes out is flat and dull, exactly as it should
be. The intelligence lives in the `.bas`.

## Neither project knows about the other

`sb` has never heard of SAF: it prints text. SAF has never heard of `sb`: it
reads a `.patch`, which is what it read yesterday. Nothing links, so there is no
dependency that could become circular — and an instrument, once generated, keeps
working on a machine where SedaiBasic is not installed.

## What is here

| file | |
|---|---|
| `saf.bas` | the library. **Generated** — see below |
| `basic.bas` | the reference subtractive voice, lifted from `basic.patch` |
| `hammond.bas` | nine drawbars as a **loop**, not as nine copied blocks |
| `moog.bas` | a *family*: a parent with three seams and a child that changes one |

## `saf.bas` is generated, not written

    bin/<plat>/patch_bas --lib > library/instruments/saf.bas

It is produced by building **every module in the registry** and asking it what
ports it has — the same trick that generates the module reference in
`../patches/README.md`. So it cannot describe an engine that does not exist, and
adding a module to SAF makes it appear here on the next run. It is committed so
that a fresh clone can write an instrument immediately; regenerate it after
touching the registry.

## The other direction

    bin/<plat>/patch_bas library/patches/moog_lead.patch > moog_lead.bas

`patch_bas` lifts an existing `.patch` back into MODERN. That exists mostly to
be a **test**: `.patch → .bas → sb → .patch'` and then require `.patch'` to
render *byte-identically*. Over the shipped library that is **25 of 25**, with
two effect patches skipped — they need an audio input, as `patch_fixture` also
skips them.

An `include` is followed, and preserved rather than flattened: the lifted
program calls `SafInclude` and the modules the other file declares are **bound**
instead of initialised, because they are not this file's to declare. The prefix
chains the way the loader chains it, so an include inside an include is
`outer.inner.name`.

⚠️ **A regenerated patch that uses `include` must sit beside the original.**
The include path is relative to the patch file, so writing the regenerated one
into a different directory breaks it — which looks exactly like a wrong
translation until you read the error.

## Traps, all of them measured

**`sb` exits 0 even when it fails.** `sb x.bas > out.patch` on a broken program
writes the error message *into* `out.patch` and reports success. Anything that
automates this must look at the **output** — check that it contains `module ` —
and never at the exit status.

**A variable may not be named after a type.** The library defines `Note`, `Env`,
`Amp`, `Osc` and so on, so a keyboard variable called `note` collides. Call it
`kbd`. The lifter sidesteps this by prefixing every variable with `m_`.

**Reserved words are fine as a FIELD, not as a variable or parameter.** `Out`
and `Pos` are legitimate field names — a field is always reached after a dot, so
there is nothing to be ambiguous with — but `pos` as a parameter is refused.
`Instr` and `Draw` are refused everywhere.

**An array of composite objects may not be a FIELD of a Type.** It is an access
violation on the first nested member access, read or write. As a local, or as an
array of pointers, it works. So modules are held as `Ptr` and allocated with
`New` — which is what polymorphism wants anyway: an array declared `As Osc`
could not hold a `Saw`.

**The order of connections is part of the sound.** An input sums its sources in
the order they were declared, which is what makes the velocity trick in
`../patches/README.md` exact. The lifter preserves that order; so must you.
