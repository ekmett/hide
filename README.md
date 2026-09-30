# thc-edit

A Haskell editor in deliberate homage to the Borland Turbo Pascal IDE.

This standalone project will provide the `thc-edit` executable, intended to be
available as `thc edit` through THC's external-command dispatch. Editor
dependencies remain independent of THC's compiler and runtime.

The implementation language is Haskell, with Vty handling terminal I/O.
The initial target is faithful menus, windows, mouse interactions and modal
dialogs, followed by robust editing, Haskell highlighting, optional WordStar
keys, HLS tooling and Cabal project browsing.

**Status:** design stage; no editor executable exists yet.
See the [proposed design](docs/superpowers/specs/2026-09-30-thc-edit-design.md).
