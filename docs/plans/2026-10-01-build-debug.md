# Build, run, debug, and resume

Compile (Alt+F9), Make (F9), and Run (Ctrl+F9) use the selected THC or GHC
installation and project target. Reuse the saved run configuration, extending it
with a toolchain choice. Resolve the project directory on the session host.
Pass argument vectors directly, without a shell. Refuse disk builds while source
buffers have unsaved changes. Keep output visible and allow stopping work.

Compile and Make stream bounded output into a read-only window without requiring
the optional embedded terminal. Source diagnostics also appear in Messages. Run
uses the embedded terminal when available, and captured output otherwise. Cabal
projects use Cabal with the selected GHC; standalone Haskell files use GHC.

Keep the DAP state machine shared between loopback TCP and stdio adapters. A
launch configuration supplies an adapter command and launch/attach arguments.
THC launch uses the compiler's supported startup and endpoint contract. Connection
failures and shutdown must release owned processes; attaching does not own the
program. Reconnecting a display preserves the session's debugger and build state.

Implementation and validation:

- Extend saved toolchain settings and test command vectors, spaces, absent tools,
  dirty buffers, source errors, streamed output, cancellation, and real GHC builds.
- Enable menu, status-bar, and keyboard actions on all frontends.
- Add DAP launch and stdio transport tests; verify THC with the current compiler.
- Complete remote/resume selection tests and preserve platform regression evidence.
- Update user documentation, run the integrated suite, scan commits, and publish.
