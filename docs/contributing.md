# Development

Build the editor, run the checks and exercise the frontend affected by a change.
The source stays separate from THC's compiler and runtime.

The root Cabal project builds five linked packages. `hide` owns the editor;
`packages/hide-agent-api` contains provider contracts, typed directory reads and
bound orchestration services; `packages/hide-plugin-api` owns scoped commands,
tool exposure, forms, menus and tree contracts;
`plugins/hide-acp` implements ACP transport and the provider adapter;
`plugins/hide-agents` contributes the Agents tree and orchestration tools through
public plugin APIs. The executable selects linked UI plugins in `app/Main.hs`. The ACP package
depends on the contract, not the editor or its frontend libraries.
Ordinary `cabal build` and `cabal test` use the linked packages from this checkout.
To build only ACP, without resolving the editor's native dependencies, use its
smaller project:

```sh
cabal build --project-dir=plugins/hide-acp all
```

The agent UI and tools also build without the editor or native frontend
dependencies:

```sh
cabal build --project-dir=plugins/hide-agents all
```

## Build and check

From the repository root:

```sh
cabal build all
cabal test
```

Native windows, browser support and embedded terminals are enabled by
default. [Installation](install.md) covers their native dependencies. To omit
components, use the same opt-out flags for build and test:

```sh
cabal build all -f-window -f-web -f-terminal
cabal test -f-window -f-web -f-terminal
```

The test suite replays the desktop's pure transitions and checks files using
real temporary directories, including encoding, permissions, symlinks and
external changes. The suite also covers session handling and live editor context.
Optional components add checks for browser transport, rendering assets and
embedded terminals. A passing model check and an interactive frontend check
establish different things; run the relevant frontend too.

Agent input uses a shallow IO validation step while the session owner is
serialized. Protected document and question identities, and draft content versions,
prevent direct widget changes from bypassing policy without comparing buffer text,
disk baselines or Undo. Equal numeric revisions do not authorize replacement.
Human input remains a pure model transition; screen permission hints run on the
capture worker and do not replace admission checks.


## Coverage and test results

[CI](https://github.com/ekmett/hide/actions/workflows/ci.yml) builds the optimized
editor checks and runs correctness and allocation checks once.
Its NixOS job also builds the installed editor and checks sessions, aliases and
browser assets in a NixOS VM. Nix uses the upstream binary cache and a restored
GitHub Actions store; dependency tests, Haddocks and profiling libraries are not
part of this production build.
[Coverage](https://github.com/ekmett/hide/actions/workflows/coverage.yml) builds a
small HPC driver covering editing, file I/O, display transport, Markdown,
settings, plugin commands and recovery. It reuses inexpensive bounded contract
checks, with short end-to-end paths for transport and recovery.
It does not repeat the correctness suite or its allocation and process-lifetime
fixtures. Coverage measures the paths exercised, not an exhaustive behavioral
matrix.

Both workflows use GHC 9.14.1 on Linux x64, macOS ARM64 and native Windows x64.
Each host uploads JUnit results to
[Codecov](https://app.codecov.io/github/ekmett/hide) under its own platform flag;
the coverage job also uploads source coverage. Missing reports and upload errors
fail the workflow. A failed test still uploads the reports it produced; canceled
jobs do not invent results. Documentation-only changes use the documentation job.

The common configuration enables the browser and shared editor services, omitting
SDL windows and Ghostty terminals. HPC measures Haskell execution, not C or
shaders; native frontend and terminal checks belong to their feature builds.
Coverage uses an unoptimized build: HPC counters change optimization and allocation,
so the optimized CI job owns allocation budgets.

Run the optimized checks locally:

```sh
cabal test editor-tests --builddir=build/allocation --disable-coverage -O1 -f-window -f-terminal
```

Select a check with `--test-options="--pattern=BufferTree"`. Each invocation owns
its temporary resources. The runner excludes overlapping execution where checks
temporarily own the process environment or working directory; no check depends on
a preceding check's results or side effects.

Generate coverage reports from the repository root:

```sh
cabal install hpc-codecov-0.6.4.1
reports=$(mktemp -d "${TMPDIR:-/tmp}/hide-coverage.XXXXXX")
cabal build coverage-smoke --builddir=build/coverage --enable-coverage -O0 -f-window -f-terminal
test_exe=$(cabal list-bin coverage-smoke --builddir=build/coverage --enable-coverage -O0 -f-window -f-terminal)
test_exe=${test_exe%$'\r'}
HPCTIXFILE="$reports/editor-tests.tix" cabal exec --builddir=build/coverage --enable-coverage -O0 -f-window -f-terminal -- "$test_exe" --xml="$reports/tests.xml" +RTS --read-tix-file=no -RTS
python3 tools/coverage.py --output "$reports"
```

The workflows have separate build caches. Completed builds are cached before
tests, so a test failure does not discard them. The macOS compiler installation is
cached separately. `HPCTIXFILE` keeps counts in the invocation's report directory;
`--read-tix-file=no` prevents accumulation on repeated execution. No cleanup is
needed to repeat a run or convert its reports. Each platform converts its own
matching instrumentation; Codecov combines the commit's reports without carrying
old platform coverage forward.

Download the `coverage-<platform>` artifact and open `hpc-html/hpc_index.html` for
expression coverage, columns and Boolean outcomes. The artifact retains matching
`.mix` files, raw `.tix` counts, HTML, LCOV, JUnit and host/toolchain metadata for
seven days. HPC spans retain one-based character columns and inclusive ends, with
tabs expanded to stops of eight columns. Codecov receives the coarser LCOV
line/branch report; its upload does not preserve those column spans.

## Source documentation

Each `Hide.*` module starts with an overview of its role, ownership and notable
control flow. Public API Haddocks describe caller obligations: coordinate units,
resource lifetime, worker/adoption boundaries, privacy checks and failure behavior.
Keep those contracts beside the owning operation when changing it; avoid repeating
the type signature in prose. The [architecture guide](architecture.md) connects
the subsystems.

Generate the initial API reference and linked source with:

```sh
cabal haddock lib:hide --haddock-html --haddock-hyperlink-source
```

API documentation is preliminary, especially the model's internal helpers and
native FFI exports. A documentation build checks parsing and links, not the truth of the
contracts: review those against implementation and the relevant behavioral tests.

## Plugin command implementation

Define a typed `CommandDef` from `Hide.Plugin.Command` with input
and output codecs and a narrow host-supplied context. `withRegistry` scopes live
registrations; `registerCommand` rejects duplicate names. Native callers use the
typed handle with `invoke`. Wire adapters capture a `CommandRef` before queueing
and use `invokeJSON`, which validates input and prepares the complete JSON result
on the calling worker.

Retirement and scope closure refuse later admission. Re-registering the same
name does not redirect old handles or queued calls. Work already admitted may
finish; the host's task owner is responsible for cancellation. Codecs and handlers
run outside the registry lock. Errors are forced before return; successful typed
values remain lazy and their consumers own later evaluation. Registration grants no authority and does not
automatically expose an MCP tool.

`hide.docs.read` in `Hide.Documentation` is a small working example. Its context only
resolves a documentation corpus root; it neither imports nor receives `Desktop`.
`Hide.DocumentationHost` scopes read, list and search registrations together.
`Hide.Plugin.Documentation` supplies checked requests, codecs and the public
`DocsServices` capability. `Hide.DocsTools` in the linked agent plugin exposes
those operations through the same permission wrapper. Help calls the same read
registration; filesystem discovery and bounded reads stay on the invoking worker.
Deferred calls after session retirement fail before resolving a corpus.

## Plugin tool exposure

`Hide.Plugin.Tool` exposes selected typed commands as MCP tools. A `Tool` declares
its wire name, read-only policy hint and `CommandDef`; registration alone grants
no authority. `withTools` scopes an immutable set around the session. It rejects
collisions with host tools, duplicate wire or command names, and malformed metadata
before discovery. Input schemas declare strict object fields; output schemas
describe objects. Codecs enforce their field types and bounds.

`callTool` invokes the exact registration on the tool worker, with an admitted
context or an explicitly self-admitting service. Arguments are limited to 1 MiB
and results to 4 MiB, matching the MCP transport. Oversized results fail explicitly rather than returning a truncated
success. Object results appear as both structured content and JSON text. Closing
the scope rejects retained calls; a missing tool has no fallback interpreter.

The linked `hide-agents` package declares the nine directory, spawn, rename,
message, wait, cancel, end, history and search tools in `Hide.AgentTools`.
`Hide.Plugin.AgentServices` supplies their host-bound actor and workspace.
Arguments cannot replace either: the adapter rejects a different spawn directory
before reservation, and the Hub rechecks caller liveness on each operation.
Ancestry, limits, provider lifetime, worktree isolation and task tickets remain
Hub-owned. These tools receive no human approval or settings capability.

Plugin declarations carry their service context. `EditorTool` receives
`EditorServices` after admission; `CoordinationTool` receives authenticated
`AgentServices`. `RequestTool` receives `RequestServices`, whose read and diff operations
request fresh permission themselves. This explicit distinction prevents nested
approvals and never follows from a read-only hint. Anonymous editor connections
cannot manufacture an agent context, and child coordination connections do not
acquire editor reads. The host checks name collisions across all three sets.
The docs plugin uses `mapToolContext editorDocumentation` to select its narrow
capability; context mapping preserves schemas, command identity and retirement.

The executable's plugin list determines discovery and permission registration.
The primary route exposes editor and coordination tools; a child's coordination
route exposes only coordination tools. Autocomplete keeps its separate restricted
route. The remaining editor tools still use host types.

## Immutable plugin buffer and window reads

`Hide.Plugin.BufferRead` in `hide-plugin-api` exposes checked page requests,
masked metadata and immutable text/byte replies. `hide-agents` declares
`list_buffers` and `read_buffer` through these services and owns their JSON and
hexadecimal presentation. Each call rechecks current actor, policy and privacy
through the host reader. Discovery grants no subsequent content access.

Text pages contain at most 1,000 rows and 131,072 characters; byte pages contain
at most 4,096 bytes. The host slices measured content before materialization,
forces the bounded reply on its worker, and exposes no tree, document or Undo.
An omitted buffer selector binds to the active source at dispatch. The captured
allocation frontier excludes later-created buffers; closed IDs are never reused
within a session. Closing the session reader rejects further captures, while the
plugin tool scope separately retires retained tool calls.

`Hide.Plugin.WindowRead` supplies the same bounded text pages for `read_window`.
`Hide.BufferRequest` captures the exact frame and prepared or logical body before
worker dispatch, then supplies `RequestServices.requestWindows`. The capability
is absent on other requests. An omitted selector keeps the originally active
window; a different selector is refused. Every call rechecks current actor,
policy, privacy and body identity through the host reader. Closing or replacing
the body before admission rejects the request. An already admitted immutable
snapshot remains readable after close.

Conversation pages address the logical transcript, independently of its visible
rows or wrapping. Worker-side projection applies privacy masks before producing
the bounded page. The plugin owns the `window-text` reply codec; no prepared body,
source tree or callable window handle crosses this package boundary.

The lower-level `Hide.Plugin.Buffer` is currently a host-internal module, despite
its namespace. It provides opaque `BufferRef`, `BufferRead` and `ContentVersion` values.
The host adapter `Hide.Plugin.BufferHost` captures immutable measured trees without
retaining the separate saved baseline, Undo or Redo roots. Deleted provenance
leaves in the live tree remain retained but are invisible to reads.
Public reads use distinct character, byte and
zero-based line coordinates, validate their full range and reject the wrong
representation. `readLines` preserves source line terminators; `readLine` gives
one editor row without its terminator. Whole-buffer reads are explicit worker
operations. Capture and version checks never flatten or compare contents.

`ContentVersion` combines the revision with immutable buffer identity. A new
buffer with the same revision and contents invalidates the captured version.
The identity check is conservative: replacing the immutable buffer to establish
a new saved baseline also requires a fresh version. ACP source freshness tracking
uses the same version check.

Linked handlers can discover open documents with
`listBuffers :: BufferReader -> IO (Either Text [ListedBuffer])` before requesting
an admitted capture. Each opaque entry exposes `listedRef`, `listedMetadata` and
`listedBinary`. Entries follow ascending host document IDs and preserve the
existing metadata mask: private titles are `[private]` and private paths are
absent. References grant no read authority; capture rechecks current existence,
actor, policy and privacy. Listing uses the same ingress, cancellation claim and
Permissions lifetime. Its shallow metadata excludes source images and Undo;
exceptional dirty comparison remains consumer-worker work.

Linked command handlers can call
`captureBuffer :: BufferReader -> BufferRef -> IO (Either Text CapturedRead)` from
`Hide.Plugin.Buffer` without a Desktop. The opaque reader belongs to the running
Permissions session and a host-bound actor. It requests fresh policy for every
call; it contains no Human context or reusable approval. Workers submit to a
bounded 32-request ingress, which refuses overflow explicitly. The session tick
transfers requests into the existing permission owner, captures current state,
and resolves typed replies. Cancellation withdraws one request; shutdown closes
acceptance and terminally resolves pending replies. Only the host's fixed capture
operation and actor check run during admission.

Public buffer and window services reuse the existing reader ownership. Source
buffer pages bind document identity and capture its current contents; window
pages bind the exact prepared or logical body before approval. Both use the
host's measured paging implementation in `Hide.BufferReadServices`, with no
separate command registry.

`BufferRef` combines the running session namespace with its once-allocated
logical document ID. It survives ordinary edits and reload of that document;
close/reopen and a new daemon/recovery lifetime cannot reuse the reference.
`ContentVersion` separately identifies the immutable content captured from it.
The namespace belongs to the existing Permissions session lifetime, so frontend
detach does not create a new scope.

A read receipt exists only after ordinary MCP policy dispatch or an accepted
approval, and expires when its capture callback returns. Queued authenticated
reads resolve their token and active actor again before capture; anonymous
inspection remains guest input. Privacy comes from the existing GuestAccess
owner, including private-buffer rejection and Conversation masks. Already granted
immutable reads may outlive the receipt and cannot be recalled. Ordinary capture
forces only cheap immutable constructors, releasing the separate Buffer/Undo
thunk; Conversation redaction and tree construction stay on the reply worker.
Metadata's modified flag normally uses root measures. After a byte/text mode
switch it preserves the exact existing encoding comparison, deferred to the
worker from narrow current/baseline text inputs, without retaining Buffer/Undo. The receipt does not grant edit authority or create a public
plugin CallContext.

Session activation and teardown are available through `Hide.Plugin.Session`.
Buffer and window APIs still live in the editor library. Moving those boundaries
into independently buildable packages, along with broader subscriptions and
custom widget/window types, remains tracked in
[the delivery plan](https://github.com/ekmett/hide/issues/1).

## Host checked edit ownership

`Hide.BufferEdits` owns opaque `PreparedEdit` values, worker preparation
through `Hide.Buffer.replaceRanges`, and all-target checked atomic adoption.
`Hide.Tooling` is the live HLS consumer; it retains protocol parsing, canonical
project admission, task cancellation/session ordering and reply delivery.
Preparation forces replacement trees, output content and selection spans on the
worker. It deliberately retains the file baseline and ordinary Undo state that
will be installed; these owned edits differ from immutable read images.

Under the session lock, `commitEdits` rejects duplicate, closed, replaced,
ambiguous or private targets before changing any buffer. Open targets require
matching buffer version and file-baseline identity (including its absence for
untitled documents). Success installs worker
values, preserves unrelated navigation and rebases current selections. Changed
buffers receive one ordinary Undo entry; saving stays explicit and a stale
result never rebases the proposed edits.


`buffer_apply_diff` captures each original editable buffer and optional file
baseline when its request is admitted. Whole-text strict diff validation,
replacement construction and comparison with the original approved diff run on
an unmasked worker through WorkspaceFilesMCP and BufferEdits. A new content
identity invalidates the request even when the numeric revision and text match.
No attempt silently captures a newer source. Patch input is limited to 1 MiB
characters before building an approval review.

MCPPermissions owns the same Waiting ticket, reply and lifetime across edited
approval attempts. Allow starts a worker without blocking the UI; invalid or
stale results leave the same private review editable for correction. Current
review identity guards completion, so an older attempt cannot overwrite newer
review text, selection, Undo or body. The exact applied diff and userModified
flag come from the worker result. Successful adoption adds one ordinary Undo per changed buffer and
never saves, including untitled targets.

The existing permission tick rechecks policy, attributed token/actor and current
target/privacy before the shared BufferEdits adopter. Enable becoming Prompt
requires a new approved request. A short per-ticket claim linearizes adoption
and its success reply against cancellation: cancellation first installs nothing;
adoption first owns the successful reply. Spawn/registration is masked; workers
run unmasked, and cancellation/finalizer joins run outside the desktop lock.
Request lifetime is separate from an attempt, so failed preparation cannot retire
a correction ticket or allow an obsolete completion to apply.

Host-internal handlers call
`applyBufferDiff :: BufferEditor -> BufferRef -> ContentVersion -> Text -> IO (Either Text DiffResult)`
from `Hide.Plugin.Buffer`, using `capturedVersion` from the admitted read. The
opaque editor belongs to the running Permissions session and a fixed actor; it
contains no cached approval or Human provenance. Call on a worker. The existing
32-request ingress is shared with captures, and owner admission refuses a stale
version before retaining any original source, including equal-revision replacement
in the queue gap. Current policy, actor and privacy still apply to every request.

`Hide.Plugin.BufferDiff` in `hide-plugin-api` exposes checked diff arguments and
an immutable result through a request-bound service. `hide-agents` owns the actual
`buffer_apply_diff` declaration and JSON codec. `Hide.BufferRequest` checks its
arguments and captures every exact content identity from the same Desktop
before worker dispatch. Only that request receives a diff capability; it cannot
change the target count, order, IDs or revisions. This capture does not grant
approval: the existing permission owner still checks the actor, policy and current
target on every invocation.

The reply worker retains only the editor, target keys and exact versions. The same
Waiting request owns editable correction attempts, cancellation/adoption claim
and terminal typed result. Cancellation retires the shared attempt; shutdown
rejects new calls and resolves accepted replies before joining workers outside
the session lock. Each public `DiffReply` carries its buffer ID, exact `appliedDiff`,
`userModified` and resulting revision; replies follow request order. The plugin
encodes that bounded reply on the worker; retiring its tool registry rejects
later calls. Buffers, version
tokens and preparation objects stay host-internal.

The public service accepts checked `ApplyDiffArguments`, built from a list of
`DiffEntry` values with `applyDiffArguments`. The agent tool uses the same
`buffers` array shape for every request, including one target, and returns
ordered entries under `buffers`. Its captured callback submits the whole batch
to the existing host owner in one call:

```haskell
applyBufferDiffs editor
  [ BufferDiff (capturedRef first)  (capturedVersion first)  firstPatch
  , BufferDiff (capturedRef second) (capturedVersion second) secondPatch
  ]
```

`applyBufferDiffs :: BufferEditor -> [BufferDiff] -> IO (Either Text [DiffResult])`
accepts 1–16 distinct open text buffers with at most 1 MiB characters across all
patches. Results follow input order. One invalid, stale, private or closed target
rejects the entire batch; success adds one ordinary Undo per changed buffer and
saves nothing. The host-internal singleton convenience follows this same path.

A batch uses one `buffer_apply_diff` permission ticket. In Prompt mode, each
fixed target gets its own editable diff; the user can navigate between them
with Tab. Allow validates every review version and original source, then installs
all prepared changes together. Correcting a diff cannot change the target list.
Review buffers are prepared before queueing, and full patch validation stays on
the worker. Policy IO also runs on its worker at admission, approval and adoption.

These are checked strict-diff requests, not reusable prepared-edit grants.
Generic plugin activation/event lifetimes remain part of
[the buffer service work](https://github.com/ekmett/hide/issues/4).

## Frontend command routing

Browser shortcuts and menu packets use canonical `hide.*` IDs from `Hide.Commands`.
Short browser spellings are not accepted. Main, native and browser menus share
`menuCommandAvailable`; queued input checks availability again when consumed.
Native menu tokens refer to catalogue commands rather than menu occurrences. Each
native menu lifetime stamps its queued events, so rebuilding the menu rejects old
events; repeated occurrences of an action share enablement updates.

Source context popups capture the window, buffer, revision and character selection.
A changed focus, caret or source revision dismisses the action rather than using a
new target. Source edits and reloads derive from the original buffer and advance
its revision; read-only output/transcript replacement is not an editable source
target. Moving input ownership to the sidebar also refuses a source choice.
Agent choices retain the conversation target. Other parameterized
context actions retain their own arguments. These checks do not replace host input
origin and policy checks. `Hide.Plugin.Menu` composes contributions in declared named slots by group,
order and namespaced entry ID. `withMenus` bounds snapshots to 256 entries;
unknown slots, duplicate IDs and stale lifetimes fail explicitly. `menuAction`
projects typed arguments from an immutable host admission snapshot on the worker,
then applies a worker-side presentation adapter. The
metadata snapshot contains no plugin callback or buffer payload.

The live `Hide.MenuCommands` host contributes **Help > Contents** through a
thin `hide.help.contents` command calling the existing scoped `hide.docs.read`
reader. F1, the popup, native catalogue and transported menu packets resolve the
same retained contribution. Documentation reading and Markdown preparation run
on one session worker; adoption preserves read-only Help styling, relative links
and shell-block/navigation metadata. Browser/native frames carry entry IDs and
exact generations plus a fresh non-secret registry nonce. A native catalogue rebuild also stamps a new Cocoa incarnation.

Install linked contributions before publishing the initial session snapshot.
For runtime activation, `publishMenuFromHost` strictly prepares an exact bounded
metadata delta outside the session lock, then queues it to the owner. Publication
closes positional popups and preserves intervening contributions. Published
retirement workers use `requestMenuRetirement` on the same bounded queue.
The owner drains at most 16 deltas per tick; queue capacity is 256 and applies
backpressure only to registration workers. UI retirement uses the direct
`retireMenuFromHost` owner operation, which never waits for queue capacity. Do not retire a published command/menu from
an unrelated worker. Shutdown cancels and joins work before closing its registry
scopes. Plugin agent-enable metadata cannot grant authority: the live host only
permits its exact first-party Help/navigation refs and retains existing protected-control
policy checks. Origin comes from host dispatch, never frontend JSON.

Messages > Go to source contributes `hide.messages.go-to` in `context.messages`.
The popup and direct Messages action capture diagnostic generation, selection and
source location. Every projection replacement advances the generation; idle HLS
cache ticks preserve it. Even an empty projection retains its popup owner so Hide
Messages remains usable. Selected-message actions have separate availability.

The menu worker uses `Files.loadFile` for unopened sources and prepares buffer
projection plus measured UTF-16 navigation before publication. Already-open
sources use an immutable content read and exact content version; adoption focuses
the matching live buffer and preserves unsaved edits. An intervening replacement,
close/reopen, diagnostic refresh or input-owner change refuses the prepared result.
Agent navigation retains existing path policy: the resolved effect target is
validated before dispatch, the worker checks captured authority paths before file
loading, and adoption checks the prepared canonical path/current open-buffer
policy. Human navigation retains ordinary access.
The existing general `Tooling.jump` still performs synchronous file navigation;
it is outside this contribution slice and should reuse worker preparation later.

This slice supports main-menu slots, Messages context contributions, runtime
publication/withdrawal, prepared documentation and source navigation replies.
Other context slots and full first-party routing through the typed registry remain
open in #2. This is not a frozen extension SDK.

`sh tools/check-native.sh` checks the SDL event queue and software glyph rendering
without opening a visible window. Each run owns a temporary directory for its
binaries and captures, removed when the run exits; concurrent runs do not share
outputs or need preparatory cleanup. Set `HIDE_TEST_GPU=metal` or
`HIDE_TEST_GPU=vulkan` to also check the actual GPU backend: atlas growth,
clipping, decorations and reuse of uploaded glyphs. Vulkan needs a display server
that supports GPU presentation; a headless Wayland compositor works, while Xvfb
alone lacks the required DRI3 support. Select the hardware ICD when checking a
headless machine so a software Vulkan driver cannot stand in for the GPU.

On macOS the script also checks an unshown application menu for duplicate
enablement and retained old menu-item stamps. These checks do not start an editor
session. To also verify restoring a minimized window, set
`HIDE_TEST_DOCK_RESTORE=1`; that additional macOS check opens and restores a real
window, then closes it.

## Native Windows terminals

With the [pinned Ghostty installation](install.md#embedded-terminal) on `PATH`,
set `$ghosttyPrefix` to its prefix and run these checks from the repository root.
Use the `clang` supplied with native GHC (its `mingw/bin` directory):

```powershell
New-Item -ItemType Directory -Force build/native-terminal | Out-Null
clang -Wall -Wextra "-I$ghosttyPrefix/include" -Icbits cbits/terminal.c test/native-terminal.c "-L$ghosttyPrefix/lib" -lghostty-vt -lshell32 -o build/native-terminal/native-conpty.exe
./build/native-terminal/native-conpty.exe
./build/native-terminal/native-conpty.exe --parent-control
./build/native-terminal/native-conpty.exe --no-console
ghc --make -threaded -XGHC2021 -DWITH_TERMINAL -isrc -itest -outputdir build/native-terminal/hs test/native-terminal.hs cbits/terminal.c "-optc-I$ghosttyPrefix/include" -optc-Icbits "-L$ghosttyPrefix/lib" -lghostty-vt -o build/native-terminal/wrapper-check.exe
./build/native-terminal/wrapper-check.exe (Resolve-Path build/native-terminal/native-conpty.exe)
```

The C fixture exercises real console input, resizing, process-tree termination,
bounded queues and final output. The two additional modes check that starting a
terminal preserves an installed parent handler and works without a parent
console. The Haskell companion checks Unicode paths, argument quoting and
Ctrl-C in `cmd.exe` and PowerShell. Each command must exit successfully.
`cabal test` also checks retained editor terminal windows and ACP terminal requests;
the ACP fixtures require a working `python3` on `PATH`.

## Rendering previews

These previews use the actual Vty rendering output at 80 by 25 without starting
an interactive terminal:

```sh
cabal run -v0 hide -- --demo --snapshot
cabal run -v0 hide -- --demo --scene menu --snapshot-html > menu.html
cabal run -v0 hide -- --demo --scene help --snapshot-html > help.html
```

The HTML is a static preview. Available scenes are `desktop`, `menu`, `about`,
`gallery`, `split`, `open`, `tree`, `help`, `diff` and `preferences`.
**Tools > Widget gallery** exercises the dialog controls interactively.

## Documenting keyboard shortcuts

Verify bindings against `nativeMenuShortcut` in `src/Hide/Window.hs`,
`cbits/menu.m`, `cbits/window.c`, `src/Hide/Frontend.hs`,
`src/Hide/Model.hs` and `assets/web/editor.js`. Native menus, browser
shortcuts and terminal input are different entry points; do not infer a Mac
binding by replacing Ctrl with Command.

Use separate platform columns when bindings differ. For Mac, follow
[Apple’s modifier order](https://developer.apple.com/design/human-interface-guidelines/keyboards):
Fn, Control, Option, Shift, Command. Write symbol combinations without plus
signs (⇧⌘Z, ⌥⌘F), or full key names joined by hyphens (Shift-Command-Z).
Use Return and Esc in Mac columns. Write Ctrl+Shift+Z in Windows/Linux columns.
Repeat modifiers for each alternative; avoid ambiguous forms such as Ctrl/Cmd+G.
Keep shortcut tables in the relevant feature guide: [editing](editing.md#text-and-selection),
[conversations](conversations.md) or [running and debugging](running.md). Link to
those guides from the README rather than duplicating the tables. Update the
owning guide when its bindings change.

## Documentation screenshots

The site uses actual Metal captures of the editor running over this checkout,
including its modal dialogs. On macOS, build the window backend and run:

```sh
cabal build lib:hide
mkdir -p build/docs-capture/objects
cabal exec -- ghc -threaded -XGHC2021 -package hide -package hide-agents -itools tools/docs-screenshots.hs -outputdir build/docs-capture/objects -o build/docs-capture/screenshots
build/docs-capture/screenshots
make docs
make docs-check
```

`tools/EditorDriver.hs` supplies reusable commands, input events (including their
effects), text entry, condition-based waits, and Metal captures with a colorless
text companion. The screenshot file contains scene recipes and crop selection;
new UI workflows can reuse the driver without adding another event loop or
capture implementation. These are trusted local development scripts. Agents
controlling a live session use the permission-checked MCP tools described in
[Session tools](session-tools.md), including their protected-input rules.

Pass scene names to refresh only affected images, for example
`build/docs-capture/screenshots preferences debug-launch`. The tool loads the real
Cabal file and `src/Hide/Buffer.hs`, invokes the normal editor commands,
and draws hidden Metal windows at 3× scale, up to 100×32 cells, with **CRT filter** and
**Pixelate Unicode** enabled. It needs a macOS graphical login for Metal, but
shows no window and starts no editor session. Personal provider settings are
excluded from the default scenes; no source file is saved and no build or commit
is submitted.

The conversation illustration replays a recorded exchange through the current
renderer by default. Request it explicitly; an optional provider configuration
selects a new live exchange instead. Source stepping requires a live session:

```sh
build/docs-capture/screenshots conversation
# Optional live exchange (contacts the configured provider):
THC_DOCS_AGENT_CONFIG="$HOME/.config/thc-edit/agents.json" build/docs-capture/screenshots conversation
THC_DOCS_DAP_PORT=4730 build/docs-capture/screenshots debug-step
# Existing disposable hdb toy/config: counter = 41, values = [1..3],
# stopped before print counter on Main.hs line 7.
THC_DOCS_WATCH_ROOT=/path/to/toy THC_DOCS_HDB_CONFIG=/path/to/toy/debug.json \
  build/docs-capture/screenshots debug-watches
```

Set `THC_DOCS_CAPTURE_DIR` to a temporary directory to preview captures before
replacing the site assets. Recorded replay does not read provider configuration
or start an ACP connection. The optional live conversation asks the configured
provider to read `src/Hide/Buffer.hs` and explain it, then asks a short
follow-up about edit costs. Use a provider permitted to read this checkout; its
configuration is copied only into ignored capture scratch space. The debugger attaches to an
already-running, suspended local DAP endpoint, steps into the program, captures
the source, Debug menu and call-stack picker, then continues it to termination.
The `debug-watches` recipe launches the supplied hdb configuration, opens the
source’s real Add Watch action, evaluates scalar and list expressions, and captures
both the expression dialog and the expanded Watches result. Use a disposable toy
program that terminates, not an interactive debugging session. Live modes close
their connections on exit. Their compact desktops keep the status-bar instructions
visible. Never stage provider configuration or session records with the images.

Dialog and menu images are cropped to their actual UI rectangles plus the shadow
and a small margin. Keep full desktops only when window arrangement is the subject.
Avoid repeating the same source background for unrelated controls.

PNG artifacts live in `docs/site/screenshots/`; BMP intermediates stay under
`build/docs-capture/`. Source comments beside the dialog definitions identify
which images and guide pages need refreshing. Changes to shared dialog frames,
fields, buttons or shadows require refreshing all dialog images. Keep the scene
list in `tools/docs-screenshots.hs` and the site's screenshot allowlist in
`tools/docs/Main.hs` together when adding or removing an artifact.

Inspect regenerated images for clipped fields, wrong selections or stale labels
before committing them. Check that each image still illustrates its adjacent
instructions. The site copies only the allowlisted images and validates their
links, including deployment below `/hide/`.

## Live language-server checks

With a compatible HLS and GHC on `PATH`:

```sh
cabal exec -- ghc -threaded -package hide test/HLSLive.hs -o /tmp/thc-hls-live
/tmp/thc-hls-live
cabal exec -- ghc -threaded -package hide test/EditorLive.hs -o /tmp/hide-live
/tmp/hide-live
```

The source also includes native-input, terminal, DAP, browser and remote-session
harnesses under `test/` and `tools/`. Their setup depends on the component under
test; consult the harness before running it against a live session.

## Live debugger checks

`test/DebuggerLive.hs` drives the real editor debugger through the shared
`EditorDriver`, using both normal commands and MCP operations. Compile it after
building the library:

```sh
cabal exec -- ghc -Wall -threaded -XGHC2021 -package hide -itools test/DebuggerLive.hs -outputdir build/debugger-live -o build/debugger-live/check
build/debugger-live/check launch 4734 /absolute/path/to/toy 10 > build/debugger-live/trace.jsonl
```

Use a disposable, terminating THC Cabal executable with debug source notes. The
last argument selects a breakpoint line in the entry source which execution
will encounter after the initial stop (10 is only an example).
The program must have work after that breakpoint for a trace-in stop; the
qualification toy uses nested primitive mutable-variable operations. Choose an
unused port. Launch uses the normal saved **Build target** settings, so select
THC first and provide its compiler, runtime and LLVM tools on `PATH`. To keep
personal settings out of a run, set `XDG_CONFIG_HOME` to a fresh scratch directory;
THC is the default toolchain. Builds still write into the selected project.

The check verifies embedded-source retrieval, a verified breakpoint at the
requested line, a fresh stop on that breakpoint, trace-in/step-over, and
termination. It also exercises step-out when step-over leaves the toy stopped.
It rejects stale suspension generations and saves state/output as JSON lines.
Step-over or step-out may legitimately terminate an optimized toy; the trace
records which controls were exercised. Otherwise the check continues it. It
fails if the program exits before the breakpoint hit or initial trace-in stop.
Each wait is bounded, and failure closes the owned debugger process.

Use `attach PORT PROJECT BREAKPOINT_LINE` instead of `launch` to qualify an
already-suspended local runtime. That process belongs to the caller: record its exit status and
clean it up if the test fails. A successful DAP termination check does not prove
its operating-system exit status. Run separate instances for each THC backend;
this check does not establish locals inspection, interactive stdin, exception
stops or another adapter's compatibility.

The source-linked [GHC debugging assessment](design/ghc-debugging-options.md)
covers candidate adapters, GHCi facilities and separate profiling/heap workflows.

## Finding your way around

| Location | Responsibility |
| --- | --- |
| `app/Main.hs`, `src/Hide/App.hs` | Startup, command-line options and effects |
| `Model.hs`, `Render.hs`, `Buffer.hs` | Desktop interaction, drawing and document state |
| `Files.hs`, `Reconcile.hs` | File access and external changes |
| `Tooling.hs`, `LSP.hs` | HLS integration |
| `Git.hs`, `GitOperations.hs` | Review, commits and background Git commands |
| `Conversation.hs`, `ACP.hs`, `AgentFiles.hs`, `EditorMCP.hs` | Conversations, provider protocol, editor-mediated files and live buffer context |
| `Terminal.hs`, `Consoles.hs` | Embedded terminal and process management |
| `Debugger.hs`, `DAP.hs` | Debugger interaction and protocol |
| `Window.hs`, `Web.hs`, `BrowserServer.hs` | Native and browser frontends |
| `Session.hs`, `Remote.hs`, `RemoteEndpoint.hs` | Session records, processes and connections |
| `RemoteTerminal.hs`, `RemoteWindow.hs`, `RemoteWeb.hs` | Terminal, native and browser session frontends |
| `Protocol.hs`, `Font.hs`, `cbits/`, `assets/` | Display transport, fonts and native support |

The module filenames above are under `src/Hide/` unless a directory is shown.
[Architecture](architecture.md) explains how they fit together.

## Documentation and attribution

Build the documentation site with Pandoc and the separate Haskell generator:

```sh
make docs
make docs-check
```

The site is written to `build/site`. The generator builds independently of the
editor and checks local links, fragments and revision-pinned source links.
The Documentation workflow rebuilds and publishes every push to `main` at
[ekmett.github.io/hide](https://ekmett.github.io/hide/). It can also be
started manually from GitHub Actions.

Keep the [README](../README.md) useful both on GitHub and as F1 Help. Put detailed
workflows in the [user guide](README.md), and keep design history in `design/`
and `plans/`. User documentation should name the action, menu or key needed to
perform it.

Bundled font terms and provenance are in [assets/fonts](../assets/fonts/README.md).
Skylighting and its bundled grammar set are GPL-2 licensed; skylighting-core is
BSD-3-Clause. These are editor dependencies. The
[Turbo Pascal UI museum](https://ilyabirman.net/meanwhile/all/ui-museum-turbo-pascal-7-1/)
records the interface reference.

Run `sh test/launchers.sh` after changing the adjacent `thc-edit` or `th` launchers.

## Shared sidebar providers

`Hide.Plugin.Tree` registers a provider through the existing scoped typed command
registry. Provider-local node IDs are independent of labels and resource paths.
Prepared child pages contain at most 128 nodes and bounded presentation/cursor
fields. Node actions retain typed arguments and exact command registrations. Prepared
secondary declarations supply registered actions or captured resource-link targets;
filesystem extension rules stay with the Files provider.
`Hide.SidebarCommands` owns publication, load admission and reply adoption; no
extension callback runs during a tick or paint.

Files uses this ordinary provider route. `Hide.Sidebar` stores node metadata and
an indexed visible-row map keyed by prepared ancestry addresses. Collapse removes
a contiguous subtree with ordered-map splits. Projection workers prepare branch
prefixes, spans, row indices and directory subscriptions; paint reads only the
viewport slice and cached badges. Selected/top row identities anchor adoption.
Provider/node/request generations and captured ancestry reject obsolete loads
and actions. An obsolete Loading token is released without resetting a newer
request. Hiding the sidebar retains live provider declarations; reopening remounts
their prepared roots with fresh node/request epochs. Obsolete actions cancel on
the existing worker queue and release their bounded slot after reaping. The host
controls exact Files refs permitted for agent navigation;
resource metadata never grants authority.

There are at most four active child loads, 64 waiting loads, one projection, one
action and one badge worker. Registration publication uses a 32-entry bounded
queue and adopts at most four deltas per tick. Cancellation is scheduled off the
UI lock. UI state retains at most 32 roots, 32768 nodes and depth 64. Directory
enumeration remains on the existing Browser/filesystem worker; delivered pages
are bounded. Dirty snapshots are evaluated on the badge worker.

Files primary opens use the typed action worker. Already-open files use captured
window/buffer/content versions and never depend on another disk load; scope closure
withdraws provider roots at the owner. Its secondary Open keeps the
existing actor-stamped link worker/client-resource transport and a frozen sidebar
target. Moving that transport into the typed reply path is a separate slice.
Recovery stores bounded path/expansion/viewport hints, never live provider handles.
Selection and top-row indices are remapped after filtering resource entries.
It restores targets reachable through initial directory pages; hints beyond those
pages remain pending until interactive paging makes them reachable. Reachable
saved expansions are restored through the existing projection worker in bounded
batches; recovery never automatically follows a More cursor. New selection or
scrolling replaces the corresponding saved viewport intent, and an explicit
collapse discards that branch's deferred expansions. Checkpoints combine current
resource rows with unresolved hints, so another restart does not lose them.
`SidebarCheck` exercises Files and an independently declared test provider through
actual keyboard input, delayed/collapsed loads, paging, retirement, privacy and
filesystem observation refresh. Other domain providers remain subsequent work.

`Hide.AgentUI` in `hide-agents` consumes this tree for the Agents root. Its scoped
actions return typed `DirectoryRequest` values with opaque host receipts. The host
specializes these as `AgentSidebarRequest`. After exact hit/lifetime/modal
validation, `tickSidebar` dispatches one `AgentSidebarAction` through the existing
Conversation interpreter. Plugin handlers receive no Desktop or unrestricted
effect-list callback. Conversation owns captured-ID views and one creation attempt; AgentHub owns
rename validation and the shared spawn/task rollback. Agents Rename uses public
`Hide.Plugin.Form` preparation and the existing Sidebar action worker. The host
retains its SelectedInput draft/selection, validates exact FormRef ownership,
claims a human submission once and rechecks registration before dispatching the
closed RenameAgentTo result. Form metadata refresh carries no callback, cannot
open a modal, and preserves draft/selection through resize. Scope retirement or
a newer modal rejects a delayed reply. `PluginFormCheck` exercises public
lifetime laws; `AgentSidebarCheck` covers the real input/adoption route.
`ConfirmationFormSpec` uses the same lifecycle with no input field and an empty
submission. Sessions uses it to confirm deletion on the sidebar action worker;
the deletion rechecks catalog identity and liveness under the daemon lifetime
lock. Recovery rechecks checkpoint existence under that lock before publishing
a daemon, so a pending recovery spawn cannot recreate a deleted session.

A single metadata worker reads the public agent directory and compares its small
snapshot of names, parent IDs, states and advertised-choice availability. Tasks,
histories, transcripts and private provider keys never enter that snapshot. The
worker publishes changed-node invalidations through the bounded sidebar queue;
there is no plugin callback on the UI tick. The host drains at most four deltas
per tick, reusing ordinary request generations, cancellation and page adoption.
Closing the host wakes blocked publishers and rejects later publications.
`SelectedInput` supplies a reusable single-line selected range; ordinary caret-only Input behavior is unchanged.

## Prepared plugin windows

`Hide.Plugin.Window.prepareTextWindow` and `prepareMarkdownWindow` prepare
read-only content on a command's worker. `withWindowScope` bounds its publication
lifetime. `openWindow` returns an opaque `WindowUpdate` for a new exact
instance; `refreshWindow` publishes a later complete prepared presentation.
A `MenuReply` can return `PreparedWindow`
through the existing contributed-menu adapter. The host checks the exact menu
registration and captured target before installing the result; plugins never
receive a mutable Desktop or a rendering callback.

The view has its own window identity and normal frame, title, number, focus,
movement and resize geometry. It adds no source Document or BufferRef.
`Window.windowContent` distinguishes source identity from plugin content, and
`bufferId` returns `Nothing` for plugin views. Source-only services use the
checked `windowDocument` lookup. Selection and copying operate on the prepared
semantic text; source editing, Save and language tooling are unavailable there.

Prepared windows are private by default. `prepareSemanticTextWindow` can declare
readable content with `TextSemantics`: message-copy behavior, links and their base,
shell spans, and separate hidden ranges for agent reads, Streamer mode and recovery.
Preparation validates scalar ranges and evaluates metadata on its worker.
Readability grants no input or execution authority, and cannot change during a
window's lifetime. Title disclosure follows the same declaration.

The same adapter accepts `SidebarWindow` replies after existing sidebar action
checks. Duplicate opens and stale refreshes are refused, and refresh retains host
geometry and selection, clamping offsets when text shrinks. Closing removes the
installed body; an escaped update cannot reopen it. Scope retirement revokes
publication while retaining an inert read-only snapshot. Its old references cannot
refresh or reopen it. A content owner may retain the closed snapshot for an
explicit later opening under a fresh reference.

`prepareImageWindow title disclosure origin encoded` prepares a PNG or JPEG on a
worker and publishes through `openWindow`. The optional canonical origin remains
host metadata: the current protected-path policy can hide a formerly readable
image. Preparation caps compressed input at 16 MiB, dimensions at 4096 per side,
and decoded area at 4 megapixels. JPEG frame bounds are checked before decoding;
EXIF orientation is applied on that worker while original encoded bytes stay
intact. Host admission allows 64 image views and 64 MiB of distinct decoded
resources. Closing or scope retirement releases image bytes;
retirement retains only the small text fallback.

Image windows use Fit by default, `F` to fit again, `1` for actual size, plus/minus
or the wheel to zoom, and arrows or a content drag to pan. Host chrome and modal
input retain their normal owners. Terminal fallback reports dimensions and offers
an explicit Open externally link for file-backed images. Recovery retains the
inert `hide.image` description; it restores no links and never reloads the file.
Original encoded bytes and active image resources are never checkpointed.

`renderCellRowsAndCanvas` produces fallback cells and image ownership in one
composition. Each mask cell is little-endian uint16: a frame-local image slot in
the low 15 bits and half-intensity halo shading in bit 15. Ordinary glyph cells
and privacy masks retain zero ownership. Native, browser and logical agent PNG
capture use the same image transform and mask; nearest sampling composites
straight RGBA8 onto black. Capture includes only permitted images and describes
only image surfaces with visible owned cells. Pixel resources remain immutable
while pan, zoom and window layout change.

Ordinary views are transient. `prepareRecoverableTextWindow` takes an explicit
`WindowDisclosure` for live observation and declares
non-secret text eligible for private recovery, with a namespaced type ID and
positive format version. Recovery preserves text, title and host geometry in an
inert unavailable view with a fresh revoked reference. Parsing never runs plugin
code and a restored type does not automatically reattach to a registration.
Prepared content identities make redraw/checkpoint invalidation shallow.

Persistent multiline input uses `Hide.Plugin.Editor`: the host retains the draft,
Undo and selection independently of body refreshes. `openEditorWindow` publishes
an input attachment and prepared body together through the same command owner.
See [embedded editor ownership](design/haskell-plugins.md#embedded-editor-ownership)
for submission and lifetime rules. Conversation uses that input attachment and
prepared semantic bodies, with history rendering on the existing presentation
worker. See [prepared transcript content](design/haskell-plugins.md#prepared-transcript-content)
for copying, background adoption and recovery. Automatic reattachment and richer
widget APIs remain tracked in [#5](https://github.com/ekmett/hide/issues/5); the broader
`WindowDef` signatures remain proposed.

Agent sidebar configuration returns prepared public choices with an opaque receipt
from the existing hub epoch/capability version. Primary and child configuration
and human steering recheck it inside the shared Hub control reservation. Primary
requests enter its existing ACP mailbox and complete only on acknowledgement;
cancellation remains with the synchronous conversation owner. Neither route
changes the selected conversation to dispatch. The primary's redacted provider
output, bounded tool/plan updates and context usage also enter the shared Hub
history/status APIs. Exact connection replacement retires its event sink, while
capability refresh preserves the provider lifetime and outstanding controls.
Conversation consumes the typed `HistoryPage`/`HistoryEvent` values from
`Hide.Plugin.AgentServices` directly; `Hide.AgentTools` owns the tool response.
Both use the same retention, byte/count limits and exclusive event cursors. Checkpoints preserve those event identities.
New Agent startup uses the existing runtime's single pending launch slot. Its
mailbox completion retains the Hub ID and initial task ticket; Conversation keeps
human form/workspace admission and status display. Shutdown joins acquisition
before saving the final agent checkpoint.
The separate ACP completion node consumes cheap metadata published by Autocomplete.
Only explicit choice discovery starts that existing lazy connection. Configuration
and prompts share its serial owner; session/configuration receipts and settings
incarnations expire old choices without introducing a second connection or policy
registry. Revealing/hiding its cached transcript does not retire the provider.

The Debug sidebar uses the same scoped tree route as Files and Agents. Provider
workers wait on the existing debugger owner through a 32-entry mailbox; each tick
admits at most four messages. Validated pages are capped at 1 MiB and 64 cached
pages per stop. Cache validation and prepared presentation run off the UI lock;
owner maps retain scalar provenance and validated immutable rows, never document
payloads. `DebuggerSidebarCheck` exercises real fake-DAP interleaving, captured
frame activation, passive cached ticks, sticky lazy-reference refusal, resume
expiry and thread-exit refusal of delayed child pages.

Source popups capture a copied expression of at most 4096 characters, the measured
clicked row, file path, source IDs and revision. Toggle breakpoint and Add watch
run as exact human-only menu contributions; their worker captures ContentVersion
and prepares canonical path/dirty state before the debugger owner rechecks the
live target. The main Debug breakpoint and keyboard action resolve the same
registration. A source label cannot authorize a pathless operation: only the
existing debugger-owned source map can. Source replacement advances revision.
`MenuContextCheck` covers stamped browser/native routing, byte/modal/agent refusal,
equal-revision buffer replacement and changed-source watch confirmation. Watch
expressions are stored in the existing owner (128 entries, 4096 characters each);
Watches is an ordinary scoped, collapsible root available without a stopped
session. Add/Edit/Remove use captured human-only actions; IDs are never reused
and edit revisions reject stale rows and confirmations. Canonical source origins
and captured privacy protect the expression editor and shared row projection.
Owner ticks compare only the catalogue revision; provider workers prepare labels.
`DebuggerSidebarCheck` covers the live management route, stale Remove after Edit,
expression bounds and private editor fields. Management and passive expansion do
not evaluate. `DebuggerWatchesCheck` uses the actual captured provider action
route for explicit Evaluate and root Force, with deterministic DAP replies.
Executing requests invalidate old value handles before dispatch and retain the
exact provider registration, stop, selected frame and watch edit revision. One
pending executing request and one bounded preparation worker use the existing
debugger owner; no new session registry or executing MCP tool is introduced.
Metadata/error preparation and canonical origin resolution happen off the UI
lock. Adoption rechecks registration, provenance, modal input and current path
privacy. Nonlazy child pages reuse the existing 128-row/64-page stopped cache;
root Force publishes its prepared first page, while nested lazy values stay inert.
Unsupported evaluation remains a local error until explicit retry. These checks
are deterministic DAP evidence. A separate headless live check on hdb 0.14 / GHC
9.14.1 / macOS arm64 verified a stopped-frame scalar result of `42`, list-result
expansion with an inert lazy child through the captured provider action route. No
root Force action was available: hdb Evaluate replies omit the lazy root hint.
Both THC backends reached a source stop, rejected explicit watch expression
evaluation with a bounded error, then continued to the expected result and exited.
These runs do not qualify live Force; nested Force remains open.
Local source following still uses the existing synchronous owner route.

Debugger source inspection accepts only positive references observed in current
stopped stack metadata, including human, inspector and sidebar stacks. Source
observations use bounded path metadata and scalar stamps; generation changes
expire them. A waiting inspector resolves the captured backing path outside the
UI lock, then uses the existing bounded owner mailbox. Admission and reply
publication recheck the handle and shared authority-path policy. At most four
source inspections can be outstanding. Pathless observed sources are output from
the human-approved adapter, not filesystem reads or universally sanitized text.

`documentOrigin` is canonical privacy provenance for generated documents. It
never grants file/save authority; recovery preserves it and shared private-document
policy denies protected origins. Adapter source responses prepare canonical
origins and measured buffers on an owned worker; owner adoption rechecks the exact
stop, frame selection and observed source stamp. Generated buffers retain that
observation, so reusing a numeric reference cannot revive an old source action.
Public status/stack projections omit whole private source records on waiting
workers; shared Debug tree rows retain canonical resource provenance for masking.
Private Add Watch prompts carry frozen source privacy rather than depending on
field labels. The human frame chooser is conservatively private until it exposes
prepared public semantic rows. Arbitrary adapter variables/output remain the approved execution
boundary and are not universally sanitized. Local filesystem source following
still uses the existing synchronous owner path; its async preparation is separate.

`Hide.SessionSidebar` uses the existing private session catalog on one scoped
discovery worker. Only bounded public session summaries survive preparation; no
startup arguments, checkpoints or provider keys enter its snapshot. The UI owner
publishes forced scalar window IDs/titles through `editorWindowEntries` and
compares separate catalog/window revisions, invalidating at most two nodes.
Closed `SessionSidebarRequest` values pass the ordinary tree hit/lifetime/modal
checks, then the live session owner rechecks session identity and window
availability via `activateEditorWindow`. Switch/Recover also captures the display
attachment epoch: a late sidebar reply cannot move a replacement display. Delete
uses the existing confirmation form and rechecks the saved record under the
session lifetime lock before removing a stopped checkpoint.

Recover buffers here reads a stopped same-project checkpoint on that action
worker under the same lifetime lock. `RecoveredSources` contains validated source
documents and views only; decoding never installs plugin or conversation owners.
Adoption checks the captured display attachment and project, then assigns fresh
IDs and clamps windows to the current workspace. Duplicate paths become unnamed
copies with their saved baseline, history and privacy origin intact. If retaining
two different origins would require discarding provenance, the whole import is
refused. Count and identity limits are checked before adoption; neither existing
buffer contents nor histories are compared.

`withSessionPeer` owns handoff within the existing frontend callback. It prepares
a candidate with a fresh input journal while continuing to receive the old
session's committed replies. Candidate admission waits for a complete reset frame
after applying the frontend, theme and dimensions. The transport owner then
selects the target and retires the old attachment. Preparation failure leaves the
old session usable; a later target failure reconnects only to that target.
Frontend queues stamp inputs with their displayed attachment, and admission
rejects old stamps. Native and terminal event queues also discard input collected
during the transition. These checks use scalar lifetimes and the three display
settings; they do not compare desktops, buffers or undo history.
