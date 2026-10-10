<!-- SPDX-FileCopyrightText: 2026 Edward Kmett
SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 -->

# Unified sidebar navigation

Status: the shared indexed provider tree is implemented for Files, Agents,
Sessions, Debug, Watches and packages, with scoped actions and lazy loading.
The keybinding section records implemented named-command configuration and
remaining grammar and control customization.

## Intended experience

The left sidebar is one scrollable tree view. Files is an ordinary root alongside
Agents, Sessions, Debug and Watches. There are no tabs or independently scrolling
sections. Expanding any node pushes later rows down; collapsing it returns the
space. Existing resizing and adjacent-window behavior stays with the sidebar.

```text
▼ Files
  ├📂 src
  │ └📂 Hide
  │   └📄 Debugger.hs
  └📄 hide.cabal
▼ Agents
  └● Compiler investigation
    └○ Inspect hdb capabilities
▼ Sessions
  └This session
    ├Debugger.hs
    ├Conversation
    └Terminal
▼ Debug
  └Thread 1 · stopped
    ├main  Main.hs:18
    │ └Locals
    │   ├answer = 42
    │   └▸ result = <lazy>
    └run  Main.hs:9
▼ Watches
  ├answer = 42
  └result = <not evaluated>
```

The sketch describes hierarchy, not final icons or alignment. Continue the
single-cell tree indentation and current blue/cyan/white palette. Selected rows
use the existing green selection. Section headers scroll with their contents.
Keep a compact sidebar hide control, without reserving a separate Files title.

Sessions is provisionally the editor-session directory with the current session's
windows beneath it. The user has been asked whether it should instead contain
only this session's windows. Other roots do not depend on that choice.

## Navigation

- Up/Down and Page Up/Down traverse visible rows; Home/End reach their ends.
- Right expands a branch; Left collapses it or selects its parent. Enter opens a
  file/conversation/window or follows a selected frame to source.
- A disclosure click expands without unnecessarily changing the active source.
  Selecting a frame establishes the scope for watches; expanding that frame loads
  its locals. Nested values use the same tree affordance.
- Keyboard and mouse selection use stable node identities. Updates above the
  viewport preserve its anchor, avoiding jumps as agents emit events or values
  arrive. Collapsing a selected descendant selects its surviving ancestor.
- Starting a user-visible debug session reveals and scrolls to Debug. A stop
  reveals the stopped thread and selected frame. Background/hidden agent debug
  work respects the existing presentation policy and does not steal focus.
- Empty roots remain reachable. Loading, unavailable, running and error states are
  explicit rows, rather than blank trees or modal errors for ordinary expansion.

## Node context actions

Capture the node's stable identity when opening a context menu, not its visible
row index. Async updates or scrolling must not redirect an action to another node.

- Right-click Agents offers New Agent. Right-click an agent offers Rename and
  model/effort selection from the provider's actual advertised choices.
- Rename opens an editable field containing the current name, selected so typing
  replaces it. Validate uniqueness through the existing hub and keep the stable ID.
- Include the persistent autocomplete agent while ACP completion is enabled. Mark
  its completion role; its hidden chat is still reachable, renameable and
  configurable. Do not create a duplicate hub entry for the same provider session.
  Its model/effort changes update the completion provider's settings, not the main
  conversation. Non-ACP completion services need not pretend to be chat agents.
- Right-click a filename in the navigation tree offers Rename with its existing
  basename selected. Use the checked workspace rename service, refusing collisions
  and preserving open-buffer identity/path updates. Dirty buffers require an
  explicit save or cancellation through the existing safeguards. Renaming a file
  is separate from renaming a Haskell symbol.

## Cabal package roots

In a directory containing a Cabal package, contribute another root named after
that package. Its children are component targets, and opening a target exposes
its source files. Multiple local packages may contribute separate named roots;
Files remains the filesystem view.

Use maintained Cabal package-description parsing with configured metadata when
available. The current plan reader supplies resolved components/dependencies,
not a complete source-file list. Without a fresh plan, display declared targets
with clear conditional/disabled/unknown state; merely expanding the tree must not
run Cabal configuration. Resolve modules and main files through the component's
source directories; distinguish missing/generated files. Opening a shared file
from two targets reuses its canonical buffer.

Target context menus offer Build. Executable and supported executable-backed
benchmark targets also offer Run and Debug. Test/benchmark driver actions remain
available for interfaces that are not directly launchable; libraries do not get
Run. Use the selected THC/GHC toolchain and shared build/debug target resolver.
Show a reason when a backend cannot perform an action instead of constructing a
command from the display label.

Capture workspace, package and component identity with the action. Revalidate
configuration when it runs. File discovery, plan reading and compiler capability
checks run on workers; metadata changes preserve the viewport anchor.

## Source context actions

Add Toggle breakpoint and Add watch… to the source context menu. A right-click
inside the current selection preserves that selection. Otherwise the click sets
its source location; Add watch starts with the identifier there. An editable
expression dialog permits a more complex watch without changing source text.

Capture the buffer ID and source row when opening the context menu; do not
resolve whichever window happens to be active when its action is accepted.

Breakpoints work before launch through the existing stored breakpoint map and
normal `setBreakpoints` flow. They target the clicked line and retain adapter
verification/error feedback. Byte buffers are excluded. Use measured buffer
coordinates rather than flattening the document to locate the clicked line.

A watch stores an expression, not a DAP reference. Add/Edit/Remove/Refresh actions
are available from its row context menu. Evaluate using DAP's watch context and
the selected frame while stopped; running or unavailable adapters leave an
honest pending/unavailable state. Expanding a value uses its returned reference.
Adapter errors belong to that watch, not to a replacement modal dialog.

Haskell lazy values retain the existing explicit-evaluation boundary: expanding a
marked lazy reference must not silently force it. Expressions can themselves run
target code; adding or explicitly refreshing a watch initiates evaluation, while
routine painting, scrolling and session polling never do. Automatic refresh of
already-installed watches is tied to a new stop or selected frame, not every tick.

## Ownership and performance

`Model` owns focus, expansion, selection, viewport and semantic navigation actions.
Use a shared sidebar node representation with stable keys and typed targets;
filesystem paths must not double as magic agent/debug command strings.

Files retain their directory cache. Agent rows come from the existing hub and
conversation selection operations. Session discovery and liveness checks run on a
worker; drawing must not invoke `listSessions`, probe endpoints or traverse disk.
Window rows use existing IDs and labels. Do not copy document bodies or undo
history into navigation nodes.

`Debugger` retains DAP ownership and publishes prepared sidebar data when replies
arrive. Cache thread stacks, scopes, variables and watch outcomes by session/stop
and their owning frame or reference. Expansion queues a request only when needed;
repeated paint/scroll cannot duplicate requests. Continuation, invalidation,
thread exit and disconnect expire affected handles. Late replies cannot revive
values from a previous stop, frame choice, launch or edited watch expression.
Use bounded variable/stack pages where adapters support them.

The visible-row projection is shared by painting, hit testing and scrolling. Its
invalidation uses small revisions and immutable identities, never equality or
hashing of the desktop, buffers, histories or full debugger payloads. Retain the
existing drag capture, coalescing and dock resize rules.

The debugger separates the stopped-state epoch from selected-frame/source-follow
identity. Frame selection preserves sibling stopped handles. References expire
on resume; source-follow replies additionally match the selected-frame revision
so an earlier click cannot move the editor back. Watches will use that same
selection boundary when their consumer is implemented.

## Configurable bindings

Stable named commands and context-sensitive bindings now use the existing
configuration hierarchy. See the implemented [keybinding schema and examples](../configuration.md#keybindings)
for global/project precedence, platform profiles, context names, command IDs,
replacement lists and explicit unbinding. Profiles are independent; there is no
generic platform table inherited by macOS.

Named WordStar commands, horizontal/vertical movement and selection, row/document
edges, pages, word movement/selection, word deletion, adjacent deletion and line
deletion use an editable profile, including WordStar Ctrl+A/F aliases.
Ctrl+K/Ctrl+Q block grammar remains fixed. Dialog editing/search actions are
configurable for existing editable TextArea controls. Dialog next/previous focus
and accept/cancel commands preserve the existing field, dropdown and button
owners. Caret-only Input movement and adjacent deletion use the editable profile;
Ctrl+U and other field controls retain their existing grammar. Published runtime menu contributions
use that same configuration and exact registration lifetime; see the
[plugin design](../design/haskell-plugins.md#commands-menus-and-bindings).

Resolve the most specific active context before global commands. Cover editor,
sidebar, conversation, debugger, terminal and dialog contexts. Ordinary typing
and terminal forwarding remain fallbacks; global editing shortcuts must not take
Ctrl+C away from a focused terminal. Detect conflicts and unknown command/key
names with actionable configuration diagnostics instead of silently selecting a
winner. Keep modal and agent-authority checks after command resolution.

Menus, context menus, status help, native macOS menus and browser key handling
must use the same effective bindings. Render modifiers in the current platform's
notation and ordering. Distinguish actual modifiers from text input so Option
characters continue to work on macOS. Remote input preserves client modifier
identity; labels reflect the attached frontend. Configuring a shortcut cannot
make an OS/browser-reserved combination available or make a terminal report a
modifier it does not transmit.

Load and validate maps outside the input/render hot path. Lookup uses the compiled
map for the focused context. Provide a reload-bindings command and document the
effective-binding lookup so users can diagnose an override without restarting.
The first implementation needs configurable commands and honest hints, not a new
keymap editing dialog or arbitrary command-execution macros.

## Persistence and authority

Checkpoint sidebar expansion/viewport and watch expressions with bounded optional
fields so old checkpoints still load. Never restore live DAP handles or claim
recovered values are current. Preserve existing conversation privacy, agent input
restrictions and hidden-debug policy when adding new routes to the same actions.
Session navigation must not expose private tokens or switch an attached display
without an explicit user action.

## Verification and documentation

Use the existing model/render checks for one shared scrollbar, collapse/expand,
viewport anchoring, keyboard traversal, source-selection preservation, section
height changes and existing docking geometry. Include a strictness check that
navigation/render invalidation does not force large source/undo payloads.

Extend the existing fake-DAP integration checks for clicked-line breakpoints,
watch evaluation, thread/frame selection, locals and nested variable expansion,
repeated expansion, invalidation, delayed replies, unsupported evaluation,
termination and explicit lazy-value evaluation. Also exercise both THC and hdb
manually before claiming live compatibility.

Check binding precedence, explicit unbinding, invalid settings, modifier order,
terminal Ctrl+C forwarding and native/browser command parity. Verify that
rebinding cannot bypass protected conversation/settings/approval controls.

Update the source context-menu and debugging documentation with relevant cropped
Metal captures, using the existing capture/control harness. Mark the screenshot
owners beside the affected render code so later dialog changes identify the
artifacts that need refreshing.
