<!-- SPDX-FileCopyrightText: 2026 Edward Kmett
SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 -->

# Accessibility for the native and browser frontends

Status: the visible sidebar, current modal dialogs and focused ordinary source
view have read-only projections for browser ARIA and macOS accessibility. Image
views also expose Fit Image, Actual Size, Zoom In and Zoom Out actions.
`editor_screen` uses the same host projections with the agent privacy policy. These snapshots carry no input authority. The wider desktop
bridge, offscreen text requests and Windows/Linux adapters remain proposed.

`semanticSidebar` carries at most 256 viewport rows and 512 nodes, including
required ancestors. Identity combines the opaque provider registration, sidebar
epoch and host-minted node ordinal; provider IDs and resource paths are not
serialized. `semanticDialog` is a complete bounded snapshot of the current
modal: at most 256 nodes, clipped cell bounds, labels, current values and focus.
Its structural field IDs survive typing and resize within that projection;
they confer no lifetime or action authority. Absence clears the dialog. A hidden
modal still suppresses underlying accessibility views. Before adding actions,
dialog instances need the explicit lifetime tokens described below.

Dialog values follow central privacy policy before serialization. Text areas
read only measured visible rows; preparation never flattens a source buffer or
Undo history. Keep the existing renderer and derive semantics from the model,
not painted character cells.

`semanticSource` names the focused source view and its buffer, with the displayed
line span, horizontal display offset and clipped cell bounds. Use the browser's
**Source excerpt** region or the corresponding macOS static text element to read
it, then return to the editor controls to move or scroll. Reading this excerpt
does not move the editor caret or send keystrokes to the host.

The excerpt contains at most 256 visible rows, 2,048 scalars per row and 32,768
scalars including line separators. Each row also caps consumed source at 4,096
scalars, so zero-width controls cannot force a scan of the whole line. Tabs are
expanded; a glyph cut by the viewport is replaced by spaces. Measured line and
horizontal seeks avoid flattening the file or its Undo history. Ordinary visible
runs borrow source slices; masks, tabs and control placeholders allocate only
their replacement text. IDs survive scrolling and moving the same view; split
views have different IDs. Frontends retain reading focus while replacing the
bounded text snapshot. Source editing, range queries and selection APIs are not
yet exposed through accessibility.

Menus, dialogs, sidebar/Messages focus, private views under the applicable policy,
and views with inline completion proposals clear the source excerpt. Markdown,
change views, hex, terminals and plugin windows need their own semantic content;
they are not reported as ordinary source. Detach and invalid metadata clear the
previous text. An explicit absent snapshot clears the source; ordinary frame
deltas retain unchanged semantics. Owner displays obey Streamer Mode; agent
captures always apply protected-buffer policy before reading a title or source
text.

Image controls carry the window instance, immutable image resource and a visible
cell anchor from the same ownership mask used to draw it. Browser buttons and
macOS custom actions retain reading focus across ordinary viewport updates. The
host checks the exact target and current hit geometry when consuming the action;
closed, replaced, covered or modal-blocked images cannot receive it. These four
operations use the existing image viewport model. They grant no source editing
or command execution, and agent input does not accept their packets.

The [plugin design](haskell-plugins.md#semantic-tree-and-accessibility-transport)
uses the same retained tree for standard widgets and canvas descriptions. Track
that shared wire contract in [#13](https://github.com/ekmett/hide/issues/13), with
canvas content in [#14](https://github.com/ekmett/hide/issues/14).

## What exists, and where the bridge belongs

[Model.hs](../../src/Hide/Model.hs) already owns useful semantics: persistent
window and buffer IDs, source selection, dialogs with typed fields, menus,
sidebar rows, diagnostics and conversation controls.
[Render.hs](../../src/Hide/Render.hs) turns these into Vty pictures; it is a
consumer of the semantic model, not the right source for an accessibility tree.
[Buffer.hs](../../src/Hide/Buffer.hs) provides measured persistent line trees,
`bufferSlice`, line lookup and revisions. Accessibility must preserve that
representation and avoid `contents`, whole-document comparisons or flattening
on the desktop thread.

[Window.hs](../../src/Hide/Window.hs) and
[cbits/window.c](../../cbits/window.c) run SDL on the bound main thread and paint
one native surface. [cbits/menu.m](../../cbits/menu.m) already creates real
`NSMenu` objects; preserve their existing accessibility instead of duplicating
menu-bar elements. Painted editor windows are children inside one OS window.

Crucially, normal session attachment uses
[RemoteWindow.hs](../../src/Hide/RemoteWindow.hs), including local daemon
sessions. The daemon in [Remote.hs](../../src/Hide/Remote.hs) owns `Desktop`;
the display receives frames. Adding AppKit hooks only to `Window.runWindow`
would miss that path. Extend [Protocol.hs](../../src/Hide/Protocol.hs) with
optional semantic updates and actions alongside the existing display transport.
The browser keeps its canvas and input textarea in
[assets/web](../../assets/web/index.html), with separate read-only semantic
regions. These regions do not dispatch editing gestures to the host.

## Shared semantic contract

Extend `Hide.Accessibility` beyond its sidebar projection with a checked action reducer.
The first representation needs only nodes, parent/child IDs, roles, names,
values, states, logical bounds, relationships, supported actions and optional
text references. Compose host chrome and published plugin semantic subtrees,
including canvas content, before creating the frontend projection. Geometry
distinguishes grid cells, canvas-local coordinates, frontend logical points and
device pixels; rendering, clipping, hit testing and accessibility use the same
transforms, including any positional distortion applied by a display filter.
Use `windowId` for views and `bufferId` plus revision for text;
split views share text but keep independent selection, scrolling and focus.
Assign an explicit lifetime token to each dialog/menu instance. Field indices
are identities only within that lifetime. Never derive identity from labels,
row position or mutable text.

Suggested entry points, to be refined with the first adapter:

- `projectAccessibility :: Desktop -> SemanticSnapshot`: structure and small
  metadata, retaining immutable buffer references rather than copying text.
- `diffAccessibility`: compare identity/revision/focus/layout keys and produce
  changed or removed nodes. Do not compare whole `Desktop` or all source text.
- `applyAccessibilityAction`: check attachment epoch, node lifetime, relevant
  buffer revision, current modal state and authority, then use the existing
  model command/edit transitions and ordinary effect executor.

Roles should describe operations: source as a scroll area containing editable
text; read-only output as document text; dialog fields as text fields, checkboxes,
radiogroups and lists; sidebar as tree; diagnostics as a navigable list; composer
as multiline text. Expose names for otherwise symbolic controls, current values,
shortcuts and disabled reasons. Keep painted window groups distinct from native
OS windows. An entire terminal is not an ordinary editable source document;
start with a named terminal region, bounded readable output and existing input
routing, then separately qualify shell interaction.

Focus is one explicit node, not “whichever window was painted last.” During a
modal dialog, constrain actionable navigation to that dialog and record its
return focus. A focused conversation draft, tree row, completion popup and source
caret must each be represented correctly. Stable parent/child relationships and
z-order hit testing must agree with the model's clipping and modal rules.

## Publication, actions and ownership

Capture an immutable desktop version under the existing session lock. Build
text pages, encoding indexes and serialized deltas on one coalescing worker,
then publish a revision-tagged snapshot. Bound pending snapshots and action
queues; new snapshots replace obsolete pending ones. Keep the currently
published version alive until readers release it. A detached display or closed
native window invalidates its attachment epoch before cleanup.

Native accessibility getters read a published snapshot. They must not wait for
the desktop lock, perform filesystem/process work, make a synchronous RPC to the
daemon, or invoke a Haskell callback that might recursively enter the event
loop. Actions enqueue semantic commands and wake SDL through its event queue;
the desktop performs validation again before changing anything. Use the action's
node/dialog lifetime and text revision, rather than rejecting harmless actions
merely because an unrelated cursor blink advanced a global frame generation.

A proposed C boundary is `thc_ax_open`, `thc_ax_publish`, `thc_ax_poll_action` and
`thc_ax_close`, with explicit byte lengths, ownership and epoch fields. Native
code owns its strings and objects; no borrowed Haskell heap pointers survive a
call. If stable pointers are used for immutable text snapshots, their release
must be explicit and must not run model actions. Make any FFI call that can
reenter Haskell `safe`; preferably keep routine native publication callback-free.

For attached sessions, extend the common protocol with semantic messages. Publish
an initial node snapshot, then deltas carrying monotonically ordered attachment and
revision IDs; gaps trigger resynchronization. Keep semantic messages bounded
independently from pixel frames. Ordinary agents must not gain a new privileged
transport by claiming to be an accessibility frontend.

## macOS adapter and geometry

Implement `cbits/accessibility.m` and a small header, using the existing Cocoa
build dependency. Obtain SDL's borrowed `NSWindow` through
`SDL_GetWindowProperties` and `SDL_PROP_WINDOW_COCOA_WINDOW_POINTER`, on the main
thread. SDL also exposes the Windows HWND this way. Do not release these borrowed
handles. [SDL window properties](https://wiki.libsdl.org/SDL3/SDL_GetWindowProperties)

Prototype a transparent accessibility host view attached to the content view:
it must leave SDL painting, hit testing, keyboard input and IME handling intact.
Have it expose stable `NSAccessibilityElement` subclasses for painted controls,
with role, parent, children and frame information. Verify root discovery and
focused-element lookup in Accessibility Inspector before extending the tree;
avoid runtime swizzling of SDL's private view class. Apple explicitly supports
viewless controls through accessibility elements and requires appropriate change
notifications. [Apple custom-control guide](https://developer.apple.com/library/archive/documentation/Accessibility/Conceptual/AccessibilityMacOSX/ImplementingAccessibilityforCustomControls.html)

Expose the current cell-to-view transform from `window.c` rather than copying
its geometry formula. It includes cell width, `cell_height`, `scale`, grid
letterboxing (`origin_x/y`) and drawable-pixel versus window-point size. Convert
cell rectangles to view coordinates, then use AppKit conversions to screen
coordinates. Account for flipped views, multiple displays with negative origins,
Retina backing scale, fullscreen and resize. Parent-relative element frames can
follow parent movement, but text range bounds still require correct conversion.
Canvas semantic bounds use the same
content-local-to-frontend transform as their painted surface, including any
post-layout CRT distortion that changes positions.
[AppKit parent-space frames](https://developer.apple.com/documentation/appkit/nsaccessibilityelement-swift.class/accessibilityframeinparentspace)

After publishing a consistent snapshot, send focus, selection, value and
structural notifications for actual changes. Coalesce streaming output; do not
announce every token or replace all elements on every frame. Focus notifications
use the accessibility notification API, and announcements should be reserved for
useful status changes. [AppKit focus notifications](https://developer.apple.com/documentation/appkit/nsaccessibility-swift.struct/notification/focuseduielementchanged)

## Source text is the largest part of the work

The source element needs text length, selected text/range, visible range, line
and index conversion, text for a requested range, bounds for a range, range at
a screen point, and scrolling/selection actions. AppKit provides corresponding
parameterized text methods, including `accessibilityStringForRange:`,
`accessibilityFrameForRange:` and line/position queries.
[AppKit text range API](https://developer.apple.com/documentation/appkit/nsaccessibilityprotocol/accessibilitystring%28for%3A%29?language=objc)

Define an explicit coordinate contract: editor offsets are Unicode code points;
AppKit `NSString`/`NSRange` offsets are UTF-16 code units. Display columns and
grapheme boundaries are a third coordinate system. Build UTF-16 measures/indexes
for the persistent text projection and reuse `Unicode` display calculations.
Do not reuse the current whole-`Text` LSP offset scan for every accessibility
query. Test non-BMP characters, combining marks, ZWJ emoji, tabs, CJK, CRLF,
empty final lines and selection across line boundaries. Never split a surrogate
pair, and distinguish glyph hit testing from legal text selection boundaries.

Use range slicing and shared unchanged chunks. Whole-document API requests are
inherently proportional to returned text, but must not cause full flattening on
every edit, caret movement or render. A frontend replica should apply revisioned
text edits rather than receive the whole file again. Recompute only changed
line geometry and encoding indexes; one exceptionally long line needs a chunked
index too.

Synchronous native text APIs make remote attachment a real design constraint.
A fully functional source-text provider needs a coherent local text replica,
including offscreen text. A viewport-only cache cannot honestly claim to expose
the complete document. Populate the replica asynchronously before advertising
full document support; meanwhile expose a clearly named read-only visible
excerpt and loading state. Qualification must cover cold attachment and slow
SSH, not only warm local caches. Set and document a memory budget; when a file
cannot fit, preserve the explicit limited mode rather than silently truncating
full-range answers. A later paged implementation must establish acceptable
native-query failure/retry behavior with real screen readers before replacing
that fallback. This is a shipping limit, not something an ARIA role fixes.

Keep range objects attached to a buffer identity and revision. For live Windows
ranges, implement endpoint affinity and edit rebasing; for unsupported history
gaps, invalidate explicitly instead of acting on the same offsets in new text.

## Authority and accessibility callers

Accessibility is also an automation interface. An external AX/UIA caller is not
authenticated as a human merely because the OS permits it to use accessibility.
The proposed bridge must not label every action `HumanInput`, approve agent
permissions, answer protected questions, or reveal session/MCP keys by default.
`isAccessibilitySelectorAllowed:` controls allowed selectors, not an application-
specific proof that the caller is the human user.

Reuse `GuestAccess`'s private paths, protected dialogs/buffers and checked
transitions for an unattributed automation projection. Redact sensitive values
before publication, including titles, help text, range replies and notifications;
redacting only visible pixels is insufficient. Permission-setting controls and
approval actions remain unavailable through that projection. Ensure native
menus cannot become a second route around the same checks.

This creates a genuine accessibility policy decision: a human using VoiceOver
must eventually be able to inspect and operate protected dialogs too. Before
shipping that capability, choose and document a separate, explicit local
assistive-access policy and its OS-user trust boundary. Do not claim it keeps
privileged OS automation agents from doing what the screen reader can do.
Provider selector APIs alone do not solve attribution. The first implementation
should preserve current restrictions and clearly state this limitation; full
protected-dialog accessibility is a release gate requiring a reviewed policy,
not an implicit grant hidden in the adapter.

## Other platforms and the browser

| Target | Adapter and reuse | Main additional cost |
| --- | --- | --- |
| Windows | HWND-rooted UIA fragment provider; stable runtime IDs; Invoke, Value, Selection, ExpandCollapse and Scroll as applicable; Text/TextRange for source | COM lifetime/threading, selection/range rebasing, DPI geometry and Narrator qualification |
| Linux | AT-SPI2 Accessible, Component, Action, Selection, Text and EditableText interfaces over the accessibility bus | D-Bus object lifetime/events, desktop/session differences and Orca qualification |
| Browser | Real semantic HTML controls and a document/editor projection synchronized with the same model | DOM focus/input composition, text replication and browser/screen-reader differences |

Windows exposes a custom provider through `WM_GETOBJECT`; child elements form a
fragment with navigation and focus support. Implement the provider in a small
C++ adapter rather than hand-maintaining COM vtables in Haskell, and pin the
chosen COM threading model. Detach/disconnect old providers on frontend closure.
[Microsoft provider implementation](https://learn.microsoft.com/en-us/windows/win32/winauto/uiauto-serversideprovider)

TextPattern requires complete document, visible and selected ranges and text/
selection change events. Implement TextPattern2 caret support where useful.
VirtualizedItem/ItemContainer suit large tree/list controls; they are not a
substitute for offscreen document text. Do not claim a partial implementation
supports every pattern. [Microsoft text providers](https://learn.microsoft.com/en-us/windows/win32/winauto/uiauto-implementingtextandtextrange),
[control patterns](https://learn.microsoft.com/en-us/windows/win32/winauto/uiauto-controlpatternsoverview)

AT-SPI2 is a provider protocol over D-Bus. `libatspi` is primarily the client-side
API; simply linking it does not make SDL controls accessible. GTK4 supplies
`GtkAccessible` and platform contexts, while Qt supplies `QAccessibleInterface`
and specialized text/action interfaces. Either toolkit can reduce platform
plumbing if adopted as a real frontend host, but adding a hidden parallel widget
hierarchy introduces focus, layout and lifecycle synchronization costs. A whole
frontend rewrite is not the first step.
[GNOME architecture](https://gnome.pages.gitlab.gnome.org/at-spi2-core/devel-docs/architecture.html),
[GTK accessibility](https://docs.gtk.org/gtk4/section-accessibility.html),
[Qt accessible interfaces](https://doc.qt.io/qt-6/qaccessibleinterface.html)

Before writing three native adapters, run a bounded AccessKit C-bindings spike
against the same source-text, SDL-hosting and lifetime acceptance tests. It
already targets custom-rendered interfaces and has macOS, Windows and Unix
adapters. It adds a Rust-built dependency and platform integration that must be
pinned and packaged; do not assume its text semantics or large-document costs
fit without measurement. Keep this decision behind the shared projection.
[AccessKit](https://accesskit.dev/)

For the browser, prefer actual buttons, inputs and lists to an ARIA label on the
canvas. A visually unobtrusive semantic layer must remain in the accessibility
tree: `display:none`, `visibility:hidden` and `aria-hidden` are not ways to make
an accessible mirror. Avoid duplicate focusable canvas/textarea controls and
preserve native IME/selection behavior. Use live regions sparingly for statuses,
not all changing source/output text. [W3C ARIA guidance](https://www.w3.org/TR/using-aria/)

## Delivery and evidence

1. **Semantic core and transport:** pure projection/action tests, identity and
   modal-focus rules, redaction, epochs, bounded deltas and text coordinate
   contracts. Medium effort; protocol and authority decisions come first.
2. **macOS vertical slice:** attach, discover named controls, focus/activate an
   ordinary dialog, read/select/edit source, move among splits, close/reconnect.
   Medium effort for controls; high effort for full text and remote replicas.
   Compare direct AppKit with the small AccessKit spike before committing to
   multiple adapter implementations.
3. **Usable macOS coverage:** menus, file tree, diagnostics, composer, completion,
   read-only output and reviewed protected-control policy. Qualify VoiceOver
   navigation, edit feedback and announcements on real native sessions.
4. **Browser, then Windows/Linux:** reuse the core and transport; budget separate
   implementation and assistive-technology qualification for each. Linux/Windows
   are each substantial adapter projects, not build-flag-only ports.

Use `test/AccessibilityCheck.hs` in the existing suite for deterministic model
checks and a native harness following `tools/EditorDriver.hs` for fixtures.
Inspect actual AX trees with Accessibility Inspector and exercise VoiceOver;
on Windows use Inspect/Accessibility Insights plus Narrator; on Linux use
AT-SPI inspection and Orca. Include automation actions and manual screen-reader
flows: a readable tree alone does not establish usable editing.

Performance checks should hold unrelated IO open while querying focus/ranges,
edit a large document and a huge single line, and verify bounded queue/memory
behavior without flattening unchanged text. Exercise Retina changes, multi-
monitor coordinates, dialog close during an action, undo/redo, stale selection,
provider shutdown, SSH lag, reconnect and client references retained after close.
Negative authority tests must attempt approval, configuration edits, private
range reads and secret-bearing notifications through every adapter.

Acceptance requires source editing with feedback, stable focus and correct
Unicode ranges, no callback into the locked desktop, no stale action delivery,
and an explicit account of remaining text-size, terminal and protected-control
limits. Estimate calendar time only after the macOS/transport vertical slice;
text replication and authority policy dominate the uncertainty.

The macOS frontend consumes that same sidebar snapshot through a retained Cocoa
accessibility outline. Host-assigned IDs preserve row objects; replacement and
reset retire old names and links. AppKit derives screen coordinates from the
actual SDL content view and renderer cell edges. The adapter has no actions or
setters and does not override application accessibility focus. Native windows,
dialogs and document text still need their own semantic projections.
