<!-- SPDX-FileCopyrightText: 2026 Edward Kmett
SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 -->

# Haskell plugin API

Plugins use typed commands, immutable buffer reads and checked diffs, scoped
menus and bindings, shared sidebar trees, prepared text windows and host-owned
forms and editors. The executable chooses its linked plugins in `app/Main.hs`.

Four packages build independently of the editor's private model and native
frontends:

| Package | Contents |
| --- | --- |
| `hide-plugin-api` | Scoped commands, tools, forms, input declarations, menus, trees and session composition |
| `hide-agent-api` | Provider contracts, conversation input delivery, directory metadata and attributed orchestration services |
| `hide-acp` | ACP transport, agent adapter and persistent private completion provider |
| `hide-agents` | Agents sidebar/forms, conversation menus/input and transcripts, completion tools/hints and editor tools |

Buffer/window ownership and the multiline-editor interpreter still live in the
main `hide` library. ACP completion and primary/child conversations declare their
input and acknowledged commands through the public API. The plugin contributes
both the primary/child ACP provider and private autocomplete provider. Generic
build, debug, LSP and editor operations remain explicit permission-controlled
host services.
The sections below describe the current contracts and separate further
extensions from this delivery. The [sidebar design](../plans/sidebar-navigation.md)
supplies the navigation model.

**Agents > Rename** and **Files > Rename** use single-input forms. **New Agent**
uses named Name and Task inputs. **Tools > Resume session** uses a private
Session ID input. **Options > Agents** uses private executable, JSON arguments
and JSON environment inputs when no reply is running; **Options > Agent Context** chooses Global or
Project context before opening the corresponding TOML file. **Model** and
**Effort** use choice forms for primary, child
and ACP completion agents. With a conversation focused, its title and **Tools >
Conversation model** open a two-level dropdown: choose a setting, then its value.
The title selector and Agents tree share advertised choices and configuration
dispatch.

A linked handler prepares an `InputFormSpec`, `InputsFormSpec` or `ChoiceFormSpec`
with a typed registered action. The sidebar capability embeds it in a host reply:
`formReply` requests a modal; `popupFormReply` requests finite choices anchored to
the captured conversation title. Both use the same form lifetime and submission
worker. The host owns drafts, selection, focus, geometry and input admission;
plugins do not supply a desktop callback or arbitrary popup coordinates.

Choices submit provider IDs, not labels or row numbers. Metadata refresh can
relabel or reorder the same choices while preserving the selected ID. Named
inputs submit a map keyed by field ID; their ordered IDs stay fixed for the life
of the form. Changing either schema requires a new form. Input refresh preserves
the typed drafts, selected ranges and focus. Closing and reopening creates a new
`FormRef`.

Each popup row resolves through its captured form and metadata revision to the
raw choice ID. Keyboard navigation and paging use the ordinary context-menu layout. Moving from
a setting to its values retains the original configuration receipt. Escape,
closing the window or changing focus expires unsubmitted input. A submitted choice
survives its own visual closure, but a replaced provider or changed advertisement
still rejects it. A label or a later row index cannot redirect it to another agent.

A human submission is claimed once, and its reply is adopted only while that form
and its command registration remain current. Agent configuration rechecks the
captured provider/session receipt. New-agent creation captures the workspace at
opening and checks it before starting the existing spawn worker. Changing the
workspace expires that request. Once creation commits, the new agent has its own
lifetime. Resume retains the original primary/provider/configuration receipt; a
submitted form cannot switch a replacement provider or interrupt a newly started
reply. Its remembered ID selects the saved provider and working directory only
when explicitly resumed. Reading recovery metadata starts no provider.

The checked host results support agent creation, rename, configuration, primary
conversation Open/New/Resume, provider configuration, context-file opening and
saved-file basename rename. Persistent multiline
input is described below; arbitrary widget actions remain separate work. Agent
input cannot operate these forms. The linked owner declares capture disclosure when preparing a form;
refresh cannot change it. Rename, new-agent, resume and provider forms are private.
Model/effort forms and popups expose filtered public capability labels, preserving
readable agent settings while reserving submission for the human. A readable form
is no authority to expose protected source or session keys; its owner must remove those before preparation.

`Hide.Plugin.Sidebar` connects those prepared trees and forms to the session.
Its `Sidebar c r` capability keeps invocation context and host reply types opaque
to the provider. The host supplies origin and workspace inspection, form replies
and scoped publication; domain operations use a separate typed reply injection.
The workspace is the editor's working directory, independent of the Files tree
root. `Hide.AgentUI` in `hide-agents` uses this boundary without importing the
desktop model or its sidebar interpreter. `Hide.Plugin.Session` scopes its
registrations and metadata worker; provider services retain their own session
lifetimes. Provider acquisition, retirement and final lifecycle admission remain
in the host.

The same session supplies `MenuPublisher c r` for scoped menu contributions.
`mapMenu` projects immutable invocation context and adapts replies on the menu
worker while retaining the original command registration. The host routes a
prepared form to its existing form controller; there is no second modal queue.
Conversation Open/New/Resume, provider/context settings, raw copy, Cancel and
model/effort declarations live in `hide-agents`, along with their labels and
input validation. `sessionSelectedAgent` supplies the agent captured at menu admission;
choice preparation never looks up whichever window happens to be focused later.
New selects Primary without consuming a child conversation's draft. These commands
retain their IDs, configured shortcuts and native labels, but require a live
contribution. Provider/context forms retain the original host receipt through
submission, so changing the selection or provider cannot redirect their reply.
Raw copy retains the logical conversation source and original clipboard intent;
a later copy supersedes it.

Cancel uses the session's fixed `MenuAction`, not a registered worker callback.
The host checks its live registration and human origin, then retires the current
turn's requests, questions and queued input immediately. `invokeMenu` refuses
this action: a delayed worker cannot cancel whatever turn happens to be running
when it finishes. Context and reply mappings preserve that fixed action.

## Direction

A plugin is a Haskell package that contributes named commands, views and services
to an editor session. It can open custom windows, add roots to the shared sidebar,
read immutable buffer revisions, submit checked edits, extend menus and provide
agent integrations. It does not receive the mutable desktop or reach through it
to another component's internal state.

The useful test is our own agent support: a conversation window, an Agents tree,
ACP providers, autocomplete, tools and permission requests should fit these APIs.
They should not require privileged rendering paths based on a window's title.

Keep three responsibilities distinct:

| Owner | Responsibilities |
| --- | --- |
| Host | Buffers and Undo, window geometry/focus, standard widgets, commands, task supervision, policy, session transport and recovery |
| Plugin | Domain state, providers, commands, prepared presentation and resource cleanup |
| Frontend | Terminal/native/browser input, cell and canvas presentation, and platform accessibility adapters |

Plugins run on the session host, including an SSH host or detached daemon. A
frontend disconnect does not unload them. A browser or Metal client does not need
the plugin's Haskell package: it receives cell updates, named actions, prepared
canvas resources and semantic projections from the host. Retained PNG and JPEG
image windows provide the portable canvas path; custom rendering remains a
separate extension.

## Packaging and activation

Plugins are ordinary Cabal packages linked into the executable. `app/Main.hs`
selects `Hide.AgentUI.plugin`; `Hide.Plugin.Session.Plugin` describes its scoped
activation, tools, conversation presentation and input/provider contributions.
`Session c r settings completion receipt` supplies the host capabilities. Its
context, reply and receipt parameters keep the private desktop and operation
owners out of the plugin package.

`withPlugins` nests `withPlugin` scopes in declaration order and releases them in
reverse order. Failure during a later activation unwinds earlier scopes. Each
activation owns its registrations and metadata workers, while shared agent,
terminal and debugger services retain their separate session lifetimes. There is
no manifest solver, `PluginM` task runtime or dynamic loader.

The public packages depend on typed capabilities rather than SDL, Ghostty or
private `Hide.Model` constructors. Buffer/window implementations still use the
main library's measured content and widget owners. The first-party packages
`hide-agents` and `hide-acp` demonstrate the separate-package boundary.

Native Haskell plugins are trusted code. `IO`, FFI and shared process memory mean
this is not a sandbox. Host policy protects attributed operations and prevents
accidental authority escalation; it cannot restrain a malicious installed plugin.
Untrusted plugins would require a separate process and a narrower protocol.

Change the source API and its consumers together. Cabal/GHC check the linked
build; there is no compatibility layer for hypothetical older plugins. Runtime
loading, package discovery and hot code unloading need a concrete installation
workflow before adding their complexity.

## Commands, menus and bindings

A command has a namespaced ID, typed arguments/results and explicit wire codecs.
Labels and shortcuts are presentation, not identities. `Hide.Plugin.Command`
provides the registry and its scoped handles:

```haskell
data CommandDef context a b = CommandDef
  { commandName   :: Text
  , commandTitle  :: Text
  , commandInput  :: Codec a
  , commandOutput :: Codec b
  , commandRun    :: context -> a -> IO (Either CommandError b)
  }

withRegistry :: (Registry context -> IO a) -> IO a
registerCommand :: Registry context -> CommandDef context a b
                -> IO (Either CommandError (Command context a b))
invoke :: Registry context -> Command context a b -> context -> a
       -> IO (Either CommandError b)
```

`Codec` supplies the schema, decoder and encoder. An arbitrary `FromJSON`
instance is not a schema. The host supplies invocation context; arguments cannot
manufacture an actor or human approval.

Registration and admission take a short registry lock. Codecs and handlers run
outside it on the caller's worker. Retiring a command rejects future admission;
work already admitted may finish. Delayed actions retain the original
`CommandRef`, including its registry and generation, rather than resolving a
reused name. Hosts recheck currentness before adopting a delayed reply.
`invokeJSON` validates and forces the wire result on its worker. Typed `invoke`
leaves successful values lazy, so their caller owns evaluation before publication.

Menus use `Hide.Plugin.Menu.MenuDef` and the session's `MenuPublisher`.
A definition names its slot, group, order, label, binding and typed `MenuAction`.
Entries sort by group, order and ID. Duplicate IDs and unknown slots are rejected.
`mapMenu` projects captured immutable context and adapts replies on the worker;
it retains the original command registration. Availability is a prepared host
fact, never plugin IO during painting, and it does not replace authorization.

The host queues menu publications and retirements in order. Closing the scope
releases blocked publishers with `MenusClosed`, refuses later publications and
prevents late ticks from restoring retired entries. Plugin teardown withdraws
its exact references before closing the registry.

Context menus capture the clicked window, buffer/version, position and selected
range before dispatch. A choice acts on that captured target, not whichever file
is later focused. Expensive enrichment and command execution stay off the UI
owner. Human-only commands remain human-only through keys, menus and agent input.

Runtime menu commands can be bound by their published IDs alongside builtins.
The [keybinding schema](../configuration.md#keybindings) supplies platform/context
tables and user overrides; an empty binding list unbinds a command. Menus,
help/status hints and native macOS menus show the effective binding in the
frontend's notation. Native menu events include a registry epoch, so a stale
event cannot invoke a newly reused item. There is no separate plugin binding
registration API.

## Buffers and checked edits

`Hide.Plugin.Buffer` exposes session-bound references, immutable reads and checked
diffs. Plugins do not receive `Buffer` constructors, `Desktop -> Desktop`
callbacks or a mutable desktop reference.

```haskell
listBuffers :: BufferReader -> IO (Either Text [ListedBuffer])
captureBuffer :: BufferReader -> BufferRef -> IO (Either Text CapturedRead)
readLines :: BufferRead -> LineNumber -> Int -> Either RangeError Text
readText :: BufferRead -> TextRange -> Either RangeError Text
readBytes :: BufferRead -> ByteRange -> Either RangeError ByteString
applyBufferDiffs :: BufferEditor -> [BufferDiff]
                 -> IO (Either Text [DiffResult])
```

Listing returns shallow metadata and opaque references under the existing
Permissions owner. It grants no content authority and retains no source image or
Undo. A `CapturedRead` exposes its reference, exact `ContentVersion`, metadata,
redaction state and immutable `BufferRead`. Capture retains measured tree content
without flattening it or retaining separate saved/Undo roots. Deleted provenance
leaves can remain in the tree but are invisible to live reads.

Offsets and line numbers are zero-based. Text offsets count Unicode characters,
not UTF-8 bytes, UTF-16 units or display cells. `TextRange` and `ByteRange` are
half-open; wrong representations and invalid ranges are errors. These APIs do
not silently clamp. Immutable reads remain stable after later edits or closure;
revocation cannot erase a snapshot already granted to trusted code. Agent-facing
publication still rechecks privacy at its owning boundary.

Measured reads use cached tree summaries and splits. `lineRange` finds a row's
absolute character range, excluding its trailing CR/LF, without flattening it.
The `read_buffer` and `read_window` formatters use bounded `readText` slices to
apply their 131072-character response cap before copying an oversized row.
Large reads and their evaluation belong on a worker, never on the UI owner.

Each `BufferDiff` contains a reference, exact captured version and strict unified
diff. `applyBufferDiffs` accepts 1–16 distinct open text buffers and at most 1 MiB
characters of patch text. One permission ticket covers the batch; Prompt shows
an editable diff for each target. Every source and review version must still
match before one atomic in-session commit. Equal numeric revisions from a
replacement buffer do not match an old `ContentVersion`.

Failure installs nothing. Success returns results in input order, including the
applied patch and `userModified`; each changed buffer gets ordinary Undo. Nothing
is saved, fuzzily matched or silently rebased. `applyBufferDiff` is the singleton
operation. Saving and cross-session transactions are separate operations.
Selection movement does not invalidate a content-only edit; autocomplete also
checks its originating caret and view.

The separate `hide-plugin-api` package exposes bounded tool-facing operations:
`BufferReadServices` for listing and pages, `BufferDiffServices` for the exact
admitted diff, and `WindowReadServices` for a captured prepared body. They travel
through `RequestServices`; retaining a numeric window or buffer ID does not grant
another capture or edit. `hide-agents` consumes these services without depending
on the main library's buffer implementation.

These services do not provide arbitrary prepared edits, a general buffer event
bus or mutable decorations. Add such operations for a concrete consumer, with the
same authority and lifetime rules.

## Custom windows

### Current fixed rows and readonly Details

`Hide.Plugin.Window.prepareRowsWindow` prepares a bounded selectable list above
one readonly Details pane. Each `WindowRow` contains a stable `NodeId`, a caption
and an already-prepared plain text snapshot. Explicit existing `MenuRef`s scope
row actions to that prepared window; ordinary readonly lists pass `[]`. At most
16 unique references are accepted, with command and contribution lifetime checked
again on admission/adoption. These references grant no agent authority. The worker
rejects duplicate IDs, more than 64 rows, control characters in captions, captions over 240 characters,
nested/styled Details and Details over 16384 characters. The host keeps the
selected ID across reorder/progress, and preserves or clamps the existing Details
selection and scroll. Switching rows resets that same text-area state; resize
changes only geometry. Arrows and the mouse wheel select list rows, while Tab or
clicking Details enters its normal readonly selection/copy route.

Downloads uses this fixed surface and the existing typed menu worker. Cancel
captures the exact window lifetime and job ID; the Downloads owner rechecks the
live window, human input policy and job before signaling cancellation. Progress
and selecting another row cannot retarget a captured action. Close/reopen and
registration retirement reject late actions. Closing the manager leaves transfers
running and progress cannot reopen it. Durable rows restore only a private inert
label summary; recovery does not reconnect jobs or restore actions/Details.

### Embedded editor ownership

`Hide.Plugin.Input` lets an independently linked plugin declare multiline input:
its default/alternate labels, code-input behavior, character limit, typed command,
argument adapter and `KeepInput`, `ClearInput` or `ReplaceInput` reply. The host
compiles that declaration into its existing scoped command registry and editor
binding. Both adapters run on the command worker. The limit is checked against
cached text length before materializing the submitted text.

The plugin receives bounded text and the selected slot, never the draft's Buffer,
Undo history or content-version token. The host binds each result to the original
submission. `KeepInput` consumes the acknowledgement without changing text,
selection or Undo. Clear/replace can affect only that exact version; refusal or
newer typing preserves the draft. Labels never resolve commands or grant input
authority.

The host's `Hide.Plugin.Editor` interpreter attaches persistent multiline input
to a window. It owns the draft, Undo history, caret, selection and focus separately
from the prepared body. Prepared plugin windows and conversations use this same
input path. Body refreshes preserve the draft. The following lower-level binding
API remains in the host library; plugins use the public declaration above.
An owner registers a declaration once with
`registerDeclaredInput registry declaration reply`, where `reply` injects the
exact `EditorUpdate` into its existing result type. `attachDeclaredInput` binds
that registered declaration to each draft. Registration and draft attachment have
separate lifetimes: multiple child windows share the command while each keeps its
own input and mount. The resulting attachment uses the same window owner; there
is no separate input dispatcher.

Prepare a draft under `withDraftRef` and bind its default and alternate slots to
ordinary typed commands with `editorAction`. `prepareEditor` supplies initial
text; `remountEditor` reuses retained input. Publish the body and attachment
together through `Hide.Plugin.Window.openEditorWindow`. The menu or sidebar
owner accepts both in one operation, or neither. Refreshing ordinary prepared
content cannot install new actions or reset input.

A draft outlives its visible mount. Switching conversations retains each draft;
closing and reopening a window creates a fresh mount. Withdrawing its owner
releases empty input and moves nonempty input into a private, unsaved source
window named **Recovered input.txt**. The window opens in the background, retaining
selection and Undo/Redo; the user can Save or Discard it normally. A late callback
cannot consume that recovered input. A widget has one live attachment;
opening it through another owner must not take over its state. Input requires the
current focused mount. Outstanding clipboard reads expire on a focus or target
change, even if the user switches back before the reply arrives.

Submission captures the action, target, provider and exact draft version before
worker execution. An accepted response can clear that version even while its
draft is hidden, but cannot clear newer typing or a same-text replacement.
Existing command and provider workers own execution; input preparation never
runs plugin callbacks on the UI owner. A plugin reply uses `clearEditorDraft` or
`replacementEditorDraft` with the captured submission. Refused or stale replies
preserve the draft. Two slots may invoke the same command with different typed
arguments; labels do not resolve commands. Recovery restores conversation drafts
into fresh identities and never restores callable bindings or replays submissions.
Other retained editor drafts, including hidden ones, recover as private unsaved
source windows through the same handoff. A plugin needs no checkpoint codec to
preserve its user's input.

### Prepared transcript content

Conversation retains original message sources with stable item IDs. Its visible
prepared window contains only the demanded rows and their copy, link, shell and
privacy metadata. A position names an item, Markdown block and source offset;
wrapping and bubble decoration cannot change that position. The viewport receipt
maps those positions to painted rows. Redraw compares identities and small view
keys, never transcript contents or undo history.

The existing presentation worker parses the containing item and lays out enough
blocks to fill the viewport. Earlier items are not reflowed to recover its position.
Parsed items and one width's block rows are retained lazily. Width changes preserve
the source anchor; following the end remains a separate state. A cold seek within
one large block can still require wrapping its prefix. First show opens a loading
view while the independent input attachment remains usable.

Completed stream progress can be displayed while its successor is queued.
Changes to the provider, question, expansion state or layout reject stale work;
receiving more transcript text alone does not starve the display. A readable
window permits masked observation, not input or execution. Agent reads use the
logical catalogue, so scrolling cannot silently truncate a transcript read. Links
and shell actions still capture the exact displayed body; replacement or closure
expires an unexecuted action.

Conversation copy captures logical selection endpoints and formats the selected
source on the presentation worker, including messages outside the viewport.
One message copies without a speaker prefix; crossing messages adds attribution.
A newer clipboard intent supersedes an outstanding copy, even if it copies the
same text. Provider replacement or closing its frame also expires that request;
ordinary streaming progress does not.

Switching targets retains their catalogues and drafts. Closing a conversation
retires its visible frame; reopening creates a fresh window lifetime. Recovery
stores each target's latest received logical sources, anchor and selection once,
alongside the draft and its history. Source capture is independent of painting:
closed conversations retain incoming output, and suspension captures accepted
owner output before saving. Hidden conversations need no layout during restore, and
visible conversations reflow at their recovered width. Restored controls, links,
shell actions and provider credentials remain inactive. These conversation
owners still live in the host; the linked Agents sidebar and tool package uses
the narrower public services described below.

Inline questions keep public prompt structure separate from the live answer.
The host paints the answer and selected choice only where the current viewport
has an exact question projection. Its authenticated token outlives scrolling the
input offscreen. Editing an answer does not rebuild Markdown history; submission
and approval remain host-owned. Transient question coordinates and capabilities
are excluded from recovery.

### Further window types

The current extension points are prepared text/Markdown, rows, semantic text,
retained images and host-owned embedded editors. `withWindowScope`, `openWindow`
and `refreshWindow` publish prepared content through the existing window owner.
A refresh carries the exact window reference and generation. The host owns
geometry, focus, scrolling, selection, close handling and widget drafts.

A broader layout grammar or arbitrary plugin reducer is separate work. It should
reuse these owners rather than put plugin callbacks in painting or adoption.
Prepared output needs an explicit identity/revision; comparing a whole plugin
state, desktop or buffer history is not a redraw strategy. Pure code can still
be expensive and must be evaluated on its preparation worker.

New surfaces also need semantics for selection, copying, named actions,
accessibility and privacy. Raw cells alone do not grant agent-click authority.
Approval controls, secret fields and human composers keep their host protections.
Closing a conversation view does not stop its session-owned provider.

## Semantic tree and accessibility transport

The implemented semantic foundation publishes bounded sidebar, modal and focused
source projections alongside the matching frame. Image canvases supply semantic
names and checked viewport actions. Browser and macOS consumers retain that
metadata; reset or disconnect clears it. This delivered the scope of
[#13](https://github.com/ekmett/hide/issues/13).

The current wire fields carry complete bounded snapshots when changed. Clients
retain omitted fields and replace a field when it arrives; equal content
revisions do not imply equal geometry or privacy. Sidebar identities follow their
provider/pane lifetimes. Modal IDs are snapshot coordinates, not callable
capabilities. Readable semantics never grant editing or approval authority.

Projection follows host geometry, clipping and the centralized privacy policy.
Owner metadata follows streamer mode; agent captures always mask protected
content. Focused source reads touch only the visible measured slices. The
implementation never compares whole desktops, buffers or undo histories to
construct a semantic update.

### Further semantic and platform extensions

The [accessibility plan](accessibility.md) describes broader text APIs and platform
adapters. Generic insert/update/remove node patches, offscreen range queries,
complete conversation/message/menu subtrees, Windows UI Automation and Linux
AT-SPI are extensions to the delivered foundation, not open requirements of #13.
A future general tree should preserve these contracts:

Cells, semantic geometry and canvas surfaces commit against one layout revision.
A drag can immediately transform an existing ready surface and its semantics;
it never waits for image decoding. New content that depends on unavailable
resources uses an explicit placeholder until ready. The frontend adopts a
coherent presentation, then emits platform notifications. It must not expose a new button label at an old clickable rectangle. Actions
identify the node lifetime and any relevant text/layout revision. The host checks
current modal state, availability and caller authority again before dispatch.
An unrelated frame update must not reject an otherwise valid action.

Visible nodes are the current transport, not a claim of complete accessibility.
Full offscreen children and text ranges would need bounded queries against
measured buffers and provider indexes. Native synchronous text APIs may require
an asynchronously maintained local document replica. A viewport excerpt
cannot masquerade as complete document text; its limited state must be explicit
until the adapter can satisfy full-range requests. Range queries must not block
on a remote round trip from a native accessibility callback.

Extend the existing macOS and browser consumers, and add UI Automation on
Windows and AT-SPI on Linux through the same semantics. Preserve node identities,
focus and actions; each still needs platform text/IME, Unicode offset, geometry
and notification handling. Accessibility and agent projections share semantics
but retain caller-specific policy. Redact secrets before transmission. A platform
accessibility caller is not automatically proof of human authority; the existing
accessibility plan's assistive-access policy must be resolved before exposing
protected approvals and agent settings through those adapters.

## Canvas windows

A canvas is another window content view. The portable retained image path
completed [#14](https://github.com/ekmett/hide/issues/14): open, pan and zoom PNG
and JPEG images inside ordinary editor chrome. Later plugins can
supply plots, diagrams or custom GPU content. Canvas views contribute semantic
names, descriptions and actions to the same tree; selectable regions can expose
text or structured copy behavior.

The host owns window placement, focus, input capture and the final clip/stencil.
A plugin receives content-local coordinates and a viewport/scale. Its drawing is
clipped to the actual visible region after overlapping windows, menus, dialogs
and chrome are accounted for. Resizing, moving and scrolling use the host's
layout transform; Retina device pixels are not confused with character cells.
Rendering, hit testing and accessibility share explicit conversions among grid
cells, canvas-local coordinates, frontend logical points and device pixels. Any
filter distortion must also participate in geometry conversion. Hit testing uses
that same transform and capture rules. Plugins cannot paint or accept clicks over an approval dialog by enlarging their content bounds.

Ordinary file opening chooses the representation from the file signature. A PNG or JPEG
opens in an image window through the same Files, file dialog, command-line and
drop routes as a source file. No separate viewing service is required.

The PNG/JPEG decoder prepares immutable RGBA8 on a worker through
`prepareImageWindow`, then uses `openWindow` for ordinary scoped publication.
Additional image formats belong before that boundary: recognize the encoding,
check allocation bounds, decode, apply orientation and color conversion, then
publish pixels. The canvas and transport do not need to know which codec produced
them. Large-image tiling can replace a single retained texture with a set of
texture tiles behind the same placement and clipping contract; it must preserve
bounded upload work and discard stale tiles when the view changes.
The cell compositor emits the text grid and a tiny ownership stencil in one pass:
one little-endian 16-bit value per normal character cell, with a surface slot in
the low bits and a shadow bit at the top. Wide glyph halves occupy separate cells.
An empty image scene needs no ownership array. GPU image shaders consult that
mask; there is no CPU clip-rectangle expansion or full-resolution depth buffer.

Retained image resources and a small portable scene description are shared by
Metal, Vulkan and WebGL. Send resource creation/upload/release and
scene/damage changes over the session wire protocol. Give surfaces and resources
explicit lifetimes and revisions; an unchanged image is uploaded once, not
encoded into every cell frame. Reconnect and graphics-context loss rebuild the
current scene from retained state. Bound decoded image size and GPU memory,
retire resources on view/plugin closure, and retain them until presentations and
GPU work using them have finished. Attachment/context epochs reject stale uploads
and releases. Decode and prepare off-thread; budget graphics-thread upload work
where the backend requires it. Transport resources in bounded cancellable chunks
with input and interactive presentation updates taking priority. Animated content
requests frames while active; a static canvas does not force continuous desktop
redraws.

### Future custom rendering

Plugin-defined rendering pipelines can extend the portable path when a concrete
consumer needs them; they are not part of the completed image milestone.
Arbitrary Haskell callbacks on an SSH host cannot run inside a browser, and
Metal/Vulkan/WebGL do not share shader formats. Such a plugin must
provide frontend-executable resources/commands or an explicitly installed
renderer extension for the chosen backend. The compositor renders plugin output
into a host-controlled target and applies the final stencil itself; it does not
trust plugin code to preserve graphics state or voluntarily obey the clip.
Renderer IDs select explicitly installed and locally enabled extensions; a remote
host cannot install native code by naming one. If unavailable, use the
portable scene or named fallback. Enforce scene/resource limits and schedule
custom GPU work within a frame budget; arbitrary installed callbacks still need
cooperative limits. Native renderer extensions are trusted installed code, not a
sandbox.

Color-space and per-canvas filter requests belong in that future renderer
contract, with the host controlling their composition with the editor's CRT pass.

### Image actions and capture

The portable image path includes resources, clipping, identity/lifecycle and the
shared wire path. Image semantics expose Fit Image, Actual Size, Zoom In and Zoom
Out through browser buttons and macOS accessibility actions. The host checks the
exact view/resource and current visible anchor before applying viewport changes.

Screenshots composite cells and canvases; text captures use semantic descriptions
and available actions. A stencil is not a confidentiality boundary: apply audience
policy before transmitting resources or semantics. Agent screenshots expose the
authorized masked composite, never underlying private textures. Text terminals
retain a named image fallback with metadata and Open Externally.

## Sidebar contributions

A root is a provider in the single shared tree, not a docked plugin window.
Files, Agents, Sessions, Debug and Watches use this interface.

`Hide.Plugin.Tree` registers the root and its asynchronous child loader through
an ordinary typed command:

```haskell
registerTree :: Registry context -> Text -> NodeDef context reply
             -> (context -> ChildRequest
                 -> IO (Either CommandError (NodePage context reply)))
             -> IO (Either CommandError (TreeProvider context reply))
```

The session's `Sidebar` publishes that provider and invalidates a captured
`TreeRef`/`NodeId`. A page contains at most 128 prepared nodes and an optional
bounded provider cursor. Node actions retain typed arguments and their exact
command registration.

Nodes have provider-local stable IDs, parent identity, prepared text labels/icons,
expandability, actions and optional context-menu contributions. Styling is host-owned. IDs are scoped by
plugin and provider instance. They are not overloaded filesystem paths. Agent
nodes use agent IDs; debugger nodes additionally carry stop/frame epochs.

The current sidebar host orders tree and form metadata publications through a
bounded queue. Closing its scope releases blocked publishers with an explicit
error; later publications are rejected and late ticks cannot restore providers.
Single-node invalidation shares that ordered queue and may block its publishing
worker when the queue is full; it must not run on the UI owner. The host adopts
at most four publication events per tick. No provider callback runs during adoption.

Automatic child refresh reuses the admitted load’s origin and repeats its current
privacy checks. It never turns an agent expansion into a human request. Fresh
loads require admission; changed resources and restored hints carry no old
receipt. A node with no admitted origin is invalidated without an automatic
reload. Explicit expansion can admit a new request.

Child loading is asynchronous, bounded and paged. Expanding twice shares an
in-flight request. Results match the parent's request generation before adoption;
collapse/refresh/removal cancels obsolete work. Loading and failed states are
ordinary rows. Retry does not need a modal dialog.

The host owns expansion, one selection, one scrollbar and a visible-row index.
Rendering slices the viewport; it does not walk every expanded file or variable
on every frame. Preserve the top node plus intra-row offset when updates above it
arrive. Reveal scrolls once on an explicit action—later replies must not repeatedly
pull the viewport away from the user.

The root provider uses the same API as child providers: Files is an actual root,
not a title painted separately above the scrolled contents. A context-menu query
captures its `TreeHit` path: each hit contains the provider reference, stable node
ID and publication generation. The node action retains its typed target separately.
A menu choice never acts on whichever row later occupies that screen position.

The concrete contributions include:

- Agents root: New Agent. Agent rows: Rename (current name preselected) and
  provider-advertised model/effort choices. Include the persistent ACP autocomplete
  worker with an explicit completion role, even while its conversation is hidden.
- File rows: Rename with the current basename selected for replacement. The
  filesystem service handles collision checks and open-buffer path updates;
  this is file renaming, not HLS symbol renaming.
- Cabal package roots: package name → component targets → source files. Targets
  contribute Build and, where supported, Run/Debug or Test/Benchmark actions.

Domain providers can
share a service: Debug and Watches both consume the debugger without owning two
DAP connections.

## Tasks, events and lifetime

Resource ownership follows the existing scopes. `withPlugin` owns registrations
and metadata preparation; a window scope owns its publications; session services
own agents, terminals and debuggers. A frontend detach does not close the session.
There is no second plugin scheduler or general subscription runtime.

Workers prepare and force bounded results before publication. A lazy parse in a
queue still puts parsing on its consumer. UI adoption checks small captured
identities and revisions, then installs prepared references. It does not invoke
plugin code, flatten text or compare desktops, buffers and Undo histories.

Use bounded queues with explicit overflow behavior. Replaceable metadata can
coalesce; accepted command replies and ordered transcript events must retain
their completion or explicit failure. The command registry's retirement prevents
new admission but does not cancel already admitted work. Each supplying operation
owns cancellation, pending replies and any side effects it has admitted.

Teardown invalidates registrations and publication lifetimes, then cancels/joins
owned workers outside the UI lock. Late work cannot reopen a retired view or
register a command in a closed scope. Shared services must not be stopped merely
because one plugin view closes. Human and agent origins remain attached to
operations; a timer or refresh cannot invent human authority.

In-process cancellation is not protection against arbitrary hung FFI calls or
process corruption. Recovery preserves the last completed checkpoint. It cannot
promise to capture an unresponsive plugin's latest private state.

## Agent tools and providers

Tools explicitly expose selected operations. Registering a menu command does not
make it callable by an agent.

The implemented boundary is `Hide.Plugin.Tool`: an immutable `Tools c` set scopes
explicit `Tool c` declarations over the existing typed command registry.
`withTools` rejects host-name collisions, duplicate wire/command names and invalid
metadata before exposing the set. Strict object codecs validate input on the
worker; object results are published as structured content and JSON text. The
wire limits match MCP: 1 MiB of arguments and 4 MiB of result. Scope closure
refuses retained calls, and unknown names never reach a fallback interpreter.
`ToolHints` declares read-only, destructive and open-world behavior independently.
Read-only configures default policy; none of these hints authorizes execution.

`Hide.AgentTools` in the linked `hide-agents` package registers the existing nine
orchestration tools. Its only execution context is `AgentServices`, supplied by
the host for the authenticated actor and workspace after permission admission.
The workspace guard rejects substitution before provider reservation. Every
operation still checks live Hub authority; retaining the record cannot keep an
ended agent alive. Hub ancestry, limits, tickets, worktree isolation and rollback
are unchanged. Coordination receives no human approval or provider-configuration
capability.

`Hide.DocsTools` declares the three offline documentation tools over
`DocsServices`, composed through `EditorServices.editorDocumentation`. Its
checked request types and codecs live in
`Hide.Plugin.Documentation`; the host retains corpus selection, path confinement,
native file access and read/search budgets. Help invokes the same session-scoped
read registration. List, search and read all reject deferred calls after their
owner retires, before any filesystem discovery.

`Hide.EnvironmentTools` declares environment inspection and updates through
`EditorServices.editorEnvironment`. `Hide.Plugin.Environment` supplies checked
arguments and scope choices; the host alone checks protected names, redacts
credentials and applies process/configuration changes. Human dialogs use the
same mutation owner, with no human validation switch exposed to plugins. The
entire batch validates before mutation; project settings override global ones,
and existing child processes keep their environment. Captured services reject
calls after their session registration retires.

`Hide.AgentSettingsTools` declares `agent_settings` through
`EditorServices.editorAgentSettings`. The host captures public primary metadata
and the selected directory without filesystem access or retaining the Desktop.
The scoped read resolves a disconnected build root and reads configured agent
context on the tool worker. Provider arguments, environment values and session
keys are absent; sensitive option values stay redacted. Changing the selected
child does not change the primary scope. Deferred reads reject after retirement;
already admitted reads may finish with their captured metadata. This service has
no configuration write operation.

`PluginTool` distinguishes editor, request, coordination and private completion
tools by their service contexts. Request services own fresh admission on every
invocation; ordinary editor services are supplied after admission. This is an
explicit declaration, never inferred from tool names or read-only metadata.
Child coordination receives neither kind of editor service.

The first-party `list_buffers` and `read_buffer` tools now consume
`Hide.Plugin.BufferRead`: checked requests, masked metadata, and bounded
text/byte pages. The host retains measured source trees, target identity,
permission ownership and privacy filtering. The plugin owns tool declarations
and wire presentation.

`read_window` uses `Hide.Plugin.WindowRead` through a request-bound capability.
The host captures its exact frame and prepared or logical body before dispatch;
changing focus cannot retarget it. Each invocation rechecks caller, policy,
privacy and that body identity. Projection and bounded paging run on the worker,
including the logical conversation catalogue. The plugin owns the declaration
and `window-text` reply codec; no host body or source tree crosses the API.

`Hide.QuestionTools` declares `ask_user` through
`RequestServices.requestQuestions`. `Hide.Plugin.Questions` supplies checked
create/poll requests; the host binds the authenticated primary actor and exact
provider incarnation before dispatch. Its worker capability enters the same
bounded permission queue as other editor requests. Creation returns an identified
pending result after admission. Polling does not ask for another approval, but
still respects Disable and checks the requester. Human input alone submits an
answer; neither polling nor elapsed time exposes the draft or selects a choice.
Cancellation and session retirement resolve pending admission without creating a
question, and a replacement provider cannot inherit the previous caller's result.
The host retains question state, presentation and answer delivery.

`Hide.TerminalTools` declares the five shared-terminal tools through
`RequestServices.requestTerminals`. The public terminal service supplies typed
launch requests, terminal metadata and bounded output pages; the plugin owns
argument codecs and MCP presentation. The host retains process handles, PTYs,
terminal IDs and window adoption.

Each call enters the existing permission owner. Starting a process requires
approval before preparation begins. Directory resolution and process preparation
run on a worker; the same request retains ownership until a fresh caller and
policy check admits its window. A canceled or expired request closes any prepared
terminal it has not adopted. It cannot open a late window or borrow another
request's approval. The read-only approval fields describe the exact validated
command and arguments that will run.

Listing, output, input and stop also run through the session's console owner off
the interaction thread. They use the exact session-local terminal ID, with no
implicit shell. Output offsets count bytes in the retained tail; decoding for
MCP happens on the worker. A canceled request cannot undo input already sent to
a terminal or another side effect already admitted. Closing a window and stopping
its process remain separate operations. Retained services refuse new calls after
session retirement.

`buffer_apply_diff` consumes the public `Hide.Plugin.BufferDiff` service through
`RequestServices`. The checked `ApplyDiffArguments` contains 1–16 distinct
`DiffEntry` targets with at most 1 MiB patch characters in total. All exact
content identities are captured from the same Desktop before worker dispatch.
Only that request receives the diff capability; its callback rejects changes
to target count, order, IDs or revisions. It submits the batch once to the same
host permission owner for admission, editable approval, cancellation and atomic
adoption. Results follow input order and report the human-approved patches and
`userModified`; each changed buffer gets ordinary Undo. The plugin owns the
`buffers` array request/reply codecs. Buffers, exact version grants and batch
preparation remain host-internal.

Tool declarations carry their typed service context through `PluginTool`.
`EditorTool` receives permission-admitted `EditorServices`; `RequestTool` receives
services that own fresh admission; `CoordinationTool` receives attributed
`AgentServices`; `CompletionTool` is confined to the authenticated completion
request. None can reconstruct a human capability from arguments or prompt text.
A plugin must not reuse a captured human request to service a later agent call.

The host maps tool policy into Agent Permissions and TOML. Unknown tools default
to prompting until configured; first-party defaults can be shipped explicitly.
Tool metadata is not the security check: actual buffer/process/debug services
still enforce their own permissions. Restricted tool routes must fail closed;
a missing plugin tool never falls back to unrestricted builtins.

Human approvals remain host-owned. A plugin supplies a structured request,
including a diff when applicable, and awaits the result outside the UI lock.
It cannot return an 'approved' widget event on behalf of the human. Agent origins
remain attached to semantic key/click commands, including rebound commands.
Read-only access to policy does not grant mutation of it or private session keys.

### Provider acquisition and delivery

`Plugin.pluginAgentProvider` selects one `StartAgentProvider` for primary and
child conversations. `Hide.AgentUI.plugin` supplies the ACP implementation.
`App` rejects multiple contributions and passes the selected factory to the
existing Conversation and AgentRuntime owners. Missing contributions refuse
startup; restored transcripts and drafts remain readable.

Acquisition receives a `ProviderKind`, host-minted `ProviderIdentity`, executable/
argv/environment `ProviderLaunch`, private stdio `ProviderEndpoint`s, initial
context, `ProviderHost` services and the existing `StartRequest`. The host retains
actor, workspace, capacity and resume authority. The adapter owns protocol
negotiation, process lifetime, correlation and redaction. A repeated private
session key never identifies a new acquisition as the old one.

`AgentDriver.driverDeliver` receives the host's exact `ProviderTurnId`, attributed
`HubMessage`, additional text blocks and a one-shot `ProviderSubmission`. It
returns `ProviderTurn` after the prepared prompt enters the transport queue.
That receipt separately retains the terminal result and exact-turn cancellation;
sending a prompt is not completing it. Reply preparation and transcript callbacks
run outside the correlation lock used by cancellation. Canceling an old turn cannot affect a
newer turn. The host's Hub worker awaits the retained result; primary input can
acknowledge send admission without waiting for an answer.

Encoding and byte counting run on the provider worker before admission. The
prepared enqueue atomically checks provider lifetime and consumes the submission.
Retirement prevents an uncommitted request from sending; it cannot undo a prompt
already consumed by the provider. Unknown steering ownership retires the
connection rather than replaying the draft. Startup/context delivery advances
only at the corresponding successful admission or steering acknowledgement.

Primary and child model/effort changes and human steering use the same
`AgentHub.configureAgentAt` and `steerAgentAt` reservation. The primary driver
enters the existing session mailbox with its host-issued provider identity and
private session key. Only an `injected` steering outcome consumes the submitted
draft. Cancellation, rejection and uncertain ownership retain it. Primary Cancel
retires queued input, questions and host requests synchronously before requesting
provider cancellation. Cancelling turns refuse steering until their real terminal
reply or provider retirement.

Both providers publish bounded public `DriverEvent`s to the same Hub history.
Replacing a binding retires its event sink even if the session key repeats;
capability refresh preserves the binding. Text passes through cross-chunk
redaction before publication. Public tool events omit raw arguments/results and
permission choices. Primary expanded transcript content uses the separate typed
`ProviderContent` callback; it grants no human-message constructor or editor
authority. Its exact turn boundary orders final content before terminal adoption.
Conversation retains the primary transcript and host adoption; ACP decoding and
request dispatch belong to the adapter.

### Native provider services

`ProviderHost` supplies typed permission, file and terminal services, bound to
one acquisition before startup. Each returns a `ProviderReply` over the host's
existing result cell: nonblocking poll, worker-only await and idempotent
retirement. The adapter's pump retains wire correlation and polls these receipts.
It adds no worker per terminal-exit waiter and no second host request queue.
Cancellation retires host authority immediately; the pump still refuses the
original native request IDs, including callbacks that return after cancellation.
Closing the connection retires their wire correlation as well.

Permission results select an offered option or cancel. The host retains human
origin, Review and rejection; providers cannot approve themselves. Primary
providers receive the actual optional native file/terminal services. Children
advertise neither capability and refuse native requests explicitly.

File paths are resolved and confined on a worker. An already-known literal
source binding retains its identity at initial admission, so queued path
resolution cannot accept a later edit or replacement as its original source.
Aliases bind after canonical resolution. The UI owner captures only the matching
immutable content, baseline and source receipt, then a worker reads and slices it.
The same bounded slot spans preparation through final source/privacy admission.
It never retains a Desktop, buffer map or Undo history. Private and
hex buffers remain unavailable; UTF-8, NUL and 16 MiB limits remain enforced.
Approved writes use the existing baseline check, save and Undo operation.

Native terminal creation includes argv, environment, working directory and output
retention. Output, wait, kill and release use the provider's exact owned IDs.
`Consoles` retains processes; host approval admits creation. Provider retirement
releases only its terminals. This native protocol differs from the MCP terminal
tools' paged output and uses the same underlying process owner.

### Hub and host ownership

`AgentHub.historyAgent` and `searchAgentHistory` return public `HistoryPage` and
`HistoryEvent` values from `Hide.Plugin.AgentServices`. Conversation reads typed
event indices and host-attributed actors directly; `Hide.AgentTools` encodes the
same pages for tools. The Hub retains the single history: at most 1,024 events/4 MiB per agent, with 1–100 events and 1 MiB of
encoded events per page. Exclusive cursors preserve original indices, including
through search and checkpoint recovery; reads do not consume events or message
tickets. Provider details remain extensible JSON whose publisher owns redaction.
Event fields and the private checkpoint format are unchanged.

New Agent acquisition also belongs to `AgentRuntime`: its host-only
`requestAgentCreation` admits one pending launch and returns the Hub's original
agent ID/task ticket through the existing mailbox. Conversation only adopts the
checked human form/workspace and displays completion. The runtime joins pending
acquisition before the final checkpoint; view state owns no launch worker.

Primary Hub deliveries use the existing `AgentRuntime` mailbox. An opaque
`PrimaryDelivery` carries distinct admission and terminal-result cells, bound to
its original provider and turn. Draining the mailbox grants no admission.
Cancellation before send retires the submission; an admitted prompt retains the
Hub reservation until its actual terminal result or provider retirement.
`completePrimaryDelivery` releases that original result after ordered transcript
adoption and advertises the next human turn's busy state first. Late results
cannot publish through a replaced binding.

Human input ownership, prompt-context preparation, approvals, file adoption,
terminal IDs and recovery remain host operations. Agent tools cannot manufacture
human steering or change their controlling user's model. Fork/resume require
actual provider support; a fresh session is not a substitute. Private resume
keys stay in protected host persistence, outside public descriptions and window
state.

The ACP transport and driver are linked from `hide-acp`, a separate Cabal
package in `plugins/hide-acp`. Its only Hide dependency is `hide-agent-api` in
`packages/hide-agent-api`, which exposes `Hide.Plugin.Agent`: typed launch
requests, attributed messages, public capabilities/events and a driver lifetime.
The adapter does not import the hub, Model, Render or Conversation. Capability
decoding belongs to ACP; the hub decodes its own checkpoint representation.
The linked `hide-agents` package supplies the Agents sidebar, primary and child
transcripts, Query/Steer input, completion hints, coordination, documentation,
environment, buffer and terminal tools through `hide-plugin-api`. Build/run,
debugging and other generic editor operations remain permission-controlled host
services; moving every declaration is not a first-party migration requirement.

The hub remains the owner of agent IDs, ancestry, limits, workspaces, task tickets
and message attribution. A provider plugin supplies a driver; a conversation
plugin consumes the hub. Provider tokens and actors are bound by the host, not
accepted from tool JSON. A trusted alternate orchestration plugin can use the same
services, but does not get to bypass limits through the ordinary agent API.

Buffer snapshots granted to trusted code cannot be erased by later revocation.
Agent-facing reads must recheck privacy before publication, just as current
prepared ACP responses do. Plugins implementing tools are responsible for their
output and declared resource dependencies; the host cannot infer secret origins
inside arbitrary returned JSON.

## Multiple displays and invited participants

The [live-sharing proposal](live-sharing.md) extends this ownership model to
independent terminal/native/browser displays and invited human participants.
Session resources remain shared; focus, layout, selection and private drafts belong
to views. Host-issued invocation contexts retain the authenticated participant,
delegation and display identity across plugin work. Human peers do not become
agent providers, and their input never inherits the host's authority.

The host owns admission, policy and resource lifetimes. A sharing plugin contributes
People, named conversations, suggestions, Follow and Ping through these existing
command/window/tree contracts. All peer presentation, including semantic nodes and
canvas resources, is filtered before transport by the same protected-content policy
used for agent access. These remain proposed extensions, not current SDK behavior.

## Building our agent integration with this API

The first-party integration has these responsibilities:

| Contribution | Uses |
| --- | --- |
| ACP provider | Supervised process/protocol tasks, provider events, private resume state |
| Conversation window | Markdown/transcript view, host-protected composer, selection/copy, commands |
| Agents root | Hub identities, ancestry, running state and conversation/window actions |
| Agent commands | New, resume, steer, cancel, model/effort selection and workspace reveal |
| Agent tools | Explicit typed editor/hub operations through attributed invocation contexts |
| Completion provider | Immutable source context, owned persistent side-chat and checked proposals |
| Agent documentation | Registered docs/skills corpus through the host docs service |
| Rename/model actions | Captured agent identity, hub rename and provider configuration; includes ACP completion worker |

The actual composition is selected in `app/Main.hs`:

```haskell
main :: IO ()
main = Hide.App.main [Hide.AgentUI.plugin]
```

`Hide.AgentUI.plugin` is an ordinary `Hide.Plugin.Session.Plugin` record. Its
`withPlugin` nests conversation-menu, provider-choice and Agents-tree scopes.
`pluginTools` declares the typed tool sets; the conversation/input/completion
fields select their separate prepared presentation and worker entry points.
The editor library supplies the capabilities without importing this first-party
implementation. See that record for the complete current declaration rather than
maintaining a second example activation path here.

Optional contributions can be absent. Plugin-specific collaboration uses normal
Haskell dependencies and scoped handles; a missing service is not an invitation
to discover an unrestricted replacement through a universal service locator.

`Hide.Plugin.AgentDirectory` supplies the Agents tree with typed names,
ancestry, state and advertised settings. It lives in `hide-agent-api` alongside
the provider contract. The tree imports public plugin interfaces only; the host
binds reads to the Hub and autocomplete owner. Rename reads one small directory
entry instead of serializing a conversation's status.

Directory listing and lookup do not start providers, submit prompts or change
settings. Explicit completion-choice discovery may connect the configured provider
and refresh its metadata, on its existing worker after human input admission.
Settings and completion requests retain opaque receipts supplied by the host;
the tree cannot reconstruct or reinterpret them. A metadata refresh cannot
retarget an action, and the existing host command/receipt checks still admit its
application. These are trusted linked-plugin capabilities, not an MCP endpoint
or a grant of human input authority. Retiring the tree releases its registration and metadata worker
without stopping providers.

The command, form, menu, tree and session contracts live in `hide-plugin-api`,
without the editor's buffer or rendering dependencies. The real Agents tree is
the linked `hide-agents` package. Its `Hide.AgentUI.plugin`
uses `Hide.Plugin.Session` to scope its tree, conversation menu commands and
metadata worker. `Hide.ConversationMenus` prepares its private Resume/provider
forms and readable context-scope choice through that public API. The executable selects the plugin with
`Hide.App.main [Hide.AgentUI.plugin]`; the editor library does not import its
implementation. The metadata worker publishes invalidations to the same
bounded, close-aware queue as trees and forms. No
plugin callback runs on the UI tick. The host drains bounded deltas and owns
input, geometry, forms and action admission. The same plugin declares its
orchestration tools, which the host registers with existing per-tool policy.
The plugin also contributes `Hide.AgentTranscript.presentConversation`. Its primary
reducer turns admitted messages, streamed chunks, tool updates, plans and notices
into public transcript records. The host assigns item IDs and revisions after
redaction. Each update retains a shared lazy reduction against the previous
records, so preparation processes new updates and preserves unchanged record
identities. Source capture observes only the immutable wrapper and a started
flag; it neither calls plugin code nor walks the history.

The same contribution presents bounded, authorized `AgentHistory` snapshots for
child conversations. The host's existing presentation, checkpoint and copy
workers resolve both source types, including closed conversations. Primary
sources retain their complete contents independently of the Hub's bounded history.

`Hide.Plugin.Transcript` owns the immutable record vocabulary, source capsule and
shared update reducers. The first-party plugin owns speaker roles, activity labels
and record grouping. Provider lifetime, redaction, composer submissions, approvals
and layout remain host-owned. The executable selects one conversation presenter;
the host library does not depend on the agent plugin. If the contribution is
missing, recovered conversations and drafts remain readable. New primary
submissions, sessions and questions are refused before changing their state.

`SessionServices` owns builds, compiler discovery, build settings and shared
consoles for the editor session. Conversation receives a console handle and owns
its provider's terminal IDs; retiring that provider releases those terminals,
without stopping human terminals, builds or debugger consoles. Builds can run
without a conversation mounted.

Prepared builds still re-enter the full runtime permission and currentness checks
before launch. The service interpreter sits inside conversation presentation when
that presentation is present, so service results also refresh the conversation's
layout. Session teardown joins pending acquisition cleanup before closing jobs
and consoles.

Shared MCP terminal tools and the native ACP bridge both use the session's
console owner, with their distinct protocols and admission rules. Build planning
and generic editor operations retain their host-owned interfaces. ACP, Ghostty
and DAP retain their existing workers and protocols.

### Private autocomplete provider

`Hide.AgentUI.plugin` contributes one `CompletionProvider` implemented in
`hide-acp`, plus four `CompletionTool` declarations from `hide-agents`:
`submit_completion`, `read_completion_context`, `read_completion_file` and
`read_completion_skill`. The host registers these only at the authenticated
private completion endpoint. They never enter the ordinary agent tool catalogue.
Missing contributions start no fallback ACP process. Copilot keeps its existing
provider and authentication path.

The host prepares an immutable `CompletionInput` on its completion worker: the
current file, caret, nearby numbered lines and bounded recent edits. It retains
the source identity used to reject stale responses; the plugin gets neither a
mutable buffer nor an undo tree. `CompletionContext` is the same typed snapshot
in the prompt and the context tool reply. Explicit file reads return at most
8,192 characters from that captured file and accept no other path.

The provider's existing request slot admits all four tools. Every call checks the
exact active request ID, including calls through a retained service. Hint turns,
idle providers and cancelled or completed requests expose no source snapshot.
Submission consumes one slot for up to eight whole-line replacement alternatives,
with 128 KiB of UTF-8 replacement text in total. A submission is a preview, not an
edit: only a completed provider turn can return it, and only the host can accept
it into the source buffer through ordinary Undo.

`CompletionStart` supplies the directory, private endpoint, configured model and
effort, and a host-owned launch callback. Each actual lazy acquisition freezes
its effective environment and redaction values off the UI thread. `ProviderLaunch`
carries executable, arguments and environment without shell interpretation.
The persistent side-chat, prompt/configuration serialization, feedback and
cancellation stay in the same provider owner; this boundary adds no scheduler.
Configuration receipts identify the exact client incarnation and advertisement.
A stale receipt cannot acquire or configure a replacement client. Leaving the
provider scope invalidates its tools and joins its work.

The ACP completion transcript uses a scoped prepared text window. Its existing
trace worker prepares the latest 65,536 characters after provider redaction;
owner ticks refresh only the exact installed `WindowRef`. Closing the frame
preserves its warm provider and independent hint draft, and later trace output
cannot reopen it. The hint pane is bound to that reference rather than the title.
Readable output grants no input authority. Recovery retains the transcript as an
inert private text view. Unsent hints recover through the ordinary private
**Recovered input.txt** handoff; neither recovery nor a missing plugin restores a
callable binding or sends the draft.

`Hide.CompletionInput` in `hide-agents` owns the Send hint declaration and its
acknowledged command. It accepts at most 16,384 characters, rejects blank/NUL
input, calls `Hide.Plugin.Completion`'s checked delivery service and requests
`ClearInput` only on success. The executable selects at most one completion-input
contribution. With none, the trace stays read-only.

The host compiles it into the same `PreparedEditor`, `DraftRef` and frame mount as
conversation input. Enter captures an immutable version without clearing it.
The existing completion worker checks the original provider/configuration receipt
and mount before supplying the delivery service; the plugin cannot choose another
provider. Failure, a full queue or an expired mount leaves the draft intact.
Successful delivery clears only the submitted version, including a hidden draft.
Later typing survives. Configuration changes invalidate queued submissions rather
than sending them to a replacement provider. Hints grant no agent input authority.

### Child conversation input

`Hide.ConversationInput` in `hide-agents` declares the child's **Query** and
**Steer** actions. Its command calls `Hide.Plugin.ConversationInput`'s `submitInput`
service and returns `ClearInput` only when that operation succeeds. The host binds
the service to the captured child, configuration receipt and input slot; a plugin
cannot choose a different child or turn a query into steering.

Query acknowledgement means the Hub accepted the message into its queue. Steer
acknowledgement means the active provider accepted the direction. Both use the
existing child control worker. Its prepared update clears only the submitted
draft version, even when another conversation is selected. Failure and newer
typing preserve input. Retained service callbacks expire when the command ends;
accepted calls drain before its worker retires.

The raw-input bound is 327,680 characters. Composer Markdown removes at most four
indentation characters per newline-bearing row, so this includes inputs that fit
the Hub's existing 65,536-character limit after normalization. The normalized
limit still applies at admission. Both checks run on the worker.

A missing child-input contribution leaves the transcript readable and the unsent
draft preserved, with no callable input attachment. Recovery restores content,
not authority.

### Primary conversation input

The same `Hide.ConversationInput` plugin supplies the primary **Query/Steer**
declaration. `PrimaryInputServices.submitPrimaryInput` binds each invocation to
its original input slot and provider identity. It grants no source-buffer access
and cannot redirect the operation to another conversation.

Query acknowledgement means either admission into the existing ordered query
queue, or successful submission of the ACP prompt after connection and context
preparation. Steer retains the active provider's acknowledgement. The command
runs in the existing control worker; context preparation keeps its own worker
slot. A command cannot occupy the preparation slot while waiting for that slot
to finish the same request.

Startup runs in the existing preparation worker. The host resolves the workspace
and invokes its selected provider factory; the adapter negotiates the connection
and returns a driver. A tick adopts only the original acquisition. Cancel or
replacement retires that acquisition immediately, and its cleanup worker stops
any completed driver that was never adopted. Context capture stays on its host
worker and redaction in the adapter; neither blocks interaction. The control receipt
survives the original connection startup. Once connected, an
invocation also keeps the captured model/configuration receipt; cancellation or
replacement invalidates it. The plugin returns its prepared draft update
through ordinary control adoption; later typing and hidden drafts retain the
same exact-version protection as child input. Missing input preserves the
transcript and unsent draft without a callable attachment.

Primary input retains the existing buffer-size domain; it does not acquire the
child Hub's 65,536-character message limit. Both declarations validate on their
worker before calling the captured service.

## Cabal navigation as a second example

The host's `Hide.PackageSidebar` uses the public tree interface for a root named
after the local Cabal package, with component targets and their source files
beneath. It is a second consumer of the tree API, not a separately extracted
plugin or a general public build SDK. Its semantic target includes
workspace, package, component kind/name and the selected build configuration—not
just the row label or active filename. Menus capture that target before invoking
the shared build/debug service with the currently selected THC or GHC toolchain.

Use maintained Cabal package-description parsing, and configured build metadata
when available. An existing `plan.json` identifies resolved components and their
dependencies; it is not a complete source-file index. Without a current plan,
show declared components and mark conditional/disabled/unknown status honestly.
Browsing does not silently configure or build the project. Discover files from
component metadata and source roots on a worker; mark generated sources that do
not exist yet. Shared source files resolve to the same open buffer.

Build is available for buildable targets. Run/Debug applies to executable-backed
components that the selected backend can actually launch, including supported
benchmark executables. Non-executable test/benchmark interfaces use their Cabal
driver action; libraries have no invented Run command. Capability checks belong
to the shared target service, so tree menus, tools and keybindings agree.

Changes to the Cabal file, compiler, flags or workspace invalidate the package
projection and target descriptors. Late results cannot replace a newer package
view. Retain a visible last-known tree with refresh status rather than clearing
navigation while discovery is in flight.

## Recovery and configuration

Persist bounded durable data, not closures, stable names, pointers, threads,
protocol handles or in-flight queues. Prepared windows explicitly opt into
recovery with a type/version; transient text is not checkpointed implicitly.
Conversation recovery keeps its logical text and unsent drafts separate from
private provider resume state.

A missing contribution leaves inert readable content. Restoring that content
starts no provider, replays no task and grants no authority from saved IDs.
Fresh session capabilities must be acquired through their current owners. The
host owns unsaved buffers and ordinary save/Undo behavior.

Checkpoint preparation runs on a worker and uses explicit content identities and
persistence revisions. It does not compare or hash an entire desktop to decide
whether something changed. Recovery formats can change with the implementation;
we do not maintain old schemas for hypothetical clients.

Existing configuration uses the shared global/project TOML layer and its concrete
settings owners. Project configuration does not install Haskell packages or
execute Haskell. There is no general plugin schema registry or migration runner;
introduce either only when an actual configuration workflow needs it.

## Delivery boundary

[The plugin milestone](https://github.com/ekmett/hide/issues/1) covers delivery
stages #2–#10. Commands and configurable bindings, scoped buffer services, standard
windows, stacked sidebar roots, agent/session navigation, debugger navigation and
Cabal targets are implemented. The linked `hide-agents` and `hide-acp` packages
are the first-party consumers, rather than an additional demonstration project.

[#10](https://github.com/ekmett/hide/issues/10) moves primary and child ACP
provider acquisition, permission/file/terminal requests, shared-terminal tools,
private ACP autocomplete and conversation menu declarations through the public
interfaces. Generic build, debug, LSP, Git and editor services remain
permission-controlled host operations. The host owns state, authority and
lifetimes; the plugin packages cannot import private Desktop, Render or
Conversation internals.

The semantic-tree and portable canvas milestones (#13 and #14) are independently
delivered extensions. Future layout grammars, rendering pipelines, platform
accessibility adapters, live sharing and additional provider families do not
extend the first-party milestone. They need their own concrete workflow.

## Verification

Reuse the existing checks at the changed boundary:

- Command, form, menu, tree and window checks exercise the linked contributions
  without granting access to private host state.
- Typed buffer checks cover bounded reads, immutable identities, changed/replaced
  buffers, revoked authority and atomic checked edits.
- Agent, conversation and autocomplete checks cover provider ownership, queueing,
  steering, cancellation, configuration receipts, private input and recovery.
- Frontend/transport checks cover retained presentation and detach/reconnect;
  one actual in-flight provider request completes while detached and remains
  available after reattachment.
- Allocation checks retain the prohibition on forcing whole desktop payloads or
  histories during interaction and rendering.

Record the relevant existing results in the milestone closeout. Do not create a
second integration project, an exhaustive new platform matrix or a new fixture
framework to establish the same behavior. A new failure needs a focused causal
check; unchanged evidence can be reused.

Trusted, Cabal-linked Haskell packages remain the implementation model. Dynamic
GHC loading, a separate-process SDK and live code reload are separate decisions
if a concrete installation workflow eventually needs them. The current public
interfaces remain free to improve with their first-party consumers.
