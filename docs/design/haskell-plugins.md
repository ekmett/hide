# Haskell plugin API

Status: design proposal with implemented command registration and immutable
buffer read and strict-diff slices. `Hide.Plugin.Command` and `Hide.Plugin.Buffer` document those
current APIs; the signatures below sketch the full proposed contracts and are
not a compilable SDK. The approved
[sidebar design](../plans/sidebar-navigation.md) supplies the navigation model.

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
canvas resources and a semantic widget tree from the host. Canvas rendering is a planned extension,
tracked separately from the first command/widget implementation.

## Packaging and activation

Start with ordinary Cabal packages linked into the editor executable. A small
`hide-plugin-api` package would expose the public types without SDL, Ghostty or
private `Hide.Model` constructors. A configured application chooses its packages
at build time; TOML enables and configures the plugins that are present.

```haskell
data Plugin = Plugin
  { manifest :: Manifest
  , activate :: PluginM ()
  }

-- PluginM carries this plugin instance's registration/resource scope.
-- The host creates and closes that scope; plugins do not create their own root.
data Manifest = Manifest
  { pluginId         :: PluginId
  , pluginVersion    :: Version
  , dependencies     :: [PluginDependency]
  , configuration    :: ConfigSchema
  }

main :: IO ()
main = runHide [filesPlugin, debuggerPlugin, agentsPlugin, myPlugin]
```

Dependencies here order activation of already-linked packages; this is not a
second package solver. Duplicate IDs, missing dependencies and cycles are startup
errors with the affected plugin named. Activation failures withdraw partial
registrations and release acquired resources.

Do not begin with GHC runtime linking, `hint`, downloaded object code or hot code
unloading. Cabal/GHC check the linked build. Change the source API and its consumers
together while the design develops; do not add compatibility negotiation for
hypothetical older plugins. Runtime replacement can come later if it earns its
complexity.

Native Haskell plugins are trusted code. `IO`, FFI and shared process memory mean
this is not a sandbox, even if a convenience API hides `liftIO`. Host policy still
matters for attributed agent operations and accidental authority escalation; it
cannot restrain a malicious installed plugin. Untrusted plugins would require a
separate process and a different, narrower protocol.

## Commands, menus and bindings

A named command is the common route from keys, menus, tree nodes and tools into
an operation. Use namespaced IDs such as `hide.debug.add-watch` or
`example.outline.rebuild`; labels and shortcuts are not identities.

```haskell
data Command a b                 -- opaque typed registered handle
data CallContext                 -- opaque, host-issued invocation context
data PluginError

data CommandDef a b = CommandDef
  { commandId       :: CommandId
  , title           :: Text
  , description     :: Text
  , arguments       :: Codec a
  , result          :: Codec b
  , availability    :: ContextPredicate
  , execute         :: CallContext -> a -> PluginM (Either PluginError b)
  }

registerCommand :: CommandDef a b -> PluginM (Command a b)
invoke :: CallContext -> Command a b -> a -> PluginM (Task b)
```

Handlers run outside the desktop lock. Host operations within a handler may submit
a short checked state transition; awaiting another command, IO or an approval
never holds that lock. Awaiting one's own serialized task is rejected instead of
deadlocking the dispatcher.

`TaskError` distinguishes command rejection (`PluginError`), cancellation and
worker failure; `awaitTask` returns these without treating a denial as success.

`ContextPredicate` is a small host-interpreted predicate over prepared context:
focused view kind, text/byte buffer, selection, stopped debugger, provider ready,
and so on. It is not arbitrary plugin IO invoked every time a menu is painted.
Plugins may publish namespaced prepared facts for their own availability tests.
Enablement is a UI hint, not authorization: execution checks the current state.

Typed arguments stay typed inside Haskell. The explicit codec supplies validation
and the external schema for configured actions or tool calls. Do not assume that
an arbitrary `FromJSON` instance supplies an accurate JSON Schema.

The following plugin-level signatures remain proposed, including binding
registration. Implemented host menu APIs are documented in `Hide.Plugin.Menu`.
Menus contribute entries to named slots and groups:

```haskell
contributeMenu :: MenuSlot -> MenuContribution -> PluginM Registration
bindDefault   :: BindingContext -> KeyChord -> Action -> PluginM Registration

-- Examples of slots: Menu "tools" / Group "agents", SourceContext,
-- WindowContext, and a particular sidebar root's context menu.
-- Action existentially packages a Command a b with validated arguments a.
```

The host combines contributions deterministically by group, order and ID. A plugin
can add a named top-level menu, but cannot replace another plugin's entries by
returning an entire menu. Duplicate IDs are rejected. Missing optional anchors
fall back to the declared group with a diagnostic, not a disappearing command.

A context-menu contribution receives a frozen, bounded hit context: window and
buffer identities, source revision, clicked position and any selected range.
Its action retains those identities. It must not rediscover whichever file is
focused when the user eventually chooses the item. Expensive context enrichment
runs separately; opening a menu does not start an HLS request synchronously.

The proposed command registry will also supply configurable binding defaults.
Current bindings compile against the built-in catalogue; arbitrary contributed
commands do not yet have configurable registration. Use the implemented
[keybinding schema](../configuration.md#keybindings) for current platform/context
tables and command IDs. Defaults are overridden by TOML; an empty binding list
unbinds a named command.
Menus, help/status hints and native macOS menus show the effective binding, using
the frontend's notation. Native menu events use command IDs with a registry epoch,
not positions in a compiled list. A stale event cannot invoke a newly reused item.

## Buffers and checked edits

Expose buffer references and read snapshots, not `Buffer` constructors,
`Desktop -> Desktop` functions or direct writes to an `IORef Desktop`.

```haskell
data BufferRef                   -- includes session/instance identity
data BufferRead                  -- immutable content reference, no Eq or Show

data ContentVersion              -- opaque host-issued identity, not just an Int
newtype CharOffset = CharOffset Int
newtype ByteOffset = ByteOffset Int
newtype LineNumber = LineNumber Int   -- zero based

data TextRange = TextRange CharOffset CharOffset  -- half open

data BufferInfo = BufferInfo
  { bufferRef      :: BufferRef
  , contentVersion :: ContentVersion
  , displayName    :: Text
  , path           :: Maybe FilePath
  , representation :: BufferRepresentation
  , dirty          :: Bool
  , lineCount      :: Int
  , addedLines     :: Int
  , deletedLines   :: Int
  }

listBuffers   :: CallContext -> PluginM [BufferInfo]
captureBuffer :: CallContext -> BufferRef -> PluginM (Either PluginError BufferRead)
readLines     :: BufferRead -> LineNumber -> Int -> Either RangeError Text
readText      :: BufferRead -> TextRange -> Either RangeError Text
readBytes     :: BufferRead -> ByteRange -> Either RangeError ByteString

prepareEdits  :: BufferRead -> [TextEdit] -> Either EditError PreparedEdit
commitEdits   :: CallContext -> UndoLabel -> NonEmpty PreparedEdit
              -> PluginM (Either CommitError CommitReceipt)
```

`ByteRange`, `TextEdit`, `BufferRepresentation` and the error types are ordinary
validated data types, omitted from this sketch. A snapshot exposes its version
and metadata through accessors. Text APIs reject byte buffers; byte APIs use byte
offsets. Text offsets and line columns count Unicode characters, not UTF-8 bytes,
UTF-16 units or display cells. Host helpers handle grapheme-aware movement and
LSP conversion so plugins need not invent coordinate conversions.

Capturing `BufferRead` retains immutable tree content and its version without
flattening it. It is **not** today's `Hide.Buffer.BufferSnapshot`, which is a
recovery representation containing flattened saved text and histories. Capturing
content should not implicitly retain Undo. Numeric edit revision alone is not
sufficient: reload/replacement with the same number must invalidate an old edit.

Measured line/range reads share the existing finger-tree machinery. Whole-buffer
reads are explicit worker operations. Reads from a retained snapshot remain
stable; they are not live views that change underneath a parser.

Preparation validates nonoverlapping ranges and builds replacement trees on a
worker. Commit checks every buffer's current version and authority, then installs
all prepared changes atomically within **one editor session**. Failure identifies
stale/closed/private targets and installs none. It is not an atomic filesystem
transaction or a cross-session transaction. Each changed buffer gets one ordinary
undo entry with common operation attribution; coordinated workspace undo would
be a separate feature. Saving remains an explicit checked file operation.

Return a stale result rather than silently rebasing an AI edit. A plugin can
capture again and deliberately recompute. Selection-only movement should not
invalidate a content-only operation, while a completion request can additionally
guard its originating caret/view.

Subscriptions report buffer IDs, new versions and compact change ranges. They do
not broadcast full text. Host services also expose open, save, close, decorations,
selection and navigation with the same attribution and version rules. Decorations
are scoped overlays, not edits, and carry a source version or host-managed anchor.

## Custom windows

Separate a window's content from its chrome. The host supplies the title, number,
frame, focus, drag/resize/docking, scrollbars and close negotiation. A content view
need not have a fake source buffer merely to acquire a window ID.

```haskell
data WindowType args
data WindowRef

data WindowDef args state msg = WindowDef
  { windowTypeId :: WindowTypeId
  , openArgs     :: Codec args
  , traits       :: WindowTraits
  , initialise   :: CallContext -> args -> PluginM state
  , update       :: WindowEvent msg -> state -> Transition state msg
  , present      :: state -> View msg
  , persist      :: Maybe (Persistence state)
  }

registerWindow :: WindowDef args state msg -> PluginM (WindowType args)
openWindow :: CallContext -> WindowType args -> args -> OpenPlacement
           -> PluginM (Either PluginError WindowRef)
```

`WindowTraits` declares minimum cell dimensions, allowed docking edges and the
close behavior. Presentation supplies the current title and status actions; the
host remains responsible for their geometry.

A `Transition` contains new state and scoped task requests. Task completion
produces another message. Neither `update` nor `present` performs IO, and no
`Eq state` constraint is required. The owner explicitly marks its changed
presentation revision. User-created windows normally focus; background plugin or
agent work opens hidden unless presentation is explicitly requested.

Run plugin reducers and presentation preparation in their instance worker, not
under the desktop lock. Host standard widgets handle immediate cursor movement,
selection, scrolling and text entry locally, then notify the plugin. The host
adopts a prepared view with a matching instance/revision. While preparation runs,
it can still move/resize the existing surface and paint the available viewport.
Prepared views cannot overwrite newer host-widget typing, selections or scroll
positions. Widget state belongs to stable `WidgetId`s with their own incarnation
and revision, independently of plugin domain state. Adoption merges presentation;
resetting an input or moving its caret requires an explicit checked operation.
Purity alone does not make expensive plugin code cheap or enforce a time limit.

`View msg` is a declarative composition of a small standard widget set: text,
Markdown, buffer views, editors/inputs, trees, lists/tables, buttons, split layouts
and scrolling regions. IDs are stable across view updates so focus and selection
survive new data. Reuse the editor widget for real editing rather than requiring
plugins to reimplement Unicode, Undo and clipboard behavior.

A lower-level cell surface is useful for a memory viewer or profiler. It provides
prepared viewport cells **and** semantic regions: selectable text, named actions,
focus order, accessibility descriptions and sensitivity. Cells without semantics
are display-only; they do not become unrestricted agent-click targets. Plugins do
not receive raw SDL handles, DOM elements or Metal textures through this API.

The same semantics feed mouse routing, text copying, accessibility, agent screen
capture and input masks. Bubble corners or cell art are not copied as prose.
Host-issued approval controls, secret inputs and human conversation composers
carry protections that plugin layout cannot relax. A malicious trusted plugin
could still leak data in arbitrary text; semantic metadata is not automatic
information-flow security.

Closing a conversation view closes its window scope, not necessarily its provider.
That provider belongs to a session service scope. Unsaved plugin-owned documents
must participate in host close/save negotiation; a hung plugin cannot indefinitely
prevent the host from offering cancellation or forced shutdown.

## Semantic tree and accessibility transport

The prepared `View` becomes a retained tree shared by rendering, input routing,
automation and accessibility. Think of it as a small virtual DOM for the editor's
widgets, with no dependency on HTML. The host adds window chrome and modal
structure around plugin content. Do not reverse-engineer this tree from pixels or
painted cells. This work is tracked in [#13](https://github.com/ekmett/hide/issues/13);
[the accessibility plan](accessibility.md) covers platform adapters and text APIs.

A node carries its scoped identity and lifetime, parent, role, name, current
state/value, supported actions and sensitivity. Layout supplies bounds, clipping
and reading order. Text nodes refer to a buffer identity/revision and range;
selection and focus belong to the view. A logical tree item keeps its identity
when its visible row changes. Closing/reopening a dialog or recycling a widget
for a different item creates a new lifetime.

Publish the visible tree and its ancestors alongside cell frames, with explicit
insert/update/remove patches. Include changed focus/selection and logical child
counts for virtualized regions. Structural, content and layout revisions identify
what changed; a cursor blink does not invalidate the document. Never compare
whole desktops, buffer contents or undo histories to construct a patch. Prepare
payloads off the interaction thread. Coalesce complete prepared states, then
regenerate patches against the acknowledged base; do not drop arbitrary deltas
whose successors refer to them. A missing base requests a fresh snapshot.

Cells, semantic geometry and canvas surfaces commit against one layout revision.
A drag can immediately transform an existing ready surface and its semantics;
it never waits for image decoding. New content that depends on unavailable
resources uses an explicit placeholder until ready. The frontend adopts a
coherent presentation, then emits platform notifications. It must not expose a new button label at an old clickable rectangle. Actions
identify the node lifetime and any relevant text/layout revision. The host checks
current modal state, availability and caller authority again before dispatch.
An unrelated frame update must not reject an otherwise valid action.

Visible nodes are the minimum transport, not a claim of complete accessibility.
Offscreen children and text ranges remain discoverable through bounded queries
against measured buffers and provider indexes. Native synchronous text APIs may
require an asynchronously maintained local document replica. A viewport excerpt
cannot masquerade as complete document text; its limited state must be explicit
until the adapter can satisfy full-range requests. Range queries must not block
on a remote round trip from a native accessibility callback.

Map this tree into NSAccessibility on macOS, UI Automation on Windows, AT-SPI on
Linux and a semantic DOM beside WebGL in the browser. All share node identities,
focus and actions; each still needs platform text/IME, Unicode offset, geometry
and notification handling. Accessibility and agent projections share semantics
but retain caller-specific policy. Redact secrets before transmission. A platform
accessibility caller is not automatically proof of human authority; the existing
accessibility plan's assistive-access policy must be resolved before exposing
protected approvals and agent settings through those adapters.

## Canvas windows

A canvas is another window content view, tracked in
[#14](https://github.com/ekmett/hide/issues/14). A PNG viewer is the first concrete
consumer: pan and zoom an image inside ordinary editor chrome. Later plugins can
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

Start with retained image resources and a small portable scene description that
Metal, Vulkan and WebGL can execute. Send resource creation/upload/release and
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

Keep a deliberate route for plugin-defined rendering pipelines alongside that
portable path. Arbitrary Haskell callbacks on an SSH host cannot run inside a
browser, and Metal/Vulkan/WebGL do not share shader formats. Such a plugin must
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

The host also owns color space and filter placement. A canvas can request crisp
image presentation or participation in the editor's CRT effect without changing
cell rendering globally. Final screenshots composite both cell and canvas layers;
text captures use the canvas's semantic description and available actions. A
stencil is not a confidentiality boundary: apply audience/redaction policy before
sending image resources or semantics. Agent screenshots capture only the
authorized masked composite, never underlying private canvas textures. Text
terminals receive a useful named fallback with image metadata and Open Externally,
not an empty source buffer pretending to display pixels.

Deliver this after the shared widget ownership contract. The first PNG slice
includes resources, clipping, identity/lifecycle, basic image semantics and the
shared wire path. Enrich accessibility and add backend-specific rendering
extensions with concrete consumers afterward. Keep the accessibility work
independently useful for ordinary windows throughout.

## Sidebar contributions

A root is a provider in the single shared tree, not a docked plugin window.
Files, Agents, Sessions, Debug and Watches should use this same interface.

```haskell
data TreeProvider = TreeProvider
  { rootId   :: TreeRootId
  , label    :: Text
  , children :: CallContext -> NodeId -> Maybe PageToken
             -> PluginM (Either PluginError NodePage)
  }

registerTree :: TreeProvider -> PluginM TreeRef
invalidateChildren :: TreeRef -> NodeId -> PluginM ()
revealNode :: CallContext -> TreeRef -> NodePath -> RevealPolicy -> PluginM ()
```

Nodes have provider-local stable IDs, parent identity, prepared styled labels,
expandability, actions and optional context-menu contributions. IDs are scoped by
plugin and provider instance. They are not overloaded filesystem paths. Agent
nodes use agent IDs; debugger nodes additionally carry stop/frame epochs.

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
not a title painted separately above the scrolled contents. A context-menu query captures a
`NodeHit` with provider instance, stable node ID, typed target and relevant version;
a menu choice never acts on whichever row later occupies that screen position.

The concrete contributions should include:

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

`PluginM` is an execution environment with host-owned scopes. Every registration,
subscription, task, timer and process belongs to a plugin, service or window scope.
A registration can be withdrawn early; it is always withdrawn at scope shutdown.

```haskell
spawnTask :: CallContext -> TaskKey -> PluginM a -> PluginM (Task a)
cancelTask :: Task a -> PluginM ()
awaitTask  :: Task a -> PluginM (Either TaskError a)
subscribe  :: EventSelector e -> (CallContext -> e -> PluginM ())
           -> PluginM Subscription
startService :: ServiceId -> (CallContext -> PluginM ()) -> PluginM Registration
```

Activation has no human call context. `startService` supplies a host-issued
service context with the plugin's configured grants and service lifetime. Timers
and subscriptions likewise carry service/observation provenance, not new human
authority. An event caused by an agent does not upgrade it or resurrect an expired
originating invocation. Existing attributed work can continue only through an
explicit owned continuation.

Callbacks execute on supervised workers. Short UI commits pass through the host;
callbacks never run arbitrary plugin code while a desktop lock is held. Host
process/terminal/download services provide the existing cleanup and progress
machinery instead of every plugin inventing another subprocess supervisor.

Use bounded queues with explicit overflow behavior. Coalesce replaceable state
such as the latest diagnostics/tree/view snapshot. Preserve accepted command
replies and ordered transcript events; backpressure or explicit truncation is
preferable to silently dropping them. Bulk preparation must be forced on its
worker before publication—placing a lazy parse in a queue does not move the work.

An orderly disable first negotiates unsaved content with the human; cancellation
of that negotiation leaves the plugin enabled. Once teardown is accepted,
shutdown stops accepting new calls and invalidates the owner generation,
then signals tasks and closes resources outside the UI lock. Pending callers get
a terminal error; late results cannot reopen a window or re-register a command.
Cooperative Haskell cancellation cannot safely promise recovery from every hung
FFI call or process corruption. Disabling an in-process plugin removes its active
contributions; it does not unload Haskell machine code. Forced termination preserves the last
completed checkpoint and host-buffer recovery state; it cannot promise to capture
an unresponsive plugin's latest private state. Keep that limitation visible when
offering force-close.

## Agent tools and providers

Tools explicitly expose selected operations. Registering a menu command does not
make it callable by an agent.

```haskell
exposeTool :: ToolDef a b -> Command a b -> PluginM Registration

-- ToolDef declares a namespaced external name, description, policy key,
-- effects and result-size limits. Schemas come from the command codecs.
registerAgentProvider :: ProviderId -> AgentProvider -> PluginM Registration
```

A `CallContext` contains host-authenticated actor, workspace/session identity,
cancellation, invocation trace and approval state. The public API exposes safe
inspection but no constructor for a human-origin context. Invocation descendants
inherit authority; delayed commits recheck live policy and owner identity. Contexts
expire with their invocation unless the host creates an explicitly owned task
continuation. A plugin may not reuse a retained human call to service a later
agent request.

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

The provider boundary can remain small:

```haskell
data AgentProvider = AgentProvider
  { startAgent :: ProviderStart -> EventSink AgentEvent
               -> PluginM (Either PluginError AgentDriver)
  }

data AgentDriver = AgentDriver
  { sendPrompt :: AttributedPrompt -> PluginM PromptTicket
  , steer      :: Maybe (AttributedPrompt -> PluginM PromptTicket)
  , cancel     :: PromptTicket -> PluginM ()
  , stop       :: PluginM ()
  }
```

`ProviderStart` and `AttributedPrompt` come from the host hub with their scoped
context; plugins cannot reconstruct caller authority from prompt text. The driver
lives in the service scope. Its event sink carries replies, tool activity,
capability/configuration updates and usage through the bounded event channel.
A terminal delivery event resolves each accepted ticket. Cancellation is a
request with an eventual outcome, not a claim that the model has already stopped.

This follows today's `AgentHub.StartProvider` and `AgentDriver` boundary rather
than making the conversation window own transport. Model/effort choices come from provider capabilities.
Fork/resume must mean actual provider support, not an invented fresh session.
Private resume keys use a separate host credential/checkpoint service and never
appear in public descriptions or window state.

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

## Building our agent integration with this API

The first-party proof should register these contributions:

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

Conceptually the package's activation looks like this:

```haskell
agentsPlugin :: Plugin
agentsPlugin = Plugin agentsManifest $ do
  registerAgentProvider acpProviderId acpProvider
  chat <- registerWindow conversationWindow
  openChat <- registerCommand (openConversationCommand chat)
  registerTree (agentDirectory openChat)
  contributeMenu toolsAgentsSlot (conversationMenu openChat)
  bindDefault Global newConversationKey (action openChat NewConversation)
  exposeTool sendMessageTool =<< registerCommand sendMessageCommand
  void (registerDocs agentDocs)
```

This is deliberately ordinary Haskell composition, not a generated global effect
sum or a universal service locator. Shared typed services for buffers, terminals,
builds, HLS, debugger, Git and agents are host APIs with discoverable capabilities.
An optional service can be unavailable in a build. Plugin-specific collaborations
can use a normal Haskell dependency and scoped handles; add a dynamic service bus
only if a concrete need appears.

The current `ConversationState` owns build/terminal services and permission state
as well as presentation. Those owners must be separated before claiming the agent
UI is a plugin. The existing worker implementations can remain; the migration is
about ownership and public boundaries, not rewriting ACP, Ghostty or DAP.

## Cabal navigation as a second example

A Cabal plugin contributes a root named after the local package, with component
targets as children and their source files beneath. Its semantic target includes
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

Each plugin declares a versioned configuration schema under its ID. Merge global
and project settings through the existing TOML layer, preserving unknown tables
and comments. Schema validation happens before activation/reconfiguration. Do not
execute arbitrary Haskell from project configuration. Loading a project does not
automatically install or enable a new package.

Persist versioned data, never closures, `Dynamic`, stable names, pointers, threads,
protocol handles or in-flight task queues. Window checkpoints name plugin ID,
window type, state schema version and bounded data. Keep durable document data
separate from presentation state and private credentials. Providers restore inert
until an explicit supported reconnect; restored UI does not replay agent tasks.

`Persistence state` saves a bounded durable projection and restores it into
freshly initialized state. It is not a codec for all runtime `state`: task handles,
service references and buffer handles must be reacquired. Serializing this
projection runs on a worker and is invalidated by its explicit persistence
revision, not by comparing or hashing all plugin state.

A missing/disabled plugin leaves a placeholder with its type and recovery payload
retained for a later compatible plugin. Preserve unsaved content with host-owned
buffers wherever possible. Migration failure does not discard the original state.
Buffer/window IDs are session handles; restore resolves durable references before
handing fresh handles to plugins.

## Bringing it into the current code

Delivery is staged in [the plugin tracking issue](https://github.com/ekmett/hide/issues/1).
The interfaces remain proposed until their implementation PRs establish parity:

1. Put a named, typed command registry behind the current `Command`/`Effect`
   dispatch. Keep existing constructors internally while menus and configurable
   keys adopt stable IDs. Stop using native-menu list indices as identities.
2. Give the approved sidebar a generic root/node provider interface immediately,
   so Files and Debug are its first consumers rather than special cases to undo.
3. Separate buffer ownership from window content and add standard semantic views.
   Replace title-based privacy checks with explicit host-owned view/control roles.
4. Wrap existing resource owners in scoped service handles and checked buffer
   operations. Preserve the MCP initiation/deferred-wait split.
5. Move the agent directory, conversation presentation, menus and ACP registration
   through the public API as the first complete proof. Exercise primary and child
   agents through the same consumer instead of retaining a privileged primary UI.

Only after this proof should we freeze a small public `Hide.Plugin.*` surface or
publish a standalone SDK. Today the package exposes most implementation modules;
that exposure should not accidentally become the compatibility promise.

## Checks that would make the design credible

- A separate Haskell package adds a window, sidebar root, command, menu and tool
  without importing `Hide.Model`, `Hide.Render` or private conversation state.
- The same plugin works through terminal, Metal/Vulkan, browser and SSH display;
  disconnect/reconnect does not restart its service.
- A delayed edit rejects equal-revision buffer replacement, a changed buffer,
  revoked authority and a closed/recovered session, without partial installation.
- A slow or failed provider leaves source selection, sidebar scroll and window
  dragging responsive. Render invalidation cannot force plugin state or histories.
- Closing a view cancels view tasks but retains its intentionally session-owned
  agent; plugin disable resolves pending tool calls and cannot resurrect UI.
- An agent cannot use a plugin command or rebound key to approve itself, type into
  the human composer, alter agent policy or extract a private resume key.
- Missing plugins and failed checkpoint migrations preserve recoverable data.
- Our agent integration uses the documented API, including hidden child sessions,
  user-edited approval diffs and provider-specific model/effort capabilities.

## Decisions to revisit after the first proof

The recommendation is trusted, linked Haskell packages first. If installation
without rebuilding becomes essential, compare a separate-process Haskell SDK
against runtime GHC loading rather than quietly promising both. Similarly, keep
canvas implementation in its own tracked stage after widget ownership. Live
code reload remains outside the initial implementation. Neither blocks agent
hooks or the stacked sidebar; the semantic contract must already accommodate
canvas content so it does not acquire a separate accessibility/input system.
