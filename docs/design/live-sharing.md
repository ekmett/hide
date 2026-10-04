# Live sharing and multiple displays

Status: design proposal, 4 October 2026. This is the intended architecture, not a
claim that peer access or simultaneous displays already work. Today a session has
one controlling frontend. The [plugin design](haskell-plugins.md),
[accessibility design](accessibility.md) and [session transport](../remote.md)
provide the starting points.

## What we want to do

Start in a terminal, move a conversation into a Metal window, and open a browser
view of the same work. Invite someone into that session to look around, suggest a
change, or work alongside us. Let them follow an edit, point at a line, talk in a
named conversation, and run a terminal when we grant that access. None of this
should require sharing a mouse pointer or surrendering the desktop.

There are two related features:

- **Another display:** another terminal, native window or browser belonging to
  the same person. It gets its own layout, focus and dimensions.
- **Another participant:** a person admitted to the session with a separate
  identity and permissions. They may have several displays of their own.

Keep *guest* for these invited humans. Agents remain agents. Both use attributed
host operations and protected-content projections, but a guest can write in their
own human chat composer; an agent cannot impersonate them there.

The host owns the session, buffers and running tools. A participant owns their
views. Following someone is a temporary relationship between views, not ownership
of their keyboard or permission to see everything they can see.

## Session, participant, display, view

| Identity | Owns | Does not imply |
| --- | --- | --- |
| Session | Shared buffers, saved baselines, services, jobs, agents, recovery | A particular frontend or network endpoint |
| Principal | Authenticated person, agent, or host service | Permission inferred from a display name |
| Membership | A principal's grants and lifetime in one session/workspace | Access to sibling sessions or worktrees |
| Display | Attached frontend, dimensions, appearance, focus, layout, clipboard route | New buffer contents or new authority |
| View | A buffer/plugin presentation, caret, selection, scroll and expansion | Ownership of the buffer or process shown |

A person can have two displays without appearing twice in the people list. An
agent remains identifiable through its provider incarnation, parent and workspace.
Connections have their own generations, so reconnecting cannot revive an old
approval, input grab or half-delivered operation.

The host issues these identities. Neither a WebSocket message nor tool JSON can
assert `owner`, `human`, an agent ID, or an arbitrary workspace. Public session IDs
locate work; they are not credentials. The existing broad `HumanInput`/`GuestInput`
distinction must become an attributed context before peer input is admitted.

Split the current desktop ownership at this boundary. Buffer/service state stays
shared; focus, selected windows, modal presentation and geometry become display
state. Buffer operations take an explicit buffer reference and content version.
View operations also carry display/view identity. A command never falls back to
whatever happens to be focused on the host's display.

## Keep the UI small

Use the existing sidebar and window menus rather than another persistent toolbar.
**People** is a stacked tree alongside Files, Agents and Sessions. A person expands
to their shared activity and displays; private view names are omitted. Their menu
provides Follow, Chat, Ping, Permissions and Disconnect as permitted. A participant
may change their display name, but cannot replace the authenticated identity shown
in its details or impersonate a host/agent badge.

A window's **Move to…** menu lists this person's displays and New native window,
New browser view or New terminal display. Moving is the ordinary action.
**Open another view** is explicit when two presentations of one buffer are useful.
Resize, scrolling and window tiling affect only the destination display. Unsaved
state is shared and discoverable even if its last view closes.

Closing a display detaches it. **Leave session** disconnects a guest's membership;
**End session** belongs to the host and negotiates dirty buffers and running jobs.
Closing the last host display can leave the daemon running. Guests cannot kill the
session by closing their browser. Agents prepare new views hidden; presenting a
new external display requires a human request or an explicit presentation grant.

Follow tracks a chosen person's shared source view, caret and viewport, rendered
at the follower's own dimensions. It does not mirror secret dialogs. Show
“Following Alice” and an obvious Stop following action. Deliberate local navigation
or editing stops following. Updates are coalesced; reconnect does not silently
restart follow. If the target becomes private, show “Private view” without its
path, title, line number or a snapshot of the previous contents.

Remote carets have a short name as well as a color. Selection decorations never
become file content or copied text. A ping carries a buffer reference, revision
and anchored range plus an optional short message. It creates a dismissible
notification; clicking jumps there. It never steals focus. If the range was
deleted, say so instead of guessing a new location. Pings to hidden resources
reveal neither a preview nor the resource name.

Human chat reuses conversation rendering, Markdown, selection and code-location
links, with names on messages when the speaker changes. Group chat is a distinct
conversation resource with explicit membership. Unsent text and question/approval
drafts belong to their author. Pasting source into chat is an explicit disclosure
to that conversation's audience; later source revocation cannot retract a message
already delivered. A quoted code location is resolved against the recipient's
current read rights, not the sender's. New participants receive history only for
conversations explicitly shared with them.

## Permissions and one privacy rule

Permissions are grants to a membership, scoped to workspace and resource where
appropriate. Presets make admission easy, but the host can inspect individual
rights. Missing rights deny; requests can be presented asynchronously to the host.

| Right | Suggested initial guest setting | Scope |
| --- | --- | --- |
| Read and navigate | Allow shared project resources | Buffers, tree metadata, search, diagnostics, views |
| Chat, ping, suggest | Allow | Shared conversation and readable code |
| Edit buffer | Deny until granted | Selected buffers or shared workspace |
| Save to disk | Separate from editing | Checked paths and current buffer version |
| Create, rename, delete | Deny until granted | Workspace filesystem operations |
| Build, run, test | Deny until granted | Named target/job; execution trust applies |
| Terminal output | Deny until a terminal is shared | Specific PTY and its output history |
| Terminal input/start/stop | Deny until granted | Specific PTY or process creation |
| Debug inspect/control | Separate grants, initially deny | Specific debug session; evaluation may execute |
| Use an agent | Deny until granted | Specific agent/conversation and delegated budget |
| Manage membership/policy, credentials, end session | Host only | No guest/agent self-approval |

Arbitrary commands are not implied by navigation or buffer editing. Plugin
commands declare concrete effects; admission and the owning services check the
same rights. Unknown commands are unavailable to peers until their effects are
declared. Menu enablement is informative, never the authority check.

**A non-owner receives only the resource projection permitted by both membership
and the common protected-content policy.** This is the same rule for structured
reads, file lists, searches, Git views, diagnostics, history/diffs, cells, images,
canvas assets, accessibility nodes and clipboard/download results. It is applied
on the host before serialization. Masking pixels in a browser is insufficient.

The canonical policy owns protected authority paths, credentials/session keys,
private composers, permission controls and resource-specific visibility. Its path
classification uses canonical resource provenance, including symlinks and
adapter-generated documents. Extend the [shared protected-content policy](../agent-tools.md#protected-content-policy),
not a separate guest-only list. Peer views always use this projection,
even if the host turns Streamer mode off. Turning it on/off changes the host's
presentation, never another principal's rights. A hidden buffer stays hidden in
all of its views. Ordinary private resources can be explicitly shared; authority
files and credentials cannot be unprotected by a peer or agent.

A permissions dialog shows the selected person and effective grants. Changes
come from an authenticated host control, never from a peer-edited project TOML.
Global configuration can supply admission defaults; a project can narrow them,
not silently enable sharing or widen another principal's authority. Disconnect
and Revoke are distinct: the latter also invalidates reconnect credentials.

Inspection badges must name their audience: **Agents can inspect** and **Shared
with Alice** are different facts. Prefer Material `robot-outline` / `robot-off-outline`
for agent access, with a labeled text fallback such as `A+` / `A-`; use a named
People indicator for human sharing. Do not overload an eye or padlock to mean
both visibility and editability. Effective restrictions appear in the tooltip/menu;
unknown or disconnected is a separate state rather than a misleading denial.

Visibility labels and write authority are independent. A guest's own composer can
be readable/editable by that guest while hidden from other guests and agents.
Private agent conversations are not automatically shared because their window is
visible on the host. Share a public transcript projection deliberately; never
share provider credentials or the ability to answer the host's approvals.

Recheck live membership, policy generation and resource provenance before
publishing delayed work or committing edits. Revocation invalidates queued output,
frame dictionaries, asset handles, subscriptions and continuation grants. Send a
fresh filtered snapshot after discarding the old presentation base; never compress
a private frame against a guest's public frame. Cache keys include the audience
and policy generation. Revocation prevents further delivery; it cannot erase
content already received, copied or photographed.

An unrestricted terminal, arbitrary build script, debug expression evaluation or
native plugin can execute as the host OS account. Those grants cross the editor's
confidentiality boundary: code can read files without using editor tools. Even a
buffer-only collaborator can plant code that runs when the host later builds it.
The approval UI must distinguish trusted execution from read/edit sharing. For
untrusted execution, use a separate OS account/container with a deliberate mount
and credential policy. A worktree is useful isolation of work, not a sandbox.

## Authentication and reachability

Authentication answers who connected. Transport answers how they reached the
host. A session grant answers what they may do. Do not make one stand in for the
others.

Start with a dedicated peer endpoint bound to loopback, reached through an
explicit SSH forward or private network. The existing owner/daemon attach and MCP
inspection endpoints remain private; do not expose them through the sharing URL.
The peer endpoint cannot start an arbitrary daemon, supply startup arguments,
claim owner attachment or name another workspace.

Admission without an identity provider can use a short-lived, single-use random
invitation, followed by host acceptance. An invite permits a join request, not
editing or execution. Bind the pending request to its requesting client before
the host approves it. Display a verification code on both ends for out-of-band
confirmation; a claimed name is explicitly unverified. Issue a separate revocable
membership credential after approval. Concurrent redemption has one winner;
replaying a redeemed/expired invitation fails. Limit pending requests and rate.

For repeat collaborators, support an optional OpenID Connect identity provider.
Use the provider's issuer and subject as identity, not an editable name or email.
OIDC supplies authentication on top of OAuth; hide grants remain local to the
session. Use authorization code flow with PKCE, validated issuer/audience/nonce,
exact redirect URIs and an established implementation rather than a home-grown
OAuth server. Native clients use the system browser. This remains optional: no
hosted hide account is required for an SSH/private-network collaboration.
See [OIDC Core](https://openid.net/specs/openid-connect-core-1_0.html) and
[OAuth security practice, RFC 9700](https://www.rfc-editor.org/rfc/rfc9700.html).

| Route | Benefit | Decision |
| --- | --- | --- |
| SSH forward / private network | Reuses existing reachability and account administration | First deployment; hide still authenticates membership |
| HTTPS reverse proxy / tunnel | Browser joins without an SSH account; can traverse NAT | Supported deployment after endpoint admission is complete |
| Dedicated relay with outbound connections | Convenient public discovery and host connectivity | Later, only if existing tunnels do not meet the need |
| Peer-to-peer WebRTC | Potential direct path | Defer: still needs signaling, identity and often a relay |

For example, [Tailscale Serve](https://tailscale.com/docs/features/tailscale-serve)
is private to a tailnet, while [Funnel](https://tailscale.com/docs/features/tailscale-funnel)
exposes a service to the public internet. Neither replaces hide's grants. Do not
start a public tunnel automatically. Show the endpoint and its audience when the
host enables sharing, and provide one Stop sharing action that revokes memberships
and closes the listener/tunnel owned by this session.

HTTPS/WSS protects transport to its TLS termination point. A proxy that terminates
TLS and serves our browser client is trusted with that content; do not advertise
end-to-end encryption through it. An untrusted relay would need an authenticated
encryption protocol and a trusted client-delivery path, a separate reviewed design.
Invites and credentials must not enter URLs logged by the server, screenshots or
public session metadata. A browser invite can arrive in the URL fragment, be
exchanged once and removed from history. Serve no third-party scripts. Use secure
session cookies, strict Origin/Host checks for WebSockets, and CSRF protection for
state-changing HTTP routes. Peer identity supplied by a proxy is trusted only
through a configured authenticated proxy boundary, never arbitrary headers.

## Wire protocol and display ownership

Reuse the cell/style/asset transport and frame compression, extended with explicit
attachment, view and audience identities. Multi-display must not broadcast the
host's rendered desktop and then try to remove secrets at the client.

The host prepares a presentation for each display from shared immutable resources
and that display's layout. Small revision keys identify content, geometry, policy
and presentation changes. Never compare whole desktops, buffer contents or undo
histories. Shared preparation can be cached when both content and audience match.
Each connection has its own acknowledged frame base, dimensions and bounded queue.
A slow browser cannot stall the terminal or hold the desktop lock. Coalesce motion,
presence and replaceable snapshots; preserve accepted operations and their receipts.

Input carries attachment generation, sequence, originating view and relevant
layout/content version. An old click cannot activate a new widget at the same
cell. Drag capture belongs to one participant/display until release or disconnect;
it does not cross another person's menus. Semantic actions use the plugin command
registry and live resource checks. Raw key/click input passes the same attribution
and effect admission, so clicking Run is no escape from a denied run command.

Clipboard, download, open-URL and native-dialog effects go only to the requesting
display and require its applicable local consent. They never write another
participant's clipboard or launch a browser on the host by accident. Such effects
are not replayed on reconnect. A browser/native guest does not receive server-local
paths where a scoped resource identifier suffices.

A disconnected display becomes visibly offline and stops sending commands.
Reattach authenticates anew against a still-live membership and returns a filtered
snapshot. Requests have bounded deduplication records and terminal receipts. If a
receipt has expired, report an unknown outcome and reconcile; do not rerun a shell
command because an acknowledgement was lost. Following, input grabs, invitations
and approvals do not survive restart. Session recovery retains accepted edits and
attribution; reconnect credentials and saved policy use private host storage.

## Editing together and Undo

Use the host's immutable finger-tree buffers and ordered commit service as the
single source of truth. Each edit carries actor, operation ID, buffer incarnation,
base revision and character ranges; byte buffers use explicit byte ranges. Cell
columns and UTF-16 positions are converted at the edge, not mixed into this model.
Carets, pings and selections use host-managed anchors with insertion affinity.

Do not replay stale keypresses against a newer host caret. For the first editable
slice, use an explicit **writer lease per buffer**, with a visible holder and
Request editing / Release actions. Different participants can edit different
buffers at once. Handoff establishes the current revision before typing resumes.
Each lease names its holder and a host-issued generation. Every mutation carries
that generation and rechecks it at commit, even if content has not changed: an
A → B → A handoff must not revive A's earlier queued edits. Release, disconnect,
revocation and handoff invalidate the generation. The host can reclaim a lease;
a disconnected or expired membership cannot leave an orphaned editing lock.
All mutation routes, including agents, formatting and reload, respect the lease
or negotiate a checked handoff. Keep unaccepted typing as a local recoverable
draft on disconnect or lost lease, never silently discard or resubmit it.

Simultaneous writers in the same buffer are a subsequent slice: bounded ordered
text operations, tested transformation over intervening edits and selective Undo.
Do not add a general offline CRDT merely to get multiple displays. If this slice
cannot safely transform an overlap or its history has expired, retain the proposal
and show a conflict. Generic plugin/agent prepared diffs stay strict-version
transactions; they are not silently rebased as though they were human typing.

Undo is attributed. It must not rewind a shared buffer to an old whole-tree snapshot
and erase another person's intervening work. Initially permit ordinary Undo only
within an uninterrupted history segment owned by the caller; across a handoff,
show a checked inverse diff for review. The concurrent-writer slice must implement
selective inversion before advertising normal per-person Undo. The host retains
an explicit review/revert operation for others' changes. Save checks disk conflicts
and its own permission; granting edit does not silently grant file replacement.

A read-only participant can submit a versioned suggestion. The host sees its author
and diff and applies it through the existing prepared-edit/Undo path. Acceptance
reports the actual commit, including any host modifications, to the suggester.

## Terminals, debugging and agent work

A terminal process belongs to the session, with explicit output subscribers and
an input/resize controller. Multiple read views do not fight over PTY dimensions.
Granting terminal input shows the controller; requesting control is separate from
following output. Disconnect releases control, not the process. Sharing output initially includes the current screen and retained history; the
grant dialog says so. Start a new shared terminal when existing output should not
be disclosed. Do not offer “from now onward” by filtering byte offsets alone:
existing screen cells can retain older text and leak through later snapshots,
scrollback, copy or structured reads. Such an option would require an independently
initialized post-grant terminal projection across all those surfaces. Revocation
stops input immediately; stopping already-running work is a separate visible action.

Moving an embedded terminal into a native/browser/terminal display retains the
same PTY and process identity. Under tmux/screen, launch a hide display attached to
that view. Launching the program directly in an external multiplexer is a separate
External action: it is outside hide's PTY ownership and unavailable for agent or
peer inspection until an explicit adapter exists. Do not label it private when
its real state is “Not connected.”

Debug sessions, build jobs and agents remain scoped to their workspace. Guests can
follow a shared stopped frame or terminal without controlling execution. Continue,
step, breakpoint changes and evaluation carry explicit rights and debug epochs.
A participant cannot accidentally continue another worktree's debugger because
its window happened to be selected elsewhere.

Using an agent is delegated work: record the initiating person, agent identity,
workspace and authority ceiling. Effective rights are the intersection of the
initiator's grant and the agent's configured policy, rechecked on delayed actions.
A guest cannot ask a more powerful existing agent to act as the host. Initially
restrict guest prompting to agents created under that guest's delegation; a shared
host agent may expose a read-only transcript or suggestions for host review.
Conversation rendering and named chat can be reused without routing human messages
through ACP or pretending humans are agent providers. Permission answers are bound
to the authorized person and request incarnation, never inferred from chat text.

## Plugin integration

The host owns membership/authentication, authority checks, the participant registry,
view placement, buffer commits and resource lifetimes. A live-sharing plugin uses
those services to contribute the People tree, conversations, suggestions, presence,
Follow/Ping and sharing menus. It does not get a mutable Desktop or issue human
contexts. Native Haskell plugins remain trusted installed code, not a sandbox.

Extend the proposed opaque `CallContext` with authenticated principal, membership,
display/view where applicable, delegation chain, policy generation and invocation
lifetime. Expose safe inspection, not constructors. A plugin command's typed codec
and effect declaration serve menu actions, semantic input and agent tools. Tool
exposure remains explicit; registering a command does not automatically publish it.

Window content state can be shared, while a view instance owns selection, scroll
and composer draft. Plugins declare which resources/presentation are shareable and
provide audience-filtered semantics. Unknown custom windows/canvas assets default
to private, not raw screenshot fallback. The same retained widget tree feeds
rendering, copy, accessibility and input policy. Protection is applied before cell,
semantic, canvas and text payloads leave the host, including offscreen queries.

Reuse the plugin task scopes and event channel for presence and invitation work;
do not introduce another task queue or process manager. AgentHub keeps provider,
ancestry and sub-agent lifetimes. Membership and display discovery belong to the
host and can be presented by both the collaboration plugin and Sessions sidebar.

## Delivery order and proof

1. **Multiple displays for one principal.** Extract view state from session state;
   attach two backends concurrently; move/duplicate a view; retain owned PTYs.
2. **Protected peer viewing.** Canonical policy, authenticated admission, revocation,
   independent browsing, participant names, read-only follow and ping.
3. **Conversation and suggestions.** Named group chat, private drafts, versioned
   suggestions, host review and attributed receipts.
4. **Granted editing and tools.** Writer leases, safe Undo boundary, save/filesystem
   rights, terminal controller, explicit build/debug/agent delegation.
5. **Public deployment and concurrent typing.** Harden and document the proxy/OIDC
   deployment; independently deliver same-buffer transformation/selective Undo.
   Neither is a reason to block useful private-network collaboration.

Each slice must have a useful end-to-end workflow. Before admitting a peer, prove:

- Two displays at different sizes never change each other's focus, carets or modal
  answers; a host approval is absent from peer input targets and output payloads.
- Protected fixture bytes are absent from serialized frames, compression bases,
  semantic trees, assets, titles, search, diagnostics, diffs and download replies.
  Check bytes and structured output, not only screenshots.
- Revocation racing a prepared read/edit, queued frame, reconnect or delegated tool
  cannot publish or commit after its authority is withdrawn.
- Duplicate input, stale view IDs and dropped acknowledgements do not duplicate an
  edit/process launch or retarget a command. Slow peers cannot delay host typing.
- A writer handoff preserves the other person's changes, unsent drafts and explicit
  Undo boundaries. A → B → A without content changes rejects the first lease's
  delayed mutations; disconnect releases the lease without losing its draft. Later transformation tests cover overlapping edits, Unicode,
  deleted anchors and independent inverses before concurrent writers are enabled.
- A peer cannot grant itself tools through menus, project configuration, crafted
  protocol fields, agent messages or restored approval state.
- A terminal-output grant clearly includes existing visible/retained content.
  If a future “from now” mode is added, pre-grant fixture text still on screen is
  absent from every subsequent snapshot, read and copy, not only the byte log.
- Closing one display, disconnecting a guest and ending the session have distinct,
  tested effects on buffers, agents, terminals and recoverable work.

No new sharing listener, cloud account, tunnel or peer authority is enabled by this
document. It fixes the ownership boundaries and delivery order before implementation.
