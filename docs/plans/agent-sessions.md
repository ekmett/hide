# Agent sessions

Agents share a directory of named sessions. Each has a stable identity, a
working directory, a provider conversation, and a bounded history. A subagent
places its parent in the user seat; messages from other agents remain attributed
peer messages. The human can inspect, interrupt, or end any session.

Keep the main interface small: one directory for choosing agents, the existing
conversation view for their transcripts, and the Window menu for their tools.
Background work does not take focus. Worktrees keep source edits, build outputs,
terminals, and debug sessions separate from the shared project.

## Foundation

- [x] Directory, unique names, parent attribution, message tickets, bounded
  history/search, cancellation, and subtree shutdown.
- [x] Atomic active-agent and direct-child limits, with a live policy reader.
- [x] ACP model/effort selection from advertised choices; actual provider forks
  only when supported; human permission callbacks.
- [x] Persistent Git worktrees from committed source, retaining partial checkouts
  on failure and preserving uncommitted parent files.
- [x] Strict tool schemas with host-supplied actor identity and workspace.
- [x] Checkpoint serialization; recovered records stay inert until reconnected.
- [x] Normal Cabal build and test registration for all four modules.

## Application integration

Implemented surfaces (integration validation remains separate below):

- Per-agent in-memory bearer bindings for MCP. The primary's authenticated
  `editor` server exposes editor and coordination tools; child coordination
  bridges expose only the nine agent tools. Child editor services target their
  associated workspace, with no caller-supplied actor or directory override.
- Primary and child ACP lifecycles connected to the directory, limits, user
  context, and human permission queue. Model/effort choices remain advertised
  provider choices; fork requires actual provider support.
- Worktree children have separate headless editor sessions for buffers, builds,
  terminals and debugging. Shared children use their parent's session. Background
  startup does not focus a display; checkout contents survive agent shutdown.
- Tools/Window > Agents supplies directory browsing, recent read-only history,
  workspace opening, explicit recovered-child reconnect and refresh. This is not
  a live child conversation composer.
- Agent Permissions registers the nine coordination tools. User documentation
  describes discovery, task tickets, attribution, limits and workspace isolation.

Remaining work:

- [x] Protected directory/editor-session mapping persistence and recovery.
  Restore records without replaying tasks or starting providers; issue fresh
  bearer capabilities. Publish checkpoints only after owning the session lock.
- [x] Provide explicit child-provider reconnect with capability/policy checks.
- [ ] Add a live child conversation composer and interaction view, preserving
  the distinction between the human and parent-controlled user seats.
- [x] Validate shared/worktree routing, cancellation and permission cleanup,
  primary busy handoff, bearer redaction and protected recovery.

## Verification

The editor suite exercises real stdio ACP fixtures and temporary Git repositories.
Regressions cover a busy primary conversation, cancellation while provider IO is
pending, async cancellation, reconnect workspace changes, provider exits,
configuration results, and private references split across text chunks with
unmatched replies interleaved. No dependencies are added for orchestration.

The full editor suite and documentation build/check pass. A live executable
smoke test created a real worktree and separate editor process, exercised primary
and child MCP connections, rejected root-buffer access through the coordination
connection, revoked an ended agent, checked public output/checkpoints for bearer
credentials, and closed both sessions cleanly. Recovery fixtures verify stable
identities/history/workspace mappings, fresh tokens, no task replay, corrupt-file
retention and ownership-gated checkpoint writes. Reconnect checks cover current
limits and advertised load/resume support, failed-load retry and bearer revocation,
no task or transcript replay, and ending an agent while its load is pending.

Native Windows pending-inspector shutdown is verified; remaining platform work
is tracked in the [recovery plan](session-recovery.md#remaining-platform-qualification).
