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

The foundation is a library boundary. The interactive application does not yet
advertise these orchestration tools.

## Application integration

- [ ] Bind each MCP bridge to an authenticated agent identity; reject supplied
  actor IDs and prevent cross-workspace routing.
- [ ] Connect the main conversation and child providers to the shared directory,
  global/project limits, context, and existing human permission queue.
- [ ] Give worktree agents separate editor sessions for buffers, builds,
  terminals and debugging; keep them hidden until explicitly revealed.
- [ ] Add the compact agent directory and transcript drill-down, reusing the
  current conversation renderer and Window menu.
- [ ] Persist the directory and editor-session mapping with protected recovery
  state; reconnect explicitly rather than replaying pending work.
- [ ] Publish the tools through Agent Permissions and the docs service after
  live shared/worktree isolation and recovery checks pass.

## Verification

The editor suite exercises real stdio ACP fixtures and temporary Git repositories.
Regressions cover a busy primary conversation, cancellation while provider IO is
pending, async cancellation, reconnect workspace changes, provider exits,
configuration results, and private references split across text chunks with
unmatched replies interleaved. No dependencies are added for orchestration.
