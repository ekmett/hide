---
name: debug-editor
description: Debug a running program through the Turbo Haskell editor MCP server. Use for source breakpoints, stepping, stack traces and variable inspection with THC or another DAP adapter.
---

# Debugging in the editor

The `editor` MCP server controls the live editor session. Its tools share the user's buffers and debugger. Discover `tools/list` first; this skill is also available as the `hide://debugging` resource.

1. Read `debug_status` and `list_buffers`. Reuse an active session. Otherwise use `debug_launch` for the configured THC target, or provide an `adapterConfig` file for another DAP adapter. `debug_attach` connects to a loopback adapter.
2. Wait for `ready` and inspect `stopped`. A launch or control reply with `accepted` means the command was submitted, not that execution has reached its next stop. Check status again after a short interval; report startup errors from its output rather than repeatedly launching.
3. Use the current `generation` for controls, breakpoint changes and inspections. Refresh status after continuing or stopping. Thread, frame and variable references belong to that generation; never reuse them after it changes.
4. `debug_set_breakpoints` replaces the entire breakpoint set for the selected `bufferId`. Lines start at 1. Preserve other desired breakpoints in the replacement list. Check whether the adapter reports them as verified; a source may remain pending until loaded.
5. While stopped, use `debug_inspect` to read threads, stack traces, scopes and variables. Page large results with `start` and `count`. Inspect the selected frame by default, or use a frame returned by the current stack trace. Source references can identify adapter-owned source buffers without a filesystem path.
6. Use `debug_control` to continue, step over (`next`), step in, step out, pause or disconnect. Compare the next stopped state and stack with the question being investigated.

To investigate without automatic source navigation, use `debug_present` with
`follow: false`. At the stop you want to show, request `view: "source"` with the
current `generation`, or reveal `stack`, `scopes` or `output`. Omit `follow` to
reveal once without changing mode; set it true to follow future stops. There is
one shared debugger session, so no state translation or relaunch is necessary.

Live buffers may contain unsaved changes. Breakpoints report `sourceModified`, but this does not rebuild a running program. Save/build deliberately before debugging changed code. THC source stepping may stop more than once on the same line after optimization. An empty scopes result means the adapter supplied no lexical values; do not invent them.

If an inspection times out or becomes stale, refresh status and inspect again. After an uncertain transport failure, check state before retrying a launch or execution command: it may already have run.
