# Inline autocomplete

Implement the requested bindings: Alt+\\ requests, Alt+[ / Alt+] browse
(Command+\\ and Command+[ / Command+] in Mac windows and browsers),
Alt+Right accepts a word, Tab accepts, Escape dismisses. A frontend that reports
bare modifier transitions may request after Alt alone has remained down for more
than one second; another key, another modifier, or focus loss cancels the timer.
Traditional terminal input retains explicit shortcuts.

A proposal is a bounded replacement against one immutable buffer snapshot and
caret. Keep it outside the buffer, render changed text in gray, and apply accepted
text through the ordinary undoable edit operation. Capture window identity,
buffer identity/revision, selection, and an input generation; stale results never
appear. Prepare nearby source and a few recent undo snippets on the owned worker.
Never compare whole desktops, buffers, or histories in the event/render loop.

Two providers share this UI:

- ACP: a separate configured side chat, given a completion-specific prompt and
  skill description on first use. Its private MCP connection exposes only the
  supplied context, a bounded current-file reader and a proposal submission tool. No file writes,
  terminals, builds, editor controls, approvals, or agent management.
- Copilot: GitHub's documented language server over stdio, with document sync,
  inline completions, shown/acceptance feedback, and human device-flow sign-in.
  The language server owns credentials; they never enter recovery or TOML.

Keep the side chat warm, report accepted/ignored/partial outcomes, and request
new alternatives explicitly. Hide it normally; an optional bottom-pane transcript
shows the same session and lets the human send intent hints with an isolated draft.

Settings belong in global/project TOML under editor.autocomplete and Options >
Autocomplete. Provider defaults to off until selected. ACP executable, argument
JSON, model and effort are separate from the main conversation. Copilot has its
own executable/argument settings and Sign in / Sign out actions. Only human UI
may configure completion providers or authenticate.

## Work

- [x] Implement shared proposal validation, display, keyboard acceptance and
      stale-result checks. Check ghost text leaves source/undo unchanged and
      acceptance/partial acceptance is undoable.
- [x] Implement isolated ACP prompt/MCP lifecycle, bounded context and proposals,
      cancellation and model/effort settings. Check hostile/late tool calls and
      unadvertised editor operations fail without changes.
- [x] Implement official Copilot transport, completion sync/feedback and auth.
      Check protocol lifecycle with a controlled server; require human sign-in
      for account-backed verification.
- [x] Integrate asynchronous workers, TOML/menu, native/browser hold gesture and
      session privacy; update docs and verify actual rendered proposals.
- [x] Run focused/integrated checks, review, commit and publish tested changes.

Verification includes controlled ACP and Copilot servers, source/undo preservation,
stale-result rejection, input routing, and actual Metal captures. Copilot account
sign-in and service-backed completion still require the user’s account.
