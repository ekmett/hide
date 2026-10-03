# Source cleanup follow-up

The module documentation pass identified these implementation concerns. These
are review findings, not completed fixes or measured performance results.

## Interaction cost

- [ ] `GuestAccess.guestTransitionAllowed` compares protected documents, the
  conversation composer buffer and question state structurally. Replace payload
  equality with explicit immutable identities and small state keys while preserving
  post-transition authority checks. Verify that attempted edits to protected
  controls are refused without forcing buffer contents or undo histories.
- [ ] `Conversation.tickConversation` and `paint` compare `ChatQuestion` values,
  which contain a buffer. Give question presentation a cheap change key; verify
  unchanged polling does not inspect the answer buffer and real edits still repaint.
- [ ] `Consoles.showConsole` compares a document's complete highlighted screen to
  the next snapshot. Carry a screen revision or changed result from its owner;
  verify idle polling avoids the comparison and changed cells remain visible.
- [ ] `Reconcile.synchronize` compares file states containing disk bytes, and
  reload adoption constructs replacement buffers. Review the worker/adoption
  boundary so routine observation adoption uses baseline identities and prepared
  buffers. Preserve checked saves, undo and concurrent edit rejection.

Do not replace these comparisons with whole-desktop hashing or serialization.
Keep full-text checks at operations that actually need them, outside input and
render invalidation. Use the existing strictness regressions to reject accidental
payload forcing, alongside behavior checks for the changed operation.

## Provider and file contracts

- [ ] `AutocompleteACP.feedbackACP` clamps cumulative accepted-character feedback
  to the remaining proposal text. Check prefix trimming and repeated word
  acceptance against the original normalized insertion, and report the cumulative
  count without truncating it to the suffix length.
- [ ] `AgentFiles.sourceSnapshots` and `sourceIdentity` select the highest eligible
  buffer ID for duplicate paths, while `acceptWrite` selects the lowest. Choose
  one consistent target and verify both unchanged acceptance and stale rejection
  with two separately opened buffers for the same canonical file.

## Portability and API scope

- [ ] `BrowserServer` still assumes `/dev/urandom` and uses `xdg-open` outside
  macOS. Native Windows browser startup needs platform entropy and URL opening;
  verify capability generation and actual launch on Windows.
- [ ] `Help` exposes an older plain Markdown renderer used by its own checks but
  not interactive help. Decide whether that public API is worth retaining before
  expanding its documentation or merging it into the CommonMark renderer.
