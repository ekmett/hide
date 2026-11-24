<!-- SPDX-FileCopyrightText: 2026 Edward Kmett
SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0 -->

# Source cleanup follow-up

The module documentation pass identified these implementation concerns. Checked
items cite landed code and regression coverage; unchecked items remain review
findings, not measured performance results.

## Interaction cost

- [x] `GuestAccess.guestTransitionAllowed` uses buffer versions and stable
  constructor identities for protected state; `GuestAccessCheck` exercises
  guest input with hidden payloads that must not be forced.
- [x] Conversation question presentation uses a stable question identity;
  `ConversationCheck` covers unchanged polling with poisoned answer history and
  replacement of the answer buffer.
- [x] Terminal screen revisions retain copied native cells and prepared console
  payloads until the owner reports a changed grid. `Consoles.showConsole` adopts
  those immutable references without inspecting the previous highlighted screen.
  `ConsolesCheck` covers idle allocation/redraw, intervening output readers and
  old-screen strictness; native terminal checks cover metadata-only changes,
  appearance and resize alongside changed cells.
  Prepared rows coalesce equal-style cells; the console checks retain a frame
  allocation bound and exact scene comparisons for wide/combining glyphs and
  selected text.
- [x] Routine reconciliation tracks immutable baseline identities and change
  serials. One scoped worker compares disk bytes and prepares replacement buffers;
  adoption checks baseline, source identity and the latest observation. Duplicate
  path subscriptions share one read and update every buffer. Regression checks
  cover allocation, delayed dirty/replaced sources, worker failures, Undo and
  checked saves. Explicit conflict actions retain their checked disk reread.

Do not replace these comparisons with whole-desktop hashing or serialization.
Keep full-text checks at operations that actually need them, outside input and
render invalidation. Use the existing strictness regressions to reject accidental
payload forcing, alongside behavior checks for the changed operation.

## Provider and file contracts

- [x] `AutocompleteACP.feedbackACP` preserves cumulative accepted-character
  counts from the original normalized insertion. The provider check covers
  prefix trimming and repeated word acceptance through the next prompt's
  feedback payload.
- [x] `AgentFiles` capture, identity and write admission select the highest
  eligible buffer ID for duplicate paths. Focused checks cover unchanged
  acceptance, stale rejection and private or hex duplicate refusal with
  separately opened buffers for the same canonical file.

## Portability and API scope

- [ ] Qualify native Windows browser startup with a real display. `BrowserServer`
  uses `randomIdentity`, whose Windows implementation uses native randomness,
  and shares the platform opener with `Links` (ebb1f92). Existing browser relay
  checks disable automatic opening; they do not verify a real browser launch.
- [x] Retired the unused experimental `Hide.Help` plain Markdown API and its
  dedicated tests. Interactive F1 Help continues through the registered
  CommonMark document path, covered by `MenuCommandsCheck`, `LinksCheck` and
  the Markdown rendering checks.
