# Working on hide

## Test contract

Before writing, changing or reviewing tests, fixtures, runners or CI, read
[the test contract](docs/contributing.md#test-contract). This applies throughout
the repository, including test support embedded in production modules or scripts.
Include it in delegated test work and review briefs.

A test is a repeatable build target: it must work alone, repeatedly in the same
checkout/build directory, and independently of suite order. Each invocation owns
its mutable state and resources. Declare prerequisites through the build graph
or fixture setup. Observe the operation's own result or acknowledgement.

Needing to delete a directory "to make room", clear old status, drain unrelated
messages, run another test first or clean the build before a test can pass is a
bug. Fix the test's ownership, the fixture protocol or the missing build dependency;
never document cleanup, retries, sleeps or test ordering as the solution. Teardown
of resources allocated by that invocation is normal; deleting shared/pre-existing
state to establish a passing starting point is not.

Review test changes against these rules. For changes to stateful tests or their
infrastructure, record a focused standalone run and a repeat with no intervening
cleanup. When ordering is under test, control it explicitly and check the relevant
orders. A green full-suite run does not excuse hidden dependencies. Keep this
verification focused; do not add a second test framework or require exhaustive
permutations of unrelated tests.

## Interaction performance

Never compare whole desktops, buffer contents, or undo histories to decide whether
an interaction or redraw needs work. Use explicit small UI-state keys and stable
identities or revisions for immutable payloads. Keep full-text processing on its
owning background worker. A new Desktop field must not silently add deep work to
the render loop; retain regression tests that reject forcing large payloads.

Track heap allocation per actual rendered frame against reproducible, optimized
baselines at matching viewport, contents and interaction. Attribute costs to
scene construction, cell composition, transport and frontend upload; measure
retained CPU/GPU memory separately. Keep benchmark results private by default.

Investigate an increase above 2× the matching baseline. Growth into or beyond the
2–3× range needs a concrete feature benefit that clearly pays for the cost,
recorded with the evidence; do not silently reset a baseline to excuse a
regression. Reuse persistent glyph storage and frame staging buffers, respecting
in-flight GPU ownership. Keep focused allocation checks alongside the affected
renderer paths, and avoid building a second benchmark framework.

## Product direction

This is a greenfield project. Change interfaces when it improves the experience;
do not add aliases, version negotiation or migration machinery for hypothetical
legacy clients. Preserve user data and check stale actions, but prefer one clear
current interface. The visual direction is a modern editor loosely rooted in
Turbo Pascal, using our own renderer.
