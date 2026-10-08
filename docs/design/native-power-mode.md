# Native Power Mode prototype

Start a Metal or Vulkan frontend with `HIDE_POWER_MODE=1` to enable small colorful
sparks at the last displayed caret when native text input arrives. For example:

```sh
HIDE_POWER_MODE=1 hide --window
```

The default is off. This approximates Power Mode's typing feedback; it does not
load VS Code extensions. It has no combo counter, screen shake, GIF presets or
custom CSS. Terminal and dialog frames disable the effect. Feedback on a remote
session is optimistic and uses its last displayed caret, so it can precede the
host's acceptance of input. The short-lived sparks use screen coordinates and
are clipped at the menu/status rows, rather than anchored to document text.

Four bursts of eight particles live for 600 ms in fixed SDL-thread storage. The
HLSL cell shader analytically computes their trajectories; the native build
packages validated SPIR-V and translated Metal source. Animation wake kind 17
presents the retained glyph atlas and cell buffer. It does not reconstruct a
Haskell scene, resend a session frame, or upload a new cell grid. Once the final
burst expires, one presentation clears it and the ordinary idle timeout resumes.

`tools/check-native.sh` checks the bounded burst state on every run. Its GPU leg
(`HIDE_TEST_GPU=vulkan` or `metal`) also checks visible sparks, exact disabled and
expired pixels, local extent, unchanged atlas/grid upload counts, and return to
idle. A shader compile or Metal translation alone is not device validation.

The native atlas fixture can emit a private SDL heap-allocation receipt with
`HIDE_TEST_ALLOCATIONS=1`. It compares 32 actual retained presentations with
effects disabled/enabled at the same viewport and contents, after warmup, and
rejects a greater than 2x increase. Compare the disabled result with the matching
optimized revision baseline too. This captures SDL allocations only; driver
internals, Haskell scene/transport work and retained CPU/GPU memory are separate.
