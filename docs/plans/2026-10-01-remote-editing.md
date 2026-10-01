# Remote editing

## Design

Run the editor and all project tooling beside the remote filesystem. Local Metal/Vulkan and browser displays use the same protocol. `thc edit PATH --remote` serves length-framed packets on stdin/stdout, diagnostics only on stderr, no PTY. `thc-edit USER@HOST:PATH --metal` and `--web` launch the installed OpenSSH client and attach locally. Honor SSH configuration and host authentication; do not forward credentials or install software automatically.

The SSH bootstrap runs `thc-edit --remote` directly, without requiring the full thc project. Startup arguments travel in the protocol hello, avoiding Unix/cmd.exe shell quoting differences. Native Windows remote servers are required alongside macOS/Linux.

Remote sessions survive transport loss. Private per-user endpoints hold persistent sessions: Unix sockets on POSIX, authenticated loopback connections on Windows. Reattachment receives a complete frame and resets compression. The session continues polling tooling while detached. One writer is allowed. Explicit Exit runs ordinary unsaved-buffer checks and ends the session; closing the transport detaches. Session identities and client event sequences prevent duplicate edits on reconnect. Keep unacknowledged input bounded; reject protocol mismatches and malformed or oversized packets.

Protocol version 1 uses existing WebInput JSON, assets/control JSON, and unchanged compressed display bytes. Stdio framing is a four-byte big-endian payload length followed by one kind byte (0 JSON, 1 binary) and the payload. Limit payload to 16 MiB plus its kind byte. Upload metadata is followed by one binary payload. Display decompression is bounded to 64 MiB. Control messages carry version, session identity, event sequence acknowledgements and connection status. Native rendering validates dimensions, colors and glyph spans before drawing.

The browser uses a local authenticated WebSocket bridge to SSH. Browser assets and visual processing stay local. Local clipboard and uploads/downloads bridge explicitly; remote File/Open and Save continue using the remote filesystem. Local window titles identify the host. Cursor blink, mouse pointer, CRT and zoom remain local.

Remote code is behind a manual `remote` Cabal flag, alongside existing `web`, `window`, and `terminal` flags. Share encoding and input semantics rather than implementing another editor. No repository content refers to coding agents; keep the current working branch.

## Interfaces

- `THC.Edit.Protocol` owns `WebInput`, parsing/application, display rows and compression, metadata/assets, framing (`WirePacket = JsonPacket Value | BinaryPacket ByteString`, `readPacket`, `writePacket`) and bounded display decoding.
- `THC.Edit.Remote` owns `RemotePeer {peerSend :: WirePacket -> IO (), peerReceive :: IO (Maybe WirePacket)}`, `withSSHPeer :: String -> [String] -> (RemotePeer -> IO ()) -> IO ()`, and remote daemon/attachment helpers. Persistent execution receives the existing effects and tick callbacks.
- `THC.Edit.RemoteWindow` exports `runRemoteWindow :: Backend -> Double -> (Int,Int) -> Int -> String -> RemotePeer -> IO ()`.
- Web retains public protocol reexports for compatibility; the local browser bridge consumes RemotePeer. App selects server, SSH client or existing local execution before opening files locally.

## Implementation plan

1. Extract Protocol from Web without changing browser packets. Add bounded framing and decoding tests, including truncated input, oversize lengths, dictionary reset and Unicode. Preserve existing web round-trip tests.
2. Implement persistent remote sessions and SSH peer transport. Test headless startup, malformed handshake, duplicate input, disconnect/reattach, detached ticks, exclusive writer, safe argument quoting and explicit shutdown.
3. Implement the native remote renderer/input adapter. Reuse SDL primitives and glyph rendering; keep graphical calls on the main OS thread. Test decoded frames, input conversion, native close versus detach, uploads and clipboard behavior.
4. Wire CLI flags and browser bridge. Document setup and session lifecycle. Test that remote paths never open locally and that headless builds require no SDL or web server dependencies.
5. Run the full regression suite and end-to-end local process/socket/browser tests; exercise SSH where an authorized host is available. Perform independent review, resolve findings, build final executable and commit.

## Review focus

- Broken pipes and half-written frames do not destroy buffers or spin.
- Lost acknowledgements do not duplicate edits or commands.
- Shell metacharacters and leading-dash paths remain arguments.
- Untrusted lengths and compressed packets have memory bounds.
- Modal dialogs and disconnection do not leak keystrokes to the wrong recipient.
