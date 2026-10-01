#!/usr/bin/env python3
"""Check the stdio relay, detached daemon, reconnect replay, and clean shutdown.

Usage: python3 test/remote-session.py /absolute/path/to/thc-edit
"""
import json
import os
from pathlib import Path
import secrets
import select
import struct
import subprocess
import sys
import tempfile
import time


def send(process, value):
    payload = b"\0" + json.dumps(value, ensure_ascii=False).encode()
    process.stdin.write(struct.pack(">I", len(payload)) + payload)
    process.stdin.flush()


def receive(process):
    deadline = time.monotonic() + 15

    def exact(size):
        chunks = bytearray()
        while len(chunks) < size:
            remaining = deadline - time.monotonic()
            if remaining <= 0 or not select.select([process.stdout], [], [], remaining)[0]:
                raise AssertionError("Remote packet timed out")
            chunk = os.read(process.stdout.fileno(), size - len(chunks))
            if not chunk:
                raise AssertionError("Remote relay ended before expected packet")
            chunks.extend(chunk)
        return bytes(chunks)

    size, = struct.unpack(">I", exact(4))
    assert 1 <= size <= 16777217
    payload = exact(size)
    return json.loads(payload[1:]) if payload[0] == 0 else payload[1:]


def control(process, kind):
    while True:
        value = receive(process)
        if isinstance(value, dict) and value.get("type") == kind:
            return value
        assert not isinstance(value, dict) or value.get("type") != "error", value


def main(executable):
    session, client = secrets.token_hex(24), secrets.token_hex(24)
    env = dict(os.environ)
    env.setdefault("thc_edit_datadir", str(Path(__file__).resolve().parent.parent))
    with tempfile.TemporaryDirectory(prefix="thc-remote-") as directory:
        path = Path(directory) / "file ' quoted ; λ.hs"
        path.write_text("")

        def launch():
            process = subprocess.Popen([executable, "--remote"],
                                       stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                       stderr=subprocess.PIPE, env=env, bufsize=0)
            send(process, dict(type="hello", version=1, session=session, client=client, ack=0, args=["--", str(path)]))
            hello = control(process, "hello")
            control(process, "assets")
            frame = receive(process)
            assert isinstance(frame, bytes) and frame[0] == 0, "Reset frame required on attach"
            return process, hello

        def event(process, seq, **fields):
            send(process, dict(seq=seq, **fields))
            assert control(process, "ack")["seq"] == seq

        process, first = launch()
        event(process, 1, type="paste", text="λ")
        event(process, 1, type="paste", text="λ")
        event(process, 2, type="key", key="F2")
        assert path.read_text() == "λ", "Duplicate sequence changed the file twice"
        send(process, dict(seq=3, type="paste", text="-"))
        process.stdin.close()
        process.wait(timeout=5)
        process.stdout.close()
        process.stderr.close()
        time.sleep(0.1)
        process, resumed = launch()
        assert resumed["epoch"] == first["epoch"]
        assert resumed["ack"] in (2, 3)
        event(process, 3, type="paste", text="-")
        event(process, 4, type="key", key="F2")
        assert path.read_text() == "λ-", "Unacknowledged edit replay duplicated or lost input"
        event(process, 5, type="key", key="x", mods=["alt"])
        control(process, "closed")
        process.stdin.close()
        process.wait(timeout=5)
        process.stdout.close()
        process.stderr.close()
        socket = Path(f"/tmp/thc-edit-{os.geteuid()}/{session}")
        for _ in range(50):
            if not socket.exists():
                break
            time.sleep(0.02)
        assert not socket.exists(), "Explicit Exit left the remote daemon listening"
        # A reconnect after a lost final ACK must not create a replacement daemon.
        ended = subprocess.Popen([executable, "--remote"], stdin=subprocess.PIPE,
                                 stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                 env=env, bufsize=0)
        send(ended, dict(type="hello", version=1, session=session, client=client,
                         ack=5, resume=True, args=["--", str(path)]))
        error = control(ended, "error")
        assert "refusing to restart" in error["message"]
        ended.stdin.close()
        ended.wait(timeout=5)
        ended.stdout.close()
        ended.stderr.close()
        assert not socket.exists(), "Reconnect recreated an explicitly closed session"
    print("remote relay integration checks passed")


if __name__ == "__main__":
    main(str(Path(sys.argv[1]).resolve()))
