#!/usr/bin/env python3
"""Check headless startup, discovery and resume with no display dependencies.

Usage: python3 test/daemon-session.py /absolute/path/to/thc-edit
Uses only temporary projects and closes only sessions created by this test.
The wire helper uses POSIX pipe selection.
"""
import json
import os
from pathlib import Path
import re
import runpy
import secrets
import subprocess
import sys
import tempfile
import time

wire = runpy.run_path(str(Path(__file__).with_name("remote-session.py")))
binary = str(Path(sys.argv[1]).resolve())
with tempfile.TemporaryDirectory(prefix="thc-daemon-cli-") as temporary:
    root = Path(temporary)
    env = dict(os.environ, XDG_DATA_HOME=str(root / "data"),
               XDG_CONFIG_HOME=str(root / "config"), THC_EDIT_BACKEND="remote",
               thc_edit_datadir=str(Path(__file__).resolve().parents[1]))
    sessions = []

    def run(*args, success=True):
        result = subprocess.run([binary, *args], cwd=root, env=env,
                                stdin=subprocess.DEVNULL, capture_output=True,
                                text=True, timeout=90)
        assert (result.returncode == 0) == success, (args, result.stdout, result.stderr)
        return result.stdout + result.stderr

    def start(*args):
        output = run("--daemon", *args)
        found = re.search(r"^Session: ([a-f0-9]{48})$", output, re.MULTILINE)
        assert found, output
        ident = found.group(1)
        assert "Resume: thc-edit --resume " + ident in output, output
        if ident not in sessions:
            sessions.append(ident)
        return ident

    def close(ident):
        process = subprocess.Popen([binary, "--remote"], cwd=root, env=env,
                                   stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                                   stderr=subprocess.PIPE, bufsize=0)
        try:
            wire["send"](process, dict(type="hello", version=1, session=ident,
                                      client=secrets.token_hex(24), ack=0,
                                      resume=True, args=[]))
            wire["control"](process, "hello")
            wire["control"](process, "assets")
            wire["send"](process, dict(type="command", command="quit", seq=1))
            wire["control"](process, "closed")
            process.stdin.close()
            process.wait(timeout=10)
        finally:
            if process.poll() is None:
                process.kill()
                process.wait(timeout=5)
            process.stdout.close()
            process.stderr.close()

    try:
        source = root / "headless λ.txt"
        source.write_text("headless original\n")
        first = start(str(source))
        listing = run("--sessions")
        assert first in listing and "running detached" in listing and str(root) in listing, listing
        assert start("--resume=" + first) == first
        assert start("--resume", first[:16]) == first
        second = start(str(root))
        choice = run("--daemon", "--resume", success=False)
        assert first in choice and second in choice and "Choose one" in choice, choice
        for args in [("--sessions", str(source)), ("--daemon", "--snapshot"),
                     ("--daemon", "--remote"), ("--daemon", "--resume", first, str(source))]:
            run(*args, success=False)
        for ident in sessions[:]:
            close(ident)
            sessions.remove(ident)
        deadline = time.monotonic() + 10
        while time.monotonic() < deadline:
            listing = run("--sessions")
            if first not in listing and second not in listing:
                break
            time.sleep(.05)
        else:
            raise AssertionError(listing)
        assert source.read_text() == "headless original\n"
        print("daemon CLI startup, discovery, resume, argument and cleanup checks passed")
    finally:
        for ident in sessions:
            close(ident)
