#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
"""Inspect the installed Haskell editor, separately from the C provider fixture."""
import os
from pathlib import Path
import secrets
import subprocess
import sys
import tempfile
import time
import gi

gi.require_version("Atspi", "2.0")
from gi.repository import Atspi


def await_value(label, predicate):
    deadline = time.monotonic() + 20
    while time.monotonic() < deadline:
        value = predicate()
        if value:
            return value
        time.sleep(.02)
    raise AssertionError(label)


def roots():
    desktop = Atspi.get_desktop(0)
    for i in range(desktop.get_child_count()):
        app = desktop.get_child_at_index(i)
        for j in range(app.get_child_count()):
            node = app.get_child_at_index(j)
            if node.get_name() == "Hide":
                yield node


def children(node):
    return [node.get_child_at_index(i) for i in range(node.get_child_count())]


executable = str(Path(sys.argv[1]).resolve())
with tempfile.TemporaryDirectory(prefix="hide-atspi-editor-") as directory:
    work = Path(directory)
    env = dict(os.environ, HOME=directory, XDG_DATA_HOME=str(work / "data"))
    env.pop("THC_EDIT_SESSION", None)
    env.pop("THC_EDIT_CAPTURE_EXIT", None)
    if "HIDE_ATSPI_VULKAN_ICD_DIR" in env:
        candidates = list(Path(env["HIDE_ATSPI_VULKAN_ICD_DIR"]).glob("*lvp*.json"))
        assert len(candidates) == 1, "one software Vulkan ICD is required"
        env["VK_DRIVER_FILES"] = env["VK_ICD_FILENAMES"] = str(candidates[0])
    capture = work / "first-frame.bmp"
    env["THC_EDIT_CAPTURE"] = str(capture)
    session = secrets.token_hex(24)
    owned = []
    with (work / "editor.log").open("w+") as log:
        try:
            daemon = subprocess.Popen([executable, "--remote-daemon", session],
                                      cwd=directory, env=env, stdout=log, stderr=log)
            owned.append(daemon)
            record = work / "data" / "thc-edit" / "sessions" / (session + ".json")
            await_value("owned daemon did not publish its session", record.exists)
            frontend = subprocess.Popen([executable, "--resume", session, "--window", "--scale", "1"],
                                        cwd=directory, env=env, stdout=log, stderr=log)
            owned.append(frontend)
            root = await_value("installed editor window missing from AT-SPI", lambda: next(roots(), None))
            # The existing renderer capture is an actual completed-frame
            # receipt; do not assume an initial scene survives attach/resize.
            await_value("installed editor did not render its initial frame", lambda:
                        capture.exists() and capture.stat().st_size > 54)
            window = subprocess.check_output(["xdotool", "search", "--onlyvisible", "--pid", str(frontend.pid)],
                                             text=True, timeout=5).splitlines()[0]

            def key(value):
                subprocess.run(["xdotool", "key", "--window", window, value], check=True, timeout=5)

            key("F10")
            menu = await_value("actual Haskell File menu missing", lambda: next(
                (node for node in children(root) if node.get_name() == "File"), None))
            assert menu.get_role() == Atspi.Role.MENU
            assert any("Open" in row.get_name() for row in children(menu))
            selected = [row for row in children(menu) if row.get_state_set().contains(Atspi.StateType.SELECTED)]
            assert len(selected) == 1 and selected[0].get_name() == "New"
            for row in children(menu):
                action = row.get_action_iface()
                assert action is None or action.get_n_actions() == 0
            key("Escape")
            await_value("Haskell menu close not published", lambda: root.get_child_count() == 0)
            await_value("Haskell menu retained reference survived close", lambda:
                        menu.get_state_set().contains(Atspi.StateType.DEFUNCT))
            key("F10")
            await_value("Haskell menu reopen not published", lambda: root.get_child_count() > 0)
            assert menu.get_state_set().contains(Atspi.StateType.DEFUNCT)
            key("Escape")
            key("alt+x")
            assert frontend.wait(timeout=8) == 0
            assert daemon.wait(timeout=8) == 0
            print("Installed Haskell editor menu discovery, read-only state and retirement passed")
        except BaseException:
            log.flush()
            log.seek(0)
            print(log.read()[-8192:], file=sys.stderr)
            raise
        finally:
            for process in reversed(owned):
                if process.poll() is None:
                    process.terminate()
                    try:
                        process.wait(timeout=5)
                    except subprocess.TimeoutExpired:
                        process.kill()
                        process.wait(timeout=5)
