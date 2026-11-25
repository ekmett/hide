#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
"""Run inside a disposable display and accessibility bus; argv[1] is the host."""
import os
import select
import subprocess
import sys
import time
import gi

gi.require_version("Atspi", "2.0")
from gi.repository import Atspi


def await_value(label, predicate):
    deadline = time.monotonic() + 8
    while time.monotonic() < deadline:
        result = predicate()
        if result:
            return result
        time.sleep(.02)
    raise AssertionError(label)


def roots():
    desktop = Atspi.get_desktop(0)
    result = []
    for i in range(desktop.get_child_count()):
        app = desktop.get_child_at_index(i)
        for j in range(app.get_child_count()):
            root = app.get_child_at_index(j)
            if root.get_name() == "Hide":
                result.append(root)
    return result


def children(node):
    return [node.get_child_at_index(i) for i in range(node.get_child_count())]


def defunct(node):
    return node.get_state_set().contains(Atspi.StateType.DEFUNCT)


def receipt(host):
    assert select.select([host.stdout], [], [], 8)[0], "host receipt timed out"
    return host.stdout.readline().strip()


# Missing accessibility services must not prevent normal SDL startup/rendering.
without_bus = dict(os.environ, DBUS_SESSION_BUS_ADDRESS="unix:path=" +
                   os.path.join(os.environ["XDG_RUNTIME_DIR"], "absent-bus"))
plain = subprocess.run([sys.argv[1]], input="OPEN\nCLOSE\nFREE\nQUIT\n",
                       stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
                       encoding="utf-8", env=without_bus, timeout=8)
assert plain.returncode == 0 and plain.stdout.splitlines() == ["READY", "PUBLISHED", "PUBLISHED", "FREED"]

host = subprocess.Popen([sys.argv[1]], stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                        text=True, encoding="utf-8")
try:
    assert receipt(host) == "READY"
    root = await_value("actual SDL window root discovery", lambda: next(iter(roots()), None))

    def command(value, expected="PUBLISHED"):
        host.stdin.write(value + "\n")
        host.stdin.flush()
        assert receipt(host) == expected

    command("OPEN")
    menu = await_value("menu discovery", lambda: next((x for x in children(root) if x.get_name() == "File"), None))
    assert menu.get_role() == Atspi.Role.MENU
    rows = children(menu)
    assert [row.get_name() for row in rows] == ["Open fixture λ", "Disabled fixture", "Checked fixture"]
    assert not rows[1].get_state_set().contains(Atspi.StateType.ENABLED)
    assert rows[2].get_state_set().contains(Atspi.StateType.CHECKED)
    assert rows[0].get_state_set().contains(Atspi.StateType.SELECTED)
    for node in [menu] + rows:
        action = node.get_action_iface()
        assert action is None or action.get_n_actions() == 0
    extents = rows[0].get_component_iface().get_extents(Atspi.CoordType.WINDOW)
    assert (extents.width, extents.height) == (184, 16)
    command("REPLACE")
    replacement = await_value("replacement discovery", lambda: next((x for x in children(root) if x.get_name() == "Edit"), None))
    await_value("retained old menu object retirement", lambda: defunct(menu))
    await_value("retained old item object retirement", lambda: defunct(rows[0]))
    command("PRIVATE")
    await_value("privacy-hidden menu clears published children", lambda: root.get_child_count() == 0)
    await_value("privacy-hidden menu retires previous object", lambda: defunct(replacement))
    command("OPEN")
    reopened = await_value("reopened menu discovery", lambda: next(iter(children(root)), None))
    assert defunct(menu) and defunct(rows[0])
    command("INVALID", "REJECTED")
    await_value("invalid metadata clears previous menu", lambda: root.get_child_count() == 0)
    await_value("invalid metadata retires old reference", lambda: defunct(reopened))
    command("OPEN")
    reopened = await_value("menu recovers after invalid metadata", lambda: next(iter(children(root)), None))
    command("CLOSE")
    await_value("closed menu clears children", lambda: root.get_child_count() == 0)
    await_value("closed menu retires references", lambda: defunct(reopened))
    command("FREE", "FREED")
    await_value("adapter shutdown retires root", lambda: defunct(root))
    command("QUIT", "")
    assert host.wait(timeout=5) == 0
    print("SDL AT-SPI menu discovery, readonly state, replacement, privacy and shutdown passed")
finally:
    if host.poll() is None:
        host.terminate()
        host.wait(timeout=5)
