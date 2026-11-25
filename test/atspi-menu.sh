#!/bin/sh
# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
# Run the native menu host on a disposable X server and accessibility bus.
set -eu
if [ "${1:-}" != --session ]; then
    session_dir=$(mktemp -d)
    trap 'rm -rf "$session_dir"' EXIT HUP INT TERM
    mkdir "$session_dir/config" "$session_dir/cache" "$session_dir/data" "$session_dir/runtime"
    chmod 700 "$session_dir/runtime"
    export GSETTINGS_BACKEND=memory XDG_CONFIG_HOME="$session_dir/config" XDG_CACHE_HOME="$session_dir/cache" XDG_DATA_HOME="$session_dir/data" XDG_RUNTIME_DIR="$session_dir/runtime"
    dbus-run-session -- "$0" --session "$session_dir" "$@"
    exit
fi
shift
session_dir=$1
shift
unset DISPLAY WAYLAND_DISPLAY
Xvfb -displayfd 3 -screen 0 800x600x24 -nolisten tcp -ac 3>"$session_dir/display" >"$session_dir/xvfb.log" 2>&1 &
display_pid=$!
trap 'kill "$display_pid" 2>/dev/null || :; wait "$display_pid" 2>/dev/null || :' EXIT HUP INT TERM
attempt=0
while [ ! -s "$session_dir/display" ]; do
    if ! kill -0 "$display_pid" 2>/dev/null || [ "$attempt" -ge 400 ]; then cat "$session_dir/xvfb.log" >&2;exit 1;fi
    attempt=$((attempt+1))
    sleep .02
done
DISPLAY=:$(cat "$session_dir/display")
export DISPLAY
for property in IsEnabled ScreenReaderEnabled; do
    gdbus call --session --dest org.a11y.Bus --object-path /org/a11y/bus --method org.freedesktop.DBus.Properties.Set org.a11y.Status "$property" '<true>' >/dev/null
done
python3 "$(dirname "$0")/atspi-menu.py" "$@"
