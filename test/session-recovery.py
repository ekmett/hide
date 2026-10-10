#!/usr/bin/env python3
# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
"""Kill a private test daemon and recover unsaved edits through another frontend.
Usage: python3 test/session-recovery.py /absolute/path/to/hide
POSIX integration harness; operates only on its own temporary session.
"""
import json
import os
import pathlib
import queue
import secrets
import signal
import subprocess
import sys
import tempfile
import threading
import time

binary = str(pathlib.Path(sys.argv[1]).resolve())

def wait_for(predicate, seconds=15):
    until = time.monotonic() + seconds
    while time.monotonic() < until:
        result = predicate()
        if result:
            return result
        time.sleep(.05)
    raise AssertionError('Timed out waiting for checkpoint/session')

class Bridge:
    def __init__(self, ident, root, env):
        self.process = subprocess.Popen([binary, '--mcp-editor', ident], cwd=root, env=env,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        self.queue = queue.Queue()
        self.sequence = 0
        threading.Thread(target=lambda: [self.queue.put(json.loads(line)) for line in self.process.stdout], daemon=True).start()
        self.rpc('initialize', {'protocolVersion': '2025-11-25'})
    def rpc(self, method, params={}):
        self.sequence += 1
        self.process.stdin.write(json.dumps(dict(jsonrpc='2.0', id=self.sequence, method=method, params=params))+'\n')
        self.process.stdin.flush()
        reply = self.queue.get(timeout=20)
        assert reply.get('id') == self.sequence, reply
        return reply
    def call(self, name, args={}, error=False):
        result = self.rpc('tools/call', dict(name=name, arguments=args))['result']
        assert bool(result.get('isError')) == error, result
        return json.loads(result['content'][0]['text'])
    def close(self):
        self.process.stdin.close()
        try:
            self.process.wait(timeout=3)
        except subprocess.TimeoutExpired:
            self.process.kill()
            self.process.wait()

with tempfile.TemporaryDirectory(prefix='thc-recovery-live-') as directory:
    root = pathlib.Path(directory).resolve()
    source = root/'sample.txt'
    source.write_text('original\n')
    config = root/'config/thc/config.toml'
    config.parent.mkdir(parents=True)
    config.write_text('')
    env = dict(os.environ, XDG_CONFIG_HOME=str(root/'config'), XDG_DATA_HOME=str(root/'data'),
        THC_EDIT_WEB_OPEN='0', hide_datadir=str(pathlib.Path(__file__).resolve().parents[1]))
    ident = secrets.token_hex(24)
    checkpoint = root/'data/thc-edit/sessions'/f'{ident}.checkpoint'
    endpoint = pathlib.Path(f'/tmp/thc-edit-{os.geteuid()}')/ident
    # The relay may already have spawned recovery when deletion wins its lock.
    # An absent checkpoint must fail before publishing a new desktop or endpoint.
    rejected = subprocess.run([binary, '--remote-daemon', ident, '--require-checkpoint', str(source)],
        cwd=root, env=env, capture_output=True, text=True, timeout=20)
    assert rejected.returncode != 0, rejected
    assert not checkpoint.exists() and not endpoint.exists(), rejected
    log = open(root/'daemon.log', 'w')
    daemon = subprocess.Popen([binary, '--remote-daemon', ident, str(source)], cwd=root, env=env, stdout=log, stderr=log)
    bridge = None
    frontend = None
    recovered_pid = None
    try:
        wait_for(endpoint.exists)
        bridge = Bridge(ident, root, env)
        names = [tool['name'] for tool in bridge.rpc('tools/list')['result']['tools']]
        config.write_text('[editor.mcp.permissions]\n'+''.join(json.dumps(n)+' = "enable"\n' for n in names))
        doc = next(b for b in bridge.call('list_buffers')['buffers'] if b.get('path') == str(source))
        bid = doc['bufferId']
        bridge.call('buffer_apply_diff', {'buffers':[{'bufferId':bid, 'revision':doc['revision'],
            'diff':'@@ -1 +1 @@\n-original\n+unsaved work\n'}]})
        bridge.call('editor_arrange', {'action':'split_vertical'})
        layout = bridge.call('editor_layout')
        def saved():
            try:
                value = json.loads(checkpoint.read_text())
                return 'unsaved work' in json.dumps(value) and len(value['windows']) == 2
            except (OSError, ValueError):
                return False
        wait_for(saved)
        daemon.kill()
        daemon.wait(timeout=5)
        bridge.close()
        bridge = None
        listing = subprocess.check_output([binary, '--sessions'], cwd=root, env=env, text=True)
        assert ident in listing and 'recoverable' in listing, listing
        # Resuming must preserve the old disk baseline, not silently accept this change.
        source.write_text('external change\n')
        # Change frontend and working directory while restoring the same session.
        frontend = subprocess.Popen([binary, '--web', '--resume', ident], cwd=root.parent, env=env,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        def live():
            result = subprocess.run([binary, '--sessions'], cwd=root, env=env, text=True, capture_output=True)
            return 'running' in result.stdout
        wait_for(live)
        bridge = Bridge(ident, root, env)
        read = bridge.call('read_buffer', {'bufferId':bid})
        assert read['text'] == 'unsaved work\n', read
        assert read['buffer']['modified'], read
        assert len(bridge.call('editor_layout')['windows']) == len(layout['windows'])
        history = bridge.call('editor_history', {'bufferId':bid})
        assert 'original' in json.dumps(history), history
        # A normal save must not overwrite changed disk bytes after recovery.
        result = bridge.rpc('tools/call', dict(name='editor_file',arguments=dict(action='save',bufferId=bid,revision=read['buffer']['revision'])))
        assert source.read_text() == 'external change\n', result
        # Undo still reaches the pre-crash baseline.
        bridge.call('editor_undo', {'bufferId':bid, 'revision':read['buffer']['revision']})
        assert bridge.call('read_buffer', {'bufferId':bid})['text'] == 'original\n'
        # Dismiss a disk-conflict modal through the human wire below, not guest approval.
        # End the private daemon by process signal; clean Exit is covered in RemoteCheck.
        print('PASS: checkpoint, discovery, SIGKILL, browser resume, unsaved buffers, splits, undo and disk conflict')
    finally:
        if bridge:
            bridge.close()
        if frontend:
            frontend.terminate()
            try: frontend.wait(timeout=5)
            except subprocess.TimeoutExpired: frontend.kill(); frontend.wait()
        if daemon.poll() is None:
            daemon.kill(); daemon.wait()
        # Only the daemon bearing this random test identity is eligible for cleanup.
        output = subprocess.check_output(['ps','-axo','pid=,command='], text=True)
        for line in output.splitlines():
            if f'--remote-daemon {ident}' in line:
                pid = int(line.strip().split(None,1)[0])
                try: os.kill(pid, signal.SIGKILL)
                except ProcessLookupError: pass
        for path in [endpoint, pathlib.Path(str(endpoint)+'.log')]:
            try: path.unlink()
            except FileNotFoundError: pass
        log.close()
