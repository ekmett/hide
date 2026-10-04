#!/usr/bin/env python3
"""Check persistent local sessions across browser and terminal frontends.

Usage: python3 test/editor-session.py /absolute/path/to/hide
Requires a POSIX pseudo-terminal and an executable built with browser support.
Only sessions created for this test's temporary files are closed.
"""
import fcntl
import json
import os
import pathlib
import pty
import re
import runpy
import secrets
import select
import signal
import struct
import subprocess
import sys
import tempfile
import termios
import time

helpers = {}
exec(pathlib.Path(__file__).with_name('remote-browser.py').read_text().split('\nbinary=', 1)[0], helpers)
WebSocket, Display = helpers['WebSocket'], helpers['Display']
wire = runpy.run_path(str(pathlib.Path(__file__).with_name('remote-session.py')))
binary = str(pathlib.Path(sys.argv[1]).resolve())


def wait_for(predicate, seconds=15):
    deadline = time.monotonic() + seconds
    while time.monotonic() < deadline:
        result = predicate()
        if result:
            return result
        time.sleep(.05)
    raise AssertionError('Timed out waiting for session state')


def display_text(display):
    return '\n'.join(''.join(run if isinstance(run, str) else run[0]
                            for span in (row or []) for run in span[3])
                     for row in display.rows)


with tempfile.TemporaryDirectory(prefix='hide-session-') as directory:
    root = pathlib.Path(directory)
    source = root / 'unsaved λ.txt'
    second = root / 'second.txt'
    source.write_text('original\n')
    second.write_text('second\n')
    env = dict(os.environ, THC_EDIT_WEB_OPEN='0', TERM='xterm-256color',
               hide_datadir=str(pathlib.Path(__file__).resolve().parents[1]), XDG_DATA_HOME=str(root/'data'))
    processes, sockets, logs, sessions = [], [], [], set()
    catalog = root/'data/thc-edit/sessions'

    def discover():
        if catalog.exists():
            for path in catalog.glob('*.json'):
                try:
                    record = json.loads(path.read_text())
                    if any(str(file) in record.get('sessionArguments', []) for file in (source, second)):
                        sessions.add(record['sessionId'])
                except (OSError, ValueError):
                    pass

    def web(arguments):
        log = tempfile.TemporaryFile(mode='w+')
        logs.append(log)
        process = subprocess.Popen([binary, '--web', *arguments], env=env,
                                   stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=log)
        processes.append(process)
        def address():
            log.seek(0)
            text = log.read()
            found = re.search(r'Haskell browser: (http://\S+)', text)
            if found:
                return found.group(1)
            if process.poll() is not None:
                raise AssertionError(text)
        ws = WebSocket(wait_for(address))
        sockets.append(ws)
        display = Display(ws)
        display.until('frame')
        discover()
        return process, ws, display

    def event(ws, display, serial, **fields):
        ws.send(dict(seq=serial, **fields))
        display.until('ack', lambda value: value['seq'] == serial)

    def expect_text(display, expected):
        if expected not in display_text(display):
            display.until('frame', lambda _: expected in display_text(display))

    def detach(process, ws, sig):
        process.send_signal(sig)
        output = process.communicate(timeout=10)[0].decode()
        ws.close()
        found = re.search(r'^Session: ([a-f0-9]{48})$', output, re.MULTILINE)
        assert found, output
        assert 'Resume: hide --resume ' + found.group(1) in output, output
        sessions.add(found.group(1))
        return found.group(1)

    def inspect_live_editor(ws, display):
        discover()
        ident = next(ident for ident in sessions
                     if str(source) in json.loads((catalog / (ident + '.json')).read_text())['sessionArguments'])
        bridge = subprocess.Popen([binary, '--mcp-editor', ident], env=env,
                                  stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                                  text=True, bufsize=1)
        def rpc(serial, method, params):
            bridge.stdin.write(json.dumps(dict(jsonrpc='2.0', id=serial, method=method, params=params)) + '\n')
            bridge.stdin.flush()
            assert select.select([bridge.stdout], [], [], 10)[0], 'MCP response timed out'
            reply = json.loads(bridge.stdout.readline())
            assert reply.get('id') == serial and 'error' not in reply, reply
            return reply['result']
        def tool(serial, name, arguments=None):
            reply = rpc(serial, 'tools/call', dict(name=name, arguments=arguments or {}))
            assert not reply.get('isError'), reply
            return json.loads(reply['content'][0]['text'])
        try:
            initialized = rpc(1, 'initialize', dict(protocolVersion='2025-11-25', capabilities={},
                                                   clientInfo=dict(name='session-test', version='1')))
            assert 'tools' in initialized['capabilities'], initialized
            bridge.stdin.write(json.dumps(dict(jsonrpc='2.0', method='notifications/initialized')) + '\n')
            bridge.stdin.flush()
            names = {item['name'] for item in rpc(2, 'tools/list', {})['tools']}
            assert {'list_windows', 'list_buffers', 'read_buffer', 'read_selection'} <= names, names
            buffers = tool(3, 'list_buffers')['buffers']
            buffer = next(item for item in buffers if item['path'] and pathlib.Path(item['path']).resolve() == source.resolve())
            assert buffer['modified'], buffer
            arguments = dict(bufferId=buffer['bufferId'])
            assert tool(4, 'read_buffer', arguments)['text'] == 'persistent unsaved λ'
            assert tool(5, 'list_windows')['windows']
            # Introspection must leave the display's writer ownership intact.
            event(ws, display, 3, type='paste', text='+')
            assert tool(6, 'read_buffer', arguments)['text'] == 'persistent unsaved λ+'
            event(ws, display, 4, type='command', command='hide.edit.undo')
            assert tool(7, 'read_buffer', arguments)['text'] == 'persistent unsaved λ'
        finally:
            bridge.stdin.close()
            try:
                bridge.wait(timeout=10)
            except subprocess.TimeoutExpired:
                bridge.terminate()
                bridge.wait(timeout=10)
            bridge.stdout.close()
            bridge.stderr.close()

    def terminal(ident, *, choose=False, prefix=None, typed=b'!'):
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack('HHHH', 25, 80, 0, 0))
        before = termios.tcgetattr(slave)
        arguments = [binary, '--terminal', '--resume']
        if not choose:
            arguments.append(prefix or ident)
        process = subprocess.Popen(arguments, env=env,
                                   stdin=slave, stdout=slave, stderr=slave)
        processes.append(process)
        output = bytearray()
        def painted():
            if select.select([master], [], [], .05)[0]:
                output.extend(os.read(master, 65536))
            if process.poll() is not None:
                raise AssertionError(bytes(output).decode(errors='replace'))
            return b'persistent unsaved' in output
        try:
            if choose:
                def offered():
                    if select.select([master], [], [], .05)[0]:
                        output.extend(os.read(master, 65536))
                    if process.poll() is not None:
                        raise AssertionError(bytes(output).decode(errors='replace'))
                    return b'Resume session number (empty to cancel):' in output
                wait_for(offered)
                selected = re.search(rb'(?:^|\n)([0-9]+)\) ' + ident.encode() + rb' ', output)
                assert selected, bytes(output)
                os.write(master, selected.group(1) + b'\n')
            wait_for(painted)
            # Detach immediately after typing: the frontend must flush input.
            os.write(master, typed + b'\x1d')
            def detached():
                if select.select([master], [], [], .05)[0]:
                    output.extend(os.read(master, 65536))
                return process.poll() is not None
            wait_for(detached, 10)
            while select.select([master], [], [], .05)[0]:
                output.extend(os.read(master, 65536))
            assert process.returncode == 0, bytes(output)
            assert ('Session: ' + ident).encode() in output, bytes(output)
            after = termios.tcgetattr(slave)
            # macOS sets a kernel-owned pending-retype flag on canonical restore.
            before[3] &= ~getattr(termios, 'PENDIN', 0)
            after[3] &= ~getattr(termios, 'PENDIN', 0)
            assert before == after, 'Terminal settings were not restored'
        finally:
            if process.poll() is None:
                process.terminate()
                process.wait(timeout=10)
            os.close(master)
            os.close(slave)

    def platform_keys(ws, display):
        serial = 100
        def send(**fields):
            nonlocal serial
            serial += 1
            event(ws, display, serial, **fields)
        send(type='frontend', mode=3, mac=True)
        send(type='command', command='hide.edit.select-all')
        serial += 1
        ws.send(dict(seq=serial, type='key', key='j', mods=['cmd','shift']))
        copied = display.until('copy')
        assert copied['text'] == 'persistent unsaved λ', copied
        display.until('ack', lambda value: value['seq'] == serial)
        # A removed paste accelerator must preserve the selected text.
        send(type='key', key='v', mods=['cmd'])
        expect_text(display, 'persistent unsaved λ')
        serial += 1
        ws.send(dict(seq=serial, type='key', key='k', mods=['cmd','shift']))
        display.until('paste-request')
        display.until('ack', lambda value: value['seq'] == serial)
        send(type='paste', text='clipboard replacement')
        expect_text(display, 'clipboard replacement')
        send(type='command', command='hide.edit.undo')
        expect_text(display, 'persistent unsaved λ')
        config = root/'thc.toml'
        config.write_text('[editor.keybindings.macos.source]\n"hide.edit.copy" = ["Cmd+Shift+J"]\n"hide.edit.paste" = ["Cmd+Shift+L"]\n')
        send(type='menu', command='hide.bindings.reload')
        if ('Cmd+Shift+L','hide.edit.paste') not in map(tuple,display.meta.get('bindings', [])):
            display.until('frame', lambda value: ['Cmd+Shift+L','hide.edit.paste'] in value.get('bindings', []))
        serial += 1
        ws.send(dict(seq=serial, type='key', key='l', mods=['cmd','shift']))
        display.until('paste-request')
        display.until('ack', lambda value: value['seq'] == serial)
        send(type='frontend', mode=3, mac=False)
        print('Live macOS-profile clipboard remaps, removed accelerator, and worker reload across browser transport passed')

    try:
        (root/'thc.toml').write_text('[editor.keybindings.macos.source]\n"hide.edit.copy" = ["Cmd+Shift+J"]\n"hide.edit.paste" = ["Cmd+Shift+K"]\n')
        process, ws, display = web([str(source)])
        event(ws, display, 1, type='command', command='hide.edit.select-all')
        event(ws, display, 2, type='paste', text='persistent unsaved λ')
        expect_text(display, 'persistent unsaved λ')
        assert source.read_text() == 'original\n', 'Unsaved edit reached disk'
        platform_keys(ws, display)
        inspect_live_editor(ws, display)
        first_id = detach(process, ws, signal.SIGINT)
        assert (catalog / (first_id + '.json')).exists()

        process, ws, display = web([str(second)])
        second_id = detach(process, ws, signal.SIGTERM)
        assert first_id != second_id
        chooser = subprocess.run([binary, '--resume'], env=env, stdin=subprocess.DEVNULL,
                                 capture_output=True, text=True, timeout=10)
        listing = chooser.stdout + chooser.stderr
        assert first_id in listing and second_id in listing, listing
        assert 'Choose one with --resume ID.' in listing, listing

        terminal(first_id, choose=True)
        listed_ids = set(re.findall(r'[a-f0-9]{48}', listing))
        prefix = next(first_id[:length] for length in range(1, 48)
                      if sum(ident.startswith(first_id[:length]) for ident in listed_ids) == 1)
        terminal(first_id, prefix=prefix, typed=b'')
        assert source.read_text() == 'original\n', 'Terminal detached by saving unexpectedly'
        process, ws, display = web(['--resume=' + first_id])
        expect_text(display, 'persistent unsaved λ!')
        # The browser shortcut uses a frontend-only control, after queued input.
        ws.send(dict(type='paste', text='+', seq=1))
        ws.send(dict(type='detach'))
        display.until('detached')
        output = process.communicate(timeout=10)[0].decode()
        assert process.returncode == 0 and 'Session: ' + first_id in output, output
        assert (catalog / (first_id + '.json')).exists()
        ws.close()
        process, ws, display = web(['--resume=' + first_id])
        expect_text(display, 'persistent unsaved λ!+')
        event(ws, display, 1, type='command', command='hide.edit.undo')
        if 'persistent unsaved λ!+' in display_text(display):
            display.until('frame', lambda _: 'persistent unsaved λ!+' not in display_text(display))
        expect_text(display, 'persistent unsaved λ!')
        event(ws, display, 2, type='command', command='hide.edit.undo')
        if 'persistent unsaved λ!' in display_text(display):
            display.until('frame', lambda _: 'persistent unsaved λ!' not in display_text(display))
        expect_text(display, 'persistent unsaved λ')
        event(ws, display, 3, type='command', command='hide.edit.redo')
        expect_text(display, 'persistent unsaved λ!')
        event(ws, display, 4, type='key', key='F2')
        assert source.read_text() == 'persistent unsaved λ!'
        event(ws, display, 5, type='command', command='hide.app.quit')
        display.until('closed')
        process.wait(timeout=10)
        wait_for(lambda: not (catalog / (first_id + '.json')).exists())
        sessions.discard(first_id)
        ended = subprocess.run([binary, '--terminal', '--resume=' + first_id], env=env,
                               stdin=subprocess.DEVNULL, capture_output=True, text=True, timeout=10)
        assert ended.returncode != 0 and 'No unfinished session matches' in ended.stderr, ended
        assert not (catalog / (first_id + '.json')).exists(), 'Ended session was restarted'
        print('Local browser→terminal→browser sessions preserve unsaved edits and undo; SIGINT/SIGTERM and browser shortcut detach, interactive/nonTTY chooser, unique prefix and explicit resume, save/Exit cleanup and live read-only MCP passed')
    finally:
        for ws in sockets:
            ws.close()
        for process in processes:
            if process.poll() is None:
                process.terminate()
                process.wait(timeout=10)
        discover()
        for ident in sessions:
            relay = subprocess.Popen([binary, '--remote'], env=env, stdin=subprocess.PIPE,
                                     stdout=subprocess.PIPE, stderr=subprocess.PIPE, bufsize=0)
            try:
                wire['send'](relay, dict(type='hello', version=1, session=ident,
                                        client=secrets.token_hex(24), ack=0, resume=True, args=[]))
                wire['control'](relay, 'hello')
                wire['send'](relay, dict(type='key', key='F2', seq=1))
                wire['control'](relay, 'ack')
                wire['send'](relay, dict(type='command', command='hide.app.quit', seq=2))
                wire['control'](relay, 'closed')
            finally:
                relay.stdin.close()
                try:
                    relay.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    relay.terminate()
                    relay.wait(timeout=10)
                relay.stdout.close()
                relay.stderr.close()
        for log in logs:
            log.close()
