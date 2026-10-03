#!/usr/bin/env python3
"""Ctrl-C stops a browser session daemon, retaining unsaved work for resume.
Usage: python3 test/interrupt-session.py /absolute/path/to/hide
Only creates, interrupts and closes its own temporary session.
"""
import os
from pathlib import Path
import runpy
import secrets
import shutil
import signal
import subprocess
import sys
import tempfile
import time

wire = runpy.run_path(str(Path(__file__).with_name('remote-session.py')))
binary = str(Path(sys.argv[1]).resolve())

root = Path(tempfile.mkdtemp(prefix='thc-interrupt-'))
succeeded = False
try:
    source = root / 'sample.txt'
    source.write_text('original\n')
    env = dict(os.environ, XDG_DATA_HOME=str(root / 'data'),
               XDG_CONFIG_HOME=str(root / 'config'), THC_EDIT_WEB_OPEN='0',
               hide_datadir=str(Path(__file__).resolve().parents[1]))
    def run(*args):
        return subprocess.check_output([binary, *args], cwd=root, env=env,
                                       stdin=subprocess.DEVNULL, text=True, timeout=45)
    ident = secrets.token_hex(24)
    daemon_log = open(root / 'daemon.log', 'w+')
    daemon = subprocess.Popen([binary, '--remote-daemon', ident, str(source)],
        cwd=root, env=env, stdin=subprocess.DEVNULL, stdout=daemon_log, stderr=daemon_log)
    def control(p, kind):
        try:
            return wire['control'](p, kind)
        except BaseException:
            record_relay(p, 'waiting for ' + kind)
            if p.poll() is not None:
                error = p.stderr.read()
                with open(root / ('relay-' + str(relays.index(p)) + '.log'), 'ab') as log:
                    log.write(error)
                print('Relay error:', error.decode(errors='replace'), file=sys.stderr)
            raise
    relays = []
    def record_relay(p, stage):
        with open(root / ('relay-' + str(relays.index(p)) + '.status'), 'a') as log:
            log.write(f'{time.monotonic():.6f} {stage}: exit={p.poll()!r}\n')
    def attach():
        p = subprocess.Popen([binary, '--remote'], cwd=root, env=env,
                             stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                             stderr=subprocess.PIPE, bufsize=0)
        relays.append(p)
        wire['send'](p, dict(type='hello', version=1, session=ident,
                            client=secrets.token_hex(24), ack=0, resume=True, args=[]))
        control(p, 'hello')
        control(p, 'assets')
        return p
    def finish(p):
        record_relay(p, 'before closing stdin')
        try:
            p.stdin.close()
        except (BrokenPipeError, OSError):
            pass
        try:
            p.wait(timeout=5)
        except subprocess.TimeoutExpired:
            record_relay(p, 'cleanup timeout; killing fixture relay')
            p.kill()
            p.wait()
        record_relay(p, 'after cleanup')
        p.stdout.close()
        if not p.stderr.closed:
            with open(root / ("relay-" + str(relays.index(p)) + ".log"), "ab") as log:
                log.write(p.stderr.read())
            p.stderr.close()
    def await_state(state):
        until = time.monotonic() + 15
        while time.monotonic() < until:
            listing = run('--sessions')
            if ident + '  ' + state in listing:
                return listing
            time.sleep(.05)
        raise AssertionError(listing)
    frontend = None
    try:
        await_state('running detached')
        p = attach()
        wire['send'](p, dict(type='paste', text='unsaved ', seq=1))
        control(p, 'ack')
        finish(p)
        await_state('running detached')
        with open(root / 'frontend.log', 'w+') as log:
            frontend = subprocess.Popen([binary, '--web', '--resume', ident],
                cwd=root, env=env, stdin=subprocess.DEVNULL, stdout=log, stderr=log)
            await_state('running attached')
            frontend.send_signal(signal.SIGTERM)
            frontend.wait(timeout=20)
            await_state('running detached')
            frontend = subprocess.Popen([binary, '--web', '--resume', ident],
                cwd=root, env=env, stdin=subprocess.DEVNULL, stdout=log, stderr=log)
            await_state('running attached')
            frontend.send_signal(signal.SIGINT)
            frontend.wait(timeout=20)
            log.seek(0)
            assert 'Resume: hide --resume ' + ident in log.read()
            await_state('recoverable')
            assert daemon.wait(timeout=15) == 0
        assert source.read_text() == 'original\n'
        # Own the recovered process too, so a failing attachment cannot leak it.
        daemon = subprocess.Popen([binary, '--remote-daemon', ident, str(source)],
            cwd=root, env=env, stdin=subprocess.DEVNULL, stdout=daemon_log, stderr=daemon_log)
        await_state('running detached')
        p = attach()
        wire['send'](p, dict(type='key', key='s', mods=['ctrl'], seq=1))
        control(p, 'ack')
        assert source.read_text() == 'unsaved original\n'
        wire['send'](p, dict(type='command', command='quit', seq=2))
        control(p, 'closed')
        finish(p)
        assert daemon.wait(timeout=15) == 0
        succeeded = True
        print('Ctrl-C stops daemon; unsaved buffer resumes and saves correctly')
    except BaseException:
        daemon_log.seek(0)
        print(daemon_log.read(), file=sys.stderr)
        endpoint_log = Path('/tmp') / ('thc-edit-' + str(os.geteuid())) / (ident + '.log')
        if endpoint_log.exists():
            print(endpoint_log.read_text(), file=sys.stderr)
        raise
    finally:
        if frontend and frontend.poll() is None:
            frontend.kill()
            frontend.wait()
        for relay in relays:
            if not relay.stdout.closed:
                finish(relay)
        # Finish only this fixture, including on a pre-fix assertion failure.
        if daemon.poll() is None:
            daemon.send_signal(signal.SIGINT)
            try:
                daemon.wait(timeout=10)
            except subprocess.TimeoutExpired:
                daemon.kill()
                daemon.wait()
        daemon_log.close()
finally:
    if succeeded:
        shutil.rmtree(root)
    else:
        print('Failed fixture evidence retained in ' + str(root), file=sys.stderr)
