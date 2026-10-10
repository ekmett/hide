# SPDX-FileCopyrightText: 2026 Edward Kmett
# SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
# Deterministic DAP peers. Replies are released by requests, never by timers.
import json, os, socket, sys, time

mode = sys.argv[2] if len(sys.argv) > 2 else 'basic'
owned = mode.startswith('server-')
if owned:
    mode = mode[7:]
stdio = mode.startswith('stdio-')
if stdio:
    mode = mode[6:]
else:
    server = socket.socket()
    server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
    server.bind((os.environ['DAP_HOST'], int(os.environ['DAP_PORT'])) if owned else ('127.0.0.1', 0))
    server.listen(2)
    print(server.getsockname()[1], flush=True)

for session in range(1, 4 if mode == 'output-owner' else 3 if mode == 'reconnect' else 2):
    if stdio:
        stream = sys.stdin.buffer
    else:
        conn, _ = server.accept()
        stream = conn.makefile('rb')
    seq = 0
    pending_attach = pending_variables = pending_source = pending_scopes = pending_evaluate = None
    sidebar_scopes, sidebar_variables = {}, {}
    configured, breakpoint_requests = [], []
    scope_count = 0
    source_count = 0
    stack_count = 0
    thread_count = 0
    output_count = 0
    assistance_steps = 0
    thread_exited = False

    def send(value):
        global seq
        seq += 1
        value['seq'] = seq
        body = json.dumps(value, ensure_ascii=False).encode('utf8')
        framed = ('Content-Length: %d\r\n\r\n' % len(body)).encode() + body
        if stdio:
            sys.stdout.buffer.write(framed)
            sys.stdout.buffer.flush()
        else:
            conn.sendall(framed)

    def reply(req, body=None, success=True):
        send(dict(type='response', request_seq=req['seq'], command=req['command'], success=success,
                  body=body or {}, message='' if success else 'fixture refused'))

    def event(name, body=None):
        send(dict(type='event', event=name, body=body or {}))

    def sidebar_pair(pending, req, argument, body):
        # Arrival order is an input, never a prerequisite: hold both requests
        # and release the later one first using each original request sequence.
        target = req['arguments'][argument]
        assert target not in pending, req
        pending[target] = req
        if len(pending) == 2:
            released = list(reversed(list(pending.values())))
            for request in released:
                reply(request, body(request['arguments'][argument]))
            with open(sys.argv[1], 'a') as log:
                log.write(json.dumps(dict(session=session, sidebarRelease=dict(
                    command=req['command'],
                    targets=[request['arguments'][argument] for request in released],
                    requestSeqs=[request['seq'] for request in released]))) + '\n')
            pending.clear()

    def breakpoints(req, actual=None):
        reply(req, dict(breakpoints=[dict(id=41+i, verified=True,
              line=actual if actual is not None else b['line'])
              for i, b in enumerate(req['arguments']['breakpoints'])]))

    try:
        while True:
            headers = {}
            while True:
                line = stream.readline()
                if not line:
                    break
                if line == b'\r\n':
                    break
                key, value = line.decode().split(':', 1)
                headers[key.lower()] = value.strip()
            if not line:
                break
            req = json.loads(stream.read(int(headers['content-length'])))
            cmd, args = req['command'], req.get('arguments', {})
            with open(sys.argv[1], 'a') as log:
                log.write(json.dumps(dict(session=session, request=req)) + '\n')
            if req.get('type') == 'response':
                assert cmd == 'runInTerminal' and req['request_seq'] == terminal_sequence, req
                if mode in ('terminal-shell', 'terminal-invalid', 'terminal-missing'):
                    assert not req['success'] and req['message'], req
                    event('output', dict(category='stderr', output='reverse terminal rejected\n'))
                    continue
                assert req['success'] and req['body']['processId'] > 0, req
                event('output', dict(category='stdout', output='reverse terminal accepted\n'))
                continue
            if cmd == 'initialize':
                if mode.startswith('terminal'):
                    assert args['supportsRunInTerminalRequest']
                    terminal_request=dict(type='request', command='runInTerminal', arguments=dict(
                        kind='integrated', cwd=os.getcwd(),
                        args=[sys.executable, '-u', '-c',
                              'import os,sys,signal; signal.signal(signal.SIGINT, lambda *_: sys.exit(23)); print("terminal ready λ", flush=True); '
                              'print("argv="+repr(sys.argv[1:]), flush=True); '
                              'print("env="+os.environ["THC_DAP_ENV"], flush=True); '
                              'print("cwd="+os.getcwd(), flush=True); '
                              'print("unset="+str("HOME" not in os.environ), flush=True); '
                              'print("stdin="+input(), flush=True); '
                              'print("stderr visible", file=sys.stderr, flush=True); input()', 'literal ; λ'],
                        env=dict(THC_DAP_ENV='value with spaces', HOME=None)))
                    if mode == 'terminal-wrapper':
                        terminal_request['arguments']['args'] = ['/nonexistent/bindist/bin/hdb', 'external-interpreter'] + terminal_request['arguments']['args'][1:]
                    if mode == 'terminal-shell':
                        terminal_request['arguments']['argsCanBeInterpretedByShell'] = True
                    elif mode == 'terminal-invalid':
                        terminal_request['arguments']['args'] = []
                    elif mode == 'terminal-missing':
                        terminal_request['arguments']['args'] = ['/nonexistent/thc-debug-terminal']
                    send(terminal_request)
                    terminal_sequence = seq
                event('initialized')
                reply(req, dict(supportsConfigurationDoneRequest=True,
                      supportsExceptionInfoRequest=mode in ('exception', 'mcp'),
                      exceptionBreakpointFilters=[dict(filter='uncaught', label='Uncaught exceptions', default=True)]))
            elif cmd in ('attach', 'launch'):
                if mode in ('launch-fail', 'launch-fail-late'):
                    if mode == 'launch-fail-late':
                        event('terminated')
                    reply(req, success=False)
                else:
                    pending_attach = req
            elif cmd == 'setBreakpoints':
                configured.append(cmd)
                if mode == 'breakpoints':
                    breakpoint_requests.append(req)
                    if len(breakpoint_requests) == 3:
                        # The first and latest requests have the same line list.
                        breakpoints(breakpoint_requests[2], 202)
                        breakpoints(breakpoint_requests[0], 102)
                        breakpoints(breakpoint_requests[1], 302)
                else:
                    breakpoints(req)
            elif cmd == 'setExceptionBreakpoints':
                configured.append(cmd)
                reply(req)
            elif cmd == 'configurationDone':
                if mode == 'launch-wait':
                    reply(req)
                    event('output', dict(category='console', output='loading cradle'))
                    continue
                if mode in ('launch-fail', 'launch-fail-late'):
                    continue
                assert pending_attach and 'setExceptionBreakpoints' in configured
                reply(req)
                reply(pending_attach)
                event('stopped', dict(reason='entry', threadId=7, allThreadsStopped=True))
            elif cmd == 'exceptionInfo':
                assert mode in ('exception', 'mcp') and args['threadId'] == 7
                reply(req, dict(exceptionId='IOException', description='cannot open λ.hs', breakMode='always',
                    details=dict(typeName='IOException', message='permission denied', stackTrace='Main.hs:6',
                                 innerException=[dict(typeName='Inner', message='nested cause')])) )
            elif cmd == 'threads':
                thread_count += 1
                if pending_evaluate and os.path.exists(sys.argv[1] + '.release'):
                    reply(pending_evaluate, dict(result='STALE delayed watch', variablesReference=0))
                    pending_evaluate = None
                if mode.startswith('watches-child') and pending_variables and os.path.exists(sys.argv[1] + '.release'):
                    reply(pending_variables, dict(variables=[dict(name='STALE forced child', value='ForcedNode', variablesReference=972)]))
                    pending_variables = None
                if mode == 'lazy' and thread_count == 3:
                    event('invalidated', dict(areas=['variables']))
                if mode == 'lazy' and thread_count == 4:
                    event('invalidated', dict(areas=['threads']))
                if mode == 'output-owner' and thread_count != 2:
                    output_count += 1
                    for part in range(8 if output_count == 3 else 1):
                        event('output', dict(output=('x' * (1024 * 1024) if output_count == 3 else '') + '\nsession=%d output=%d part=%d\n' % (session, output_count, part)))
                reply(req, dict(threads=[dict(id=7, name='main λ'), dict(id=8, name='worker')] if mode in ('sidebar', 'sidebar-exit') and not thread_exited else [dict(id=7, name='main λ')]))
            elif cmd == 'stackTrace':
                stack_count += 1
                rows = [dict(id=11, name='entry λ', line=2, column=1,
                             source=dict(name='Generated.hs', sourceReference=9))]
                if mode.startswith('assist'):
                    rows[0]['line'] = 2 + assistance_steps
                if mode == 'assist-history':
                    rows[0]['source']['path'] = sys.argv[1] + ('.hs' if assistance_steps == 0 else '')
                if mode.startswith('source-') or mode in ('watches-private', 'watches-child-policy'):
                    rows[0]['source']['path'] = sys.argv[1] + ('.changed.hs' if mode == 'source-stamp' and stack_count > 1 else '.hs')
                if mode == 'local-source':
                    rows[0].update(column=5, source=dict(name='Local.hs', path=sys.argv[1] + '.hs', sourceReference=0))
                if mode in ('sidebar', 'sidebar-exit'):
                    rows = [dict(id=11 if args['threadId'] == 7 else 21, name='entry λ' if args['threadId'] == 7 else 'worker frame', line=2, column=1, source=dict(name='Generated.hs', sourceReference=9))]
                    if args['threadId'] == 7:
                        rows.append(dict(id=12, name='sibling frame', line=1, column=1, source=dict(name='Other.hs', sourceReference=10)))
                if mode == 'frame' or mode.startswith('watches'):
                    rows.append(dict(id=12, name='other frame', line=1, column=1,
                                     source=dict(name='Other.hs', sourceReference=10)))
                reply(req, dict(stackFrames=rows, totalFrames=len(rows)))
                if mode == 'source-stamp' and pending_source:
                    reply(pending_source, dict(content='STALE reused source handle'))
                    pending_source = None
            elif cmd == 'source':
                source_count += 1
                if mode == 'source-policy-delay' and source_count > 1:
                    while not os.path.exists(sys.argv[1] + '.release'):
                        time.sleep(0.001)
                    reply(req, dict(content='LATE private source body'))
                elif mode == 'source-stamp' and source_count > 1:
                    pending_source = req
                elif mode == 'frame' and args['sourceReference'] == 9:
                    pending_source = req
                elif mode == 'frame':
                    assert pending_source and pending_scopes
                    reply(req, dict(content='chosen frame source\n'))
                    reply(pending_source, dict(content='STALE frame source\n'))
                    reply(pending_scopes, dict(scopes=[dict(name='STALE scopes', variablesReference=91)]))
                else:
                    reply(req, dict(content='module Generated where\nvalue = λ\nsession = %d\n' % session,
                                    mimeType='text/x-haskell'))
            elif cmd == 'scopes':
                scope_count += 1
                if mode in ('sidebar', 'sidebar-exit'):
                    fid = args['frameId']
                    if mode == 'sidebar-exit' and fid == 21:
                        thread_exited = True
                        event('thread', dict(reason='exited', threadId=8))
                        reply(req, dict(scopes=[dict(name='STALE exited scope', variablesReference=221)]))
                    elif mode == 'sidebar' and fid in (11, 12):
                        def scopes(frame):
                            rows = [dict(name='Locals %d' % frame, variablesReference=200+frame, expensive=False)]
                            if frame == 11:
                                rows.append(dict(name='Alias', variablesReference=900, expensive=False))
                            return dict(scopes=rows)
                        sidebar_pair(sidebar_scopes, req, 'frameId', scopes)
                    else:
                        reply(req, dict(scopes=[dict(name='Locals %d' % fid, variablesReference=200+fid, expensive=False)]))
                elif mode == 'frame':
                    assert args['frameId'] == 11
                    pending_scopes = req
                else:
                    changed = mode == 'choices' and scope_count > 1
                    reply(req, dict(scopes=[dict(name='Replacement' if changed else 'Locals',
                                                variablesReference=31 if changed else 21, expensive=False)]))
            elif cmd == 'evaluate' and mode.startswith('watches'):
                assert args['context'] == 'watch' and args['frameId'] in (11, 12), args
                expression = args['expression']
                if expression == 'delay':
                    pending_evaluate = req
                elif expression == 'unsupported':
                    reply(req, success=False)
                elif expression == 'oversized':
                    reply(req, dict(result='X' * (1024*1024+1), variablesReference=0))
                elif expression == 'lazy':
                    reply(req, dict(result='<thunk>', variablesReference=970, presentationHint=dict(lazy=True)))
                elif expression == 'record':
                    reply(req, dict(result='Record', variablesReference=980))
                else:
                    reply(req, dict(result='42', variablesReference=0))
            elif cmd == 'variables':
                reference = args['variablesReference']
                if (mode.startswith('watches-child') or mode == 'watches-pages') and reference == 971:
                    assert args.get('start') == 0 and args.get('count') == 128, args
                    if mode == 'watches-child-error':
                        reply(req, success=False)
                    elif mode in ('watches-child', 'watches-child-invalidated', 'watches-pages'):
                        reply(req, dict(variables=[dict(name='nested', value='ForcedNode', variablesReference=972)]))
                        if mode == 'watches-child-invalidated':
                            event('invalidated', dict(areas=['variables']))
                    else:
                        pending_variables = req
                elif mode.startswith('watches-pages') and reference == 980:
                    assert args == dict(variablesReference=980), args
                    rows = [dict(name='item%d' % i, value=str(i), variablesReference=0) for i in range(260)]
                    rows[129] = dict(name='item129', value='<thunk>', variablesReference=971, presentationHint=dict(lazy=True))
                    if mode == 'watches-pages-oversized':
                        rows[259]['value'] = 'X' * (1024*1024+1)
                    reply(req, dict(variables=rows))
                elif mode.startswith('watches') and reference in (970, 980):
                    assert args == dict(variablesReference=reference), args
                    reply(req, dict(variables=[dict(name='counter', value='42', variablesReference=0), dict(name='nested', value='<thunk>', variablesReference=971, presentationHint=dict(lazy=True))]))
                elif mode in ('sidebar', 'sidebar-exit') and reference in (211, 212, 221):
                    def locals_rows(parent):
                        rows = [dict(name='counter%d' % parent, value=str(parent), variablesReference=0), dict(name='lazy', value='<thunk>', variablesReference=900, presentationHint=dict(lazy=True)), dict(name='waiting', value='expand to wait', variablesReference=910)]
                        if mode == 'sidebar':
                            rows.extend(dict(name='local%d_%d' % (parent, i), value=str(i), variablesReference=0) for i in range(3, 260))
                            rows[129] = dict(name='local%d_129' % parent, value='expand', variablesReference=1000+parent)
                        return rows
                    if mode == 'sidebar' and reference in (211, 212):
                        sidebar_pair(sidebar_variables, req, 'variablesReference', lambda parent: dict(variables=locals_rows(parent)))
                    else:
                        reply(req, dict(variables=locals_rows(reference)))
                elif mode == 'sidebar' and reference in (1211, 1212):
                    reply(req, dict(variables=[dict(name='child%d' % (reference-1000), value='later page', variablesReference=0)]))
                elif mode in ('sidebar', 'sidebar-exit') and reference == 900:
                    reply(req, dict(variables=[dict(name='FORCED lazy alias', value='wrong', variablesReference=0)]))
                elif reference == 21:
                    reply(req, dict(variables=[dict(name='value', value='<thunk>', type='Thunk', variablesReference=22, presentationHint=dict(lazy=mode == 'lazy'))]))
                elif reference == 31:
                    reply(req, dict(variables=[dict(name='WRONG_SCOPE', value='wrong row', variablesReference=0)]))
                else:
                    pending_variables = req
            elif cmd in ('continue', 'next', 'stepIn', 'stepOut'):
                reply(req, dict(allThreadsContinued=True))
                if mode.startswith('exit-'):
                    if mode == 'exit-first':
                        event('exited', dict(exitCode=42))
                    event('terminated')
                    if mode in ('exit-first', 'exit-last'):
                        # A file barrier proves the editor has processed terminated
                        # before this final event is released, regardless of timing.
                        while not os.path.exists(sys.argv[1] + '.release'):
                            time.sleep(0.001)
                        event('output', dict(category='stdout', output='final output'))
                        if mode == 'exit-last':
                            event('exited', dict(exitCode=42))
                    continue
                event('continued', dict(threadId=7, allThreadsContinued=True))
                if pending_evaluate:
                    reply(pending_evaluate, dict(result='STALE resumed watch', variablesReference=0))
                    pending_evaluate = None
                if pending_variables:
                    reply(pending_variables, dict(variables=[dict(name='STALE', value='must not display', variablesReference=0)]))
                    pending_variables = None
                # The resulting threads request proves the preceding events were consumed.
                event('thread', dict(reason='started', threadId=7))
                if mode.startswith('assist') and cmd != 'continue':
                    assistance_steps += 1
                    event('stopped', dict(reason='step', threadId=7, allThreadsStopped=True))
            elif cmd == 'pause':
                event('stopped', dict(reason='pause', threadId=7))
                reply(req)
            elif cmd == 'disconnect':
                reply(req)
                break
            else:
                reply(req, success=False)
    except (ConnectionResetError, BrokenPipeError):
        if mode != 'reconnect' or session != 1:
            raise
    finally:
        stream.close()
        if not stdio:
            conn.close()
if not stdio:
    server.close()
