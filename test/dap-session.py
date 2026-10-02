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

for session in range(1, 3 if mode == 'reconnect' else 2):
    if stdio:
        stream = sys.stdin.buffer
    else:
        conn, _ = server.accept()
        stream = conn.makefile('rb')
    seq = 0
    pending_attach = pending_variables = pending_source = pending_scopes = None
    configured, breakpoint_requests = [], []
    scope_count = 0
    thread_count = 0

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
            if cmd == 'initialize':
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
                if mode == 'lazy' and thread_count == 3:
                    event('invalidated', dict(areas=['variables']))
                if mode == 'lazy' and thread_count == 4:
                    event('invalidated', dict(areas=['threads']))
                reply(req, dict(threads=[dict(id=7, name='main λ')]))
            elif cmd == 'stackTrace':
                rows = [dict(id=11, name='entry λ', line=2, column=1,
                             source=dict(name='Generated.hs', sourceReference=9))]
                if mode == 'frame':
                    rows.append(dict(id=12, name='other frame', line=1, column=1,
                                     source=dict(name='Other.hs', sourceReference=10)))
                reply(req, dict(stackFrames=rows, totalFrames=len(rows)))
            elif cmd == 'source':
                if mode == 'frame' and args['sourceReference'] == 9:
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
                if mode == 'frame':
                    assert args['frameId'] == 11
                    pending_scopes = req
                else:
                    changed = mode == 'choices' and scope_count > 1
                    reply(req, dict(scopes=[dict(name='Replacement' if changed else 'Locals',
                                                variablesReference=31 if changed else 21, expensive=False)]))
            elif cmd == 'variables':
                reference = args['variablesReference']
                if reference == 21:
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
                if pending_variables:
                    reply(pending_variables, dict(variables=[dict(name='STALE', value='must not display', variablesReference=0)]))
                    pending_variables = None
                # The resulting threads request proves the preceding events were consumed.
                event('thread', dict(reason='started', threadId=7))
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
