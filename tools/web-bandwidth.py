#!/usr/bin/env python3
"""Compare identical rendered screens; stdlib-only, including receiver checks.
Usage: python3 tools/web-bandwidth.py .deps/bandwidth-trial/frames.jsonl
Counts server WebSocket payload + frame header, excluding TCP/TLS and input.
"""
import collections, json, statistics, sys, time, zlib

def cells(spans):
    result = {}
    for x, fg, bg, runs in spans:
        for run in runs:
            for text, width in ([(c, 1) for c in run] if isinstance(run, str) else [run]):
                result[x] = (fg, bg, text, width)
                for i in range(1, width): result[x+i] = (fg, bg, '', 0)
                x += width
    return result

def pack(selected):
    spans = []
    for x, (fg, bg, text, width) in selected:
        if not width: continue
        if not spans or spans[-1][0] != x or spans[-1][1:3] != [fg, bg]:
            spans.append([x, fg, bg, [], x])
        span = spans[-1]
        if width == 1 and len(text) == 1:
            if span[3] and isinstance(span[3][-1], str): span[3][-1] += text
            else: span[3].append(text)
        else: span[3].append([text, width])
        span[0] += width
    return [[start, fg, bg, runs] for _, fg, bg, runs, start in spans]

def variants(trace, mode):
    previous = {}; receiver = {}; last = None
    for phase, frame in trace:
        if frame == last: continue
        last = frame
        expected = {y: cells(row) for y, row in frame['rows']}
        new = dict(frame)
        if mode == 'rows':
            new['rows'] = [(y,row) for y,row in frame['rows'] if expected[y] != previous.get(y)]
        elif mode == 'cells':
            rows = []
            for y, row in expected.items():
                old = previous.get(y, {})
                dirty = [(x,c) for x,c in row.items() if c[3] and any(old.get(x+i) != row.get(x+i) for i in range(c[3]))]
                if dirty: rows.append((y,pack(dirty)))
            new['rows'] = rows
        for y,row in new['rows']:
            if mode != 'cells': receiver[y] = cells(row)
            else: receiver.setdefault(y, {}).update(cells(row))
        assert receiver == expected, (mode, phase, 'receiver mismatch')
        previous = expected
        yield phase, json.dumps(new, ensure_ascii=False, separators=(',',':')).encode()

def measure(messages, compression):
    encoder = zlib.compressobj(8, zlib.DEFLATED, -15)
    decoder = zlib.decompressobj(-15)
    by_phase = collections.Counter(); elapsed = 0
    for phase, raw in messages:
        start = time.perf_counter()
        if compression == 'reset':
            encoder = zlib.compressobj(8, zlib.DEFLATED, -15)
            decoder = zlib.decompressobj(-15)
        data = raw if compression == 'none' else (encoder.compress(raw) + encoder.flush(zlib.Z_SYNC_FLUSH))[:-4]
        elapsed += time.perf_counter()-start
        if compression != 'none': assert decoder.decompress(data+b'\x00\x00\xff\xff') == raw
        header = 2 if len(data)<126 else 4 if len(data)<65536 else 10
        by_phase[phase] += len(data)+header
    return dict(bytes=sum(by_phase.values()), phases=dict(by_phase), compress_ms=elapsed*1000)

def main():
    traces = collections.defaultdict(list)
    for line in open(sys.argv[1]):
        if line.strip()=="Resolving dependencies...": continue
        event=json.loads(line); traces[tuple(event['screen'])].append((event['phase'],event['frame']))
    output = {}
    for size, trace in traces.items():
        cases = [('full raw','full','none'),('full deflate','full','context'),('full fresh deflate','full','reset'),('row patches deflate','rows','context'),('cell patches deflate','cells','context')]
        results = {}
        for name,mode,compression in cases:
            messages=list(variants(trace,mode))
            runs=[measure(messages,compression) for _ in range(3)]
            results[name]={**runs[0], 'compress_ms':round(statistics.median(r['compress_ms'] for r in runs),2),'frames':len(messages)}
        output[f'{size[0]}x{size[1]}'] = results
    print(json.dumps(output,indent=2))
if __name__ == '__main__': main()
