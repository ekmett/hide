#!/usr/bin/env python3
"""Exercise HOST:PATH, SSH bootstrap, browser framing, clipboard and reattachment.
Uses a local SSH stand-in; authentication itself remains OpenSSH's responsibility.
"""
import base64, hashlib, json, os, pathlib, re, select, socket, struct, subprocess, sys, tempfile, time, urllib.parse, zlib

class WebSocket:
    def __init__(self, url):
        self.attachment=0
        parsed=urllib.parse.urlsplit(url)
        self.sock=socket.create_connection((parsed.hostname,parsed.port),timeout=10)
        self.sock.settimeout(15)
        key=base64.b64encode(os.urandom(16)).decode()
        self.sock.sendall((f'GET {parsed.path}socket HTTP/1.1\r\nHost: {parsed.netloc}\r\nOrigin: http://{parsed.netloc}\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Key: {key}\r\nSec-WebSocket-Version: 13\r\n\r\n').encode())
        response=b''
        while not response.endswith(b'\r\n\r\n'): response+=self.exact(1)
        assert response.startswith(b'HTTP/1.1 101 '), response
        expected=base64.b64encode(hashlib.sha1((key+'258EAFA5-E914-47DA-95CA-C5AB0DC85B11').encode()).digest())
        assert expected.lower() in response.lower()
    def exact(self,n):
        result=b''
        while len(result)<n:
            chunk=self.sock.recv(n-len(result))
            if not chunk: raise EOFError('WebSocket ended')
            result+=chunk
        return result
    def send(self,payload,kind=1):
        if not isinstance(payload,bytes): payload=json.dumps(dict(payload,attachment=self.attachment)).encode()
        n=len(payload);mask=os.urandom(4)
        header=bytes([0x80|kind,0x80|(n if n<126 else 126 if n<65536 else 127)])
        if n>=126: header+=struct.pack('!H' if n<65536 else '!Q',n)
        self.sock.sendall(header+mask+bytes(b^mask[i%4] for i,b in enumerate(payload)))
    def read(self):
        data=b'';kind=None
        while True:
            a,b=self.exact(2);opcode=a&15;n=b&127
            if n==126: n=struct.unpack('!H',self.exact(2))[0]
            if n==127: n=struct.unpack('!Q',self.exact(8))[0]
            assert n<=16777216
            mask=self.exact(4) if b&128 else None
            payload=self.exact(n)
            if mask: payload=bytes(v^mask[i%4] for i,v in enumerate(payload))
            if opcode==9: self.send(payload,10);continue
            if opcode==8: raise EOFError('WebSocket closed')
            if opcode==10: continue
            if opcode: kind=opcode
            data+=payload
            if a&128: return kind,data
    def close(self): self.sock.close()

class Display:
    def __init__(self,ws): self.ws=ws;self.rows=[];self.meta={}
    def read(self):
        kind,payload=self.ws.read()
        if kind==1:
            message=json.loads(payload)
            if "attachment" in message: self.ws.attachment=message["attachment"]
            if message.get("type")=="session": self.rows=[];self.meta={}
            return message
        tag=payload[0]
        dictionary=json.dumps(self.rows,ensure_ascii=False,separators=(',',':')).encode()[-32768:] if tag else b''
        decoder=zlib.decompressobj(wbits=-15,**({'zdict':dictionary} if dictionary else {}))
        frame=json.loads(decoder.decompress(payload[1:])+decoder.flush())
        if tag!=2: self.rows=[]
        for y,row in frame['rows']:
            while len(self.rows)<=y: self.rows.append(None)
            self.rows[y]=row
        self.meta.update(frame)
        return frame
    def until(self,kind,predicate=lambda _:True):
        for _ in range(200):
            message=self.read()
            if message.get('type')==kind and predicate(message): return message
        raise AssertionError('No expected '+kind)
    def text_rows(self):
        return [''.join(run if isinstance(run,str) else run[0] for span in (row or []) for run in span[4]) for row in self.rows]

binary=str(pathlib.Path(sys.argv[1]).resolve())
with tempfile.TemporaryDirectory(prefix='thc-remote-browser-') as directory:
    root=pathlib.Path(directory);source=root/"a b ' λ.txt";source.write_text('original\n')
    saved=root/"a b ' λ.bin";saved_bytes=bytes(range(256));saved.write_bytes(saved_bytes)
    ssh=root/'ssh'
    ssh.write_text('#!/usr/bin/env python3\nimport os,sys\nassert sys.argv[-1]=="hide --remote",sys.argv\nos.execv(os.environ["THC_TEST_EXE"],[os.environ["THC_TEST_EXE"],"--remote"])\n')
    ssh.chmod(0o700)
    env=dict(os.environ,PATH=str(root)+os.pathsep+os.environ['PATH'],THC_TEST_EXE=binary,THC_EDIT_WEB_OPEN='0',hide_datadir=str(pathlib.Path(__file__).resolve().parents[1]))
    with open(root/'log','w+') as log:
        process=subprocess.Popen([binary,'--web',"test-host:"+str(source)],env=env,cwd=root,stdout=subprocess.DEVNULL,stderr=log)
        ws=None
        try:
            url=None
            for _ in range(150):
                log.flush();log.seek(0);text=log.read()
                found=re.search(r'Haskell browser: (http://\S+)',text)
                if found: url=found.group(1);break
                if process.poll() is not None: raise AssertionError(text)
                time.sleep(.1)
            assert url,text
            ws=WebSocket(url);display=Display(ws)
            display.until('frame')
            assert source.name in display.meta['title'],display.meta
            help_action=next(item for item in display.meta['menuContributions'] if item['id']=='hide.help.contents')
            ws.send({'type':'command','command':'hide.edit.select-all','seq':1});display.until('ack',lambda m:m['seq']==1)
            ws.send({'type':'paste','text':'remote λ\n','seq':2});display.until('ack',lambda m:m['seq']==2)
            ws.send({'type':'key','key':'F2','seq':3});display.until('ack',lambda m:m['seq']==3)
            assert source.read_text()=='remote λ\n'
            ws.send({'type':'command','command':'hide.edit.select-all','seq':4});display.until('ack',lambda m:m['seq']==4)
            ws.send({'type':'command','command':'hide.edit.copy','seq':5});assert display.until('copy')['text']=='remote λ\n'
            display.until('ack',lambda m:m['seq']==5)
            ws.close();ws=None;time.sleep(.3)
            ws=WebSocket(url);display=Display(ws);display.until('frame')
            ws.send({'type':'command','command':'hide.edit.copy','seq':1});assert display.until('copy')['text']=='remote λ\n'
            display.until('ack',lambda m:m['seq']==1)
            ws.send({'type':'command','command':'hide.file.download','seq':2})
            display.until('download');kind,blob=ws.read();assert kind==2 and blob==b'remote \xce\xbb\n'
            display.until('ack',lambda m:m['seq']==2)
            assert next(item for item in display.meta['menuContributions'] if item['id']=='hide.help.contents')['generation']==help_action['generation']
            ws.send({'type':'menu','command':help_action['id'],'registry':help_action['registry'],'generation':help_action['generation']+1,'seq':3})
            display.until('ack',lambda m:m['seq']==3)
            assert source.name in display.meta['title'],display.meta
            ws.send({'type':'menu','command':help_action['id'],'registry':help_action['registry'],'generation':help_action['generation'],'seq':4})
            display.until('ack',lambda m:m['seq']==4)
            display.until('frame',lambda _:'hide Help' in display.meta['title'])
            assert source.read_text()=='remote λ\n'
            ws.send({'type':'menu','command':'hide.window.close','seq':5});display.until('ack',lambda m:m['seq']==5)
            showing_files=any(saved.name in row for row in display.text_rows())
            ws.send({'type':'blur','seq':6} if showing_files else {'type':'menu','command':'hide.view.files','seq':6})
            display.until('ack',lambda m:m['seq']==6)
            if not any(saved.name in row for row in display.text_rows()):
                display.until('frame',lambda _:any(saved.name in row for row in display.text_rows()))
            row=next(y for y,text in enumerate(display.text_rows()) if saved.name in text)
            ws.send({'type':'mouse','action':'down','button':2,'x':4,'y':row,'seq':7});display.until('ack',lambda m:m['seq']==7)
            if not any('Export saved copy' in text for text in display.text_rows()):
                display.until('frame',lambda _:any('Export saved copy' in text for text in display.text_rows()))
            export_row=next(y for y,text in enumerate(display.text_rows()) if 'Export saved copy' in text)
            ws.send({'type':'mouse','action':'down','x':6,'y':export_row,'seq':8})
            exported=display.until('download')
            assert set(exported)=={'type','purpose','name','row','view'},exported
            assert exported['type']=='download' and exported['purpose']=='file-export' and exported['name']==saved.name,exported
            assert len(exported['row'])==4 and exported['row'][2]>0 and exported['row'][3]==1,exported
            assert len(exported['view'])==13 and all(isinstance(value,int) for value in exported['view']),exported
            kind,blob=ws.read();assert kind==2 and blob==saved_bytes
            assert saved.read_bytes()==saved_bytes and source.read_text()=='remote λ\n'
            assert not display.meta['dirty'],display.meta
            ws.send({'type':'command','command':'hide.app.quit','seq':9});display.until('closed')
            code=process.wait(timeout=10)
            log.flush();log.seek(0)
            assert code==0,log.read()
            print('remote browser bridge checks passed')
        finally:
            if ws: ws.close()
            if process.poll() is None: process.terminate();process.wait(timeout=10)
