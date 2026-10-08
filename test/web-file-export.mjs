// Exercise the production authorized-byte export handler without a GPU/session.
import fs from 'node:fs';
import vm from 'node:vm';
import assert from 'node:assert/strict';
const source=fs.readFileSync('assets/web/editor.js','utf8');
const start=source.indexOf('// Authorized download bytes');
assert.ok(start>=0,'browser file export handler is present');
const handler=source.slice(start,source.indexOf('const systemTheme=',start));
const events=new Map(),blobs=[],revoked=[],timers=[];
let clicks=0;
class ExportURL extends URL {
  static createObjectURL(blob){blobs.push(blob);return `blob:export-${blobs.length}`;}
  static revokeObjectURL(url){revoked.push(url);}
}
const action={hidden:true,addEventListener:(name,callback)=>events.set(name,callback),click:()=>clicks++};
const status={textContent:''};
const context=vm.createContext({URL:ExportURL,Blob,downloadAction:action,status,
  setTimeout:(callback,delay)=>{timers.push({callback,delay});return timers.length;},
  clearTimeout:()=>{}});
vm.runInContext(handler,context);
function receive(name,purpose,bytes=new Uint8Array([0,255,13,10,128])){
  context.metadata={name,purpose};context.bytes=bytes;
  vm.runInContext('receiveDownload(metadata,bytes)',context);
}
function drag(){
  const data=new Map([['text/uri-list','http://editor.invalid/']]),event={prevented:false,preventDefault(){this.prevented=true;},dataTransfer:{clearData:()=>data.clear(),setData:(type,value)=>data.set(type,value)}};
  events.get('dragstart')(event);return {data,event};
}
receive("a b ' λ.bin",'file-export');
assert.equal(clicks,0);assert.equal(action.hidden,false);assert.equal(action.download,"a b ' λ.bin");
assert.deepEqual([...new Uint8Array(await blobs[0].arrayBuffer())],[0,255,13,10,128]);
const first=drag();
assert.equal(first.event.dataTransfer.effectAllowed,'copy');
assert.deepEqual([...first.data],[['DownloadURL',"application/octet-stream:a b ' λ.bin:blob:export-1"]]);
events.get('dragend')({dataTransfer:{dropEffect:'none'}});
assert.equal(timers[0].delay,5*60*1000);
assert.match(status.textContent,/cancelled/i);assert.equal(clicks,0);
receive('next.bin','file-export');assert.ok(!revoked.includes('blob:export-1'));
receive('automatic.bin',undefined);assert.equal(clicks,1);assert.ok(revoked.includes('blob:export-2'));
timers[0].callback();assert.ok(revoked.includes('blob:export-1'));assert.ok(!revoked.includes('blob:export-3'));
for(let i=0;i<4;i++){receive(`held-${i}.bin`,'file-export');assert.equal(drag().event.prevented,false);events.get('dragend')({dataTransfer:{dropEffect:'copy'}});}
receive('fallback.bin','file-export');assert.equal(drag().event.prevented,true);
assert.match(status.textContent,/Download/);assert.equal(action.download,'fallback.bin');
events.get('click')();assert.match(status.textContent,/unchanged/);
receive('/host/private/a:b\n.bin','file-export');assert.equal(action.download,'a_b_.bin');
receive('..','file-export');assert.equal(action.download,'download');
const lastURL=action.href;
vm.runInContext('clearDownload()',context);assert.equal(action.hidden,true);assert.equal(action.href,'');
assert.ok(revoked.includes('blob:export-3'));assert.ok(revoked.includes(lastURL));
console.log('Browser binary export, Download fallback, copy-only drag and URL lifetime checks passed');
