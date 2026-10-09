// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
// Exercise the production resource handler, including delayed WebSocket replies
// for which browsers no longer grant popup activation.
import fs from 'node:fs';
import vm from 'node:vm';
import assert from 'node:assert/strict';
const source=fs.readFileSync('assets/web/editor.js','utf8');
const handler=source.slice(source.indexOf('let pendingResource='),source.indexOf('// A stored, nonfinal DEFLATE'));
let blocked=false,click,opened=[],blobs=[],revoked=[],timers=[];
class ResourceURL extends URL {
  static createObjectURL(blob){blobs.push(blob);return `blob:test-${blobs.length}`;}
  static revokeObjectURL(url){revoked.push(url);}
}
const action={hidden:true,addEventListener:(_,callback)=>click=callback};
const status={textContent:''};
const context=vm.createContext({URL:ResourceURL,Blob,atob,Uint8Array,resourceAction:action,status,
  setTimeout:callback=>timers.push(callback),window:{open:(url,target)=>{
    assert.equal(url,'about:blank');assert.equal(target,'_blank');if(blocked)return null;
    const tab={opener:{},location:{replace:url=>tab.url=url},document:{createElement:name=>({name,style:{}}),body:{appendChild:img=>tab.img=img}}};
    opened.push(tab);return tab;
  }}});
vm.runInContext(handler,context);
function receive(packet){context.packet=packet;vm.runInContext('receiveResource(packet)',context);}
receive({url:'https://example.com/a?b=1&c=2'});
assert.equal(opened.at(-1).url,'https://example.com/a?b=1&c=2');assert.equal(opened.at(-1).opener,null);
blocked=true;receive({url:'https://example.com/blocked'});
assert.equal(action.hidden,false);assert.match(status.textContent,/Click Open link/);
blocked=false;click();assert.equal(opened.at(-1).url,'https://example.com/blocked');assert.equal(action.hidden,true);
receive({mime:'image/png',data:'iVBORw=='});
assert.equal(blobs.at(-1).type,'image/png');assert.deepEqual([...new Uint8Array(await blobs.at(-1).arrayBuffer())],[137,80,78,71]);
assert.equal(opened.at(-1).img.src,'blob:test-1');assert.equal(opened.at(-1).url,undefined);
receive({mime:'image/svg+xml',data:btoa('<svg xmlns="http://www.w3.org/2000/svg"><script>alert(1)</script></svg>')});
assert.equal(opened.at(-1).img.name,'img');assert.equal(opened.at(-1).url,undefined); // SVG never becomes active page content.
receive({mime:'application/pdf',data:btoa('%PDF')});assert.equal(opened.at(-1).url,'blob:test-3');
const count=opened.length;
for(const packet of [{url:'javascript:alert(1)'},{url:'data:text/html,bad'},{mime:'text/html',data:'YWJj'},{mime:'image/png',data:'!bad!'}])receive(packet);
assert.equal(opened.length,count);
blocked=true;receive({mime:'image/png',data:'iVBORw=='});receive({url:'https://example.com/replacement'});
assert.ok(revoked.includes('blob:test-4'));timers.forEach(callback=>callback());
assert.ok(revoked.includes('blob:test-1'));
console.log('Browser link, popup fallback, image and URL validation checks passed');
