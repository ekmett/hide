// Exercise the production read-only sidebar consumer with its bounded DOM seam.
import fs from 'node:fs';
import vm from 'node:vm';
import assert from 'node:assert/strict';
const source=fs.readFileSync('assets/web/editor.js','utf8');
const start=source.indexOf('// Complete sidebar metadata');
assert.ok(start>=0,'browser semantic sidebar consumer is present');
class Element {
  constructor(tag='div'){this.tag=tag;this.children=[];this.attributes=new Map();this.dataset={};this.events=new Map();this.hidden=false;}
  setAttribute(name,value){this.attributes.set(name,String(value));}
  getAttribute(name){return this.attributes.get(name)??null;}
  removeAttribute(name){this.attributes.delete(name);}
  addEventListener(name,callback){this.events.set(name,callback);}
  append(...children){for(const child of children){child.parentElement=this;this.children.push(child);}}
  replaceChildren(...children){this.children=[];this.append(...children);}
  querySelectorAll(){return this.children.flatMap(child=>[...(child.getAttribute('role')==='treeitem'?[child]:[]),...child.querySelectorAll()]);}
}
const sidebarAccess=new Element(),document={activeElement:{},createElement:tag=>new Element(tag)};
assert.match(fs.readFileSync('assets/web/index.html','utf8'),/id="semantic-sidebar"[^>]*role="tree"[^>]*aria-description="Read-only visible sidebar/);
sidebarAccess.setAttribute('role','tree');sidebarAccess.hidden=true;
const packets=[];
const context=vm.createContext({sidebarAccess,document,status:{textContent:''},cols:80,lines:25,send:packet=>packets.push(packet)});
vm.runInContext(source.slice(start,source.indexOf('// Authorized download bytes',start)),context);
const token='a'.repeat(48),id=name=>['tree',token,name];
const root={id:['sidebar'],parent:null,role:'tree',name:'Sidebar',bounds:null,selected:false,focused:false,expanded:null,loading:false,childrenKnown:1,moreChildren:false,index:null,generation:1,level:0,posInSet:0,setSize:1};
const folder={...root,id:id('folder'),parent:root.id,role:'treeitem',name:'Files',bounds:[1,2,22,1],expanded:true,index:0,level:1,posInSet:1};
const file={...root,id:id('file'),parent:folder.id,role:'treeitem',name:'λ <script>.hs',bounds:[1,3,22,1],selected:true,index:1,level:2,posInSet:1};
const snapshot={revision:1,layout:[80,25,24,0,1,1,0],visibleStart:0,visibleCount:2,logicalRows:2,readOnly:true,nodes:[root,folder,file]};
function receive(value){context.value=value;vm.runInContext('receiveSidebar(value)',context);}
function items(){return sidebarAccess.querySelectorAll();}
function key(name){const event={key:name,preventDefault(){this.prevented=true;}};sidebarAccess.events.get('keydown')(event);return event;}
receive(snapshot);assert.equal(sidebarAccess.hidden,false);
assert.equal(items().length,2);assert.equal(items()[1].children[0].textContent,file.name);
assert.equal(items()[0].getAttribute('aria-label'),'Files');assert.equal(items()[1].getAttribute('aria-label'),file.name);
assert.equal(items()[1].parentElement.getAttribute('role'),'group');assert.equal(items()[1].parentElement.parentElement,items()[0]);
assert.equal(items()[1].getAttribute('aria-selected'),'true');assert.equal(items()[1].getAttribute('aria-level'),'2');
assert.equal(items()[0].getAttribute('aria-expanded'),'true');assert.equal(items()[1].getAttribute('aria-expanded'),null);
document.activeElement=sidebarAccess;sidebarAccess.events.get('focus')();
assert.equal(sidebarAccess.getAttribute('aria-activedescendant'),items()[1].id);
assert.equal(key('ArrowLeft').prevented,true);assert.equal(sidebarAccess.getAttribute('aria-activedescendant'),items()[0].id);
key('ArrowRight');assert.equal(sidebarAccess.getAttribute('aria-activedescendant'),items()[1].id);key('Home');key('End');
assert.equal(packets.length,0);assert.equal(items()[1].getAttribute('aria-selected'),'true'); // Local reading focus grants no selection/action.
receive({...snapshot,nodes:[root,{...folder,loading:true,moreChildren:true}, {...file,name:'replacement.hs',selected:false}]});
assert.equal(items()[1].children[0].textContent,'replacement.hs');assert.equal(items()[1].getAttribute('aria-selected'),'false');
assert.equal(items()[0].getAttribute('aria-busy'),'true'); // Same revision still replaces names/selection/privacy.
receive({...snapshot,visibleStart:1,visibleCount:1,nodes:[root,{...folder,bounds:null,loading:true,moreChildren:true}]});
assert.equal(sidebarAccess.hidden,false);assert.equal(items().length,1);
assert.equal(items()[0].getAttribute('aria-busy'),'true');assert.match(items()[0].getAttribute('aria-description'),/Children are not fully loaded/);
// A visible state row has no exported placeholder text; its offscreen parent still explains loading.
receive({...snapshot,nodes:[root]});assert.equal(sidebarAccess.hidden,true);
receive({...snapshot,nodes:[]});assert.equal(sidebarAccess.hidden,true);assert.equal(items().length,0);
receive({...snapshot,visibleStart:65535,visibleCount:1,logicalRows:65536,nodes:[root,{...folder,bounds:null},{...file,index:65535}]});
assert.equal(sidebarAccess.hidden,false);assert.equal(items().length,2); // Node and state rows share the cached row budget.
receive(snapshot);vm.runInContext('clearSidebar()',context);assert.equal(sidebarAccess.hidden,true);assert.equal(items().length,0);
for(const invalid of [
  {...snapshot,logicalRows:65537},
  {...snapshot,nodes:[root,folder,{...file,bounds:[79,3,22,1]}]},
  {...snapshot,nodes:[root,folder,{...file,name:'x'.repeat(257)}]},
  {...snapshot,nodes:[root,{...folder,parent:file.id},file]},
  {...snapshot,nodes:Array(513).fill(root)},
]){receive(snapshot);assert.throws(()=>receive(invalid),/Invalid sidebar/);assert.equal(sidebarAccess.hidden,true);assert.equal(items().length,0);}
assert.equal(packets.length,0);
// Use the production socket/frame merger to exercise delta, reset and reconnect invalidation.
const sockets=[],noOp=()=>{};
Object.assign(context,{URL,ArrayBuffer,location:{href:'http://localhost/'},navigator:{platform:'MacIntel'},
 WebSocket:class {constructor(){sockets.push(this);}close(){this.onclose({code:1000,reason:''});}},
 closed:false,ready:false,glyphs:new Map(),tiles:new Map(),atlasEntries:new Map(),socket:null,
 downloadInfo:null,frame:null,mode:3,rows:[],unsaved:false,clipboard:'',mouse:[-1,-1],dirty:false,
 systemTheme:{matches:false},performance:{now:()=>0},console:{info:noOp},setTimeout:noOp,
 clearClipboardRequest:noOp,decodeRows:rows=>rows,updateTitle:noOp,guardLeave:noOp,allocate:()=>true,
 drawRows:noOp,resize:noOp});
vm.runInContext(source.slice(source.indexOf('function connect(){'),source.indexOf('function point(e)')),context);
async function message(packet){sockets.at(-1).onmessage({data:JSON.stringify(packet)});await new Promise(resolve=>setImmediate(resolve));}
await message({type:'frame',size:[80,25],rows:[],mode:3,semanticSidebar:snapshot});
assert.equal(sidebarAccess.hidden,false);
await message({type:'frame',rows:[],semanticSidebar:{...snapshot,nodes:[root,folder,{...file,name:'same revision, new privacy mask'}]}});
assert.equal(items()[1].children[0].textContent,'same revision, new privacy mask');
await message({type:'frame',rows:[]});assert.equal(sidebarAccess.hidden,false); // An omitted delta retains current semantics.
await message({type:'frame',rows:[],reset:true});assert.equal(sidebarAccess.hidden,true);
await message({type:'frame',rows:[],reset:true,semanticSidebar:snapshot});assert.equal(sidebarAccess.hidden,false);
await message({type:'assets',glyphs:[]});assert.equal(sidebarAccess.hidden,true);
await message({type:'frame',rows:[],semanticSidebar:snapshot});await message({type:'connection',connected:false});
assert.equal(sidebarAccess.hidden,true);
await message({type:'frame',rows:[],semanticSidebar:snapshot});sockets.at(-1).close();assert.equal(sidebarAccess.hidden,true);
vm.runInContext('connect()',context);
await message({type:'frame',size:[80,25],rows:[],semanticSidebar:snapshot});assert.equal(sidebarAccess.hidden,false);
assert.ok(packets.every(packet=>['frontend','theme'].includes(packet.type))); // Projection handling emits no actions.
console.log('Browser sidebar hierarchy, read-only navigation, replacement, bounds and connection lifecycle checks passed');
