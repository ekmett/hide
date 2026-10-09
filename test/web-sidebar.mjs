// Exercise the production read-only sidebar consumer with its bounded DOM seam.
import fs from 'node:fs';
import vm from 'node:vm';
import assert from 'node:assert/strict';
const source=fs.readFileSync('assets/web/editor.js','utf8');
const start=source.indexOf('// Complete sidebar metadata');
assert.ok(start>=0,'browser semantic sidebar consumer is present');
class Element {
  constructor(tag='div'){this.tag=tag;this.tagName=tag.toUpperCase();this.children=[];this.attributes=new Map();this.dataset={};this.events=new Map();this.hidden=false;}
  setAttribute(name,value){this.attributes.set(name,String(value));}
  getAttribute(name){return this.attributes.get(name)??null;}
  removeAttribute(name){this.attributes.delete(name);}
  addEventListener(name,callback){this.events.set(name,callback);}
  append(...children){for(const child of children){child.parentElement=this;this.children.push(child);}}
  replaceChildren(...children){for(const child of this.children)child.parentElement=null;this.children=[];this.append(...children);}
  remove(){if(this.parentElement)this.parentElement.children=this.parentElement.children.filter(child=>child!==this);this.parentElement=null;}
  insertBefore(child,before){child.remove();child.parentElement=this;this.children.splice(this.children.indexOf(before),0,child);}
  contains(target){return target===this||this.children.some(child=>child.contains(target));}
  querySelectorAll(){return this.children.flatMap(child=>[...(child.getAttribute('role')==='treeitem'?[child]:[]),...child.querySelectorAll()]);}
}
const sidebarAccess=new Element(),dialogAccess=new Element(),imageAccess=new Element(),document={activeElement:{},createElement:tag=>new Element(tag)};
assert.match(fs.readFileSync('assets/web/index.html','utf8'),/id="semantic-sidebar"[^>]*role="tree"[^>]*aria-description="Read-only visible sidebar/);
sidebarAccess.setAttribute('role','tree');sidebarAccess.hidden=true;
const packets=[];
const context=vm.createContext({sidebarAccess,dialogAccess,imageAccess,document,status:{textContent:''},cols:80,lines:25,send:packet=>packets.push(packet)});
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
// Current modal semantics reuse this production bounded DOM seam.
const dialogRoot={id:['dialog'],parent:null,role:'dialog',name:'Safe λ <script> dialog',value:null,bounds:[4,2,60,20],focused:false,checked:null,selected:null,expanded:null,multiline:false};
const field={...dialogRoot,id:['dialog','field','0'],parent:dialogRoot.id,role:'textbox',name:'Current text',value:'λ <script> value',bounds:[7,5,54,3],focused:true};
const checkbox={...field,id:['dialog','field','1'],role:'checkbox',name:'Auto indent',value:null,checked:true,focused:false,bounds:[7,9,54,1]};
const button={...field,id:['dialog','button','0'],role:'button',name:'Accept',value:null,focused:false,bounds:[30,20,10,1]};
const modal={present:true,readOnly:true,truncated:false,nodes:[dialogRoot,field,checkbox,button]};
function dialog(value){context.value=value;vm.runInContext('receiveDialog(value)',context);}
function dialogItems(){return dialogAccess.children;}
const priorFocus=document.activeElement;
dialog(modal);assert.equal(dialogAccess.hidden,false);assert.equal(document.activeElement,priorFocus);
assert.equal(dialogAccess.getAttribute('aria-label'),dialogRoot.name);
assert.equal(dialogItems()[0].value,field.value);assert.equal(dialogItems()[0].readOnly,true);assert.equal(dialogItems()[0].tabIndex,-1);
assert.match(dialogItems()[0].getAttribute('aria-description'),/Focused in editor/);
assert.equal(dialogItems()[1].getAttribute('aria-checked'),'true');assert.equal(dialogItems()[2].getAttribute('role'),'button');
assert.equal(sidebarAccess.getAttribute('aria-hidden'),'true');assert.equal(imageAccess.getAttribute('aria-hidden'),'true');
dialog({...modal,nodes:[dialogRoot,{...field,value:'Replacement at same structural ID',multiline:true},checkbox,button]});
assert.equal(dialogItems()[0].value,'Replacement at same structural ID');assert.equal(dialogItems()[0].getAttribute('aria-multiline'),'true');
const readingField=dialogItems()[0];document.activeElement=readingField;
dialog({...modal,nodes:[dialogRoot,{...field,value:'Changed while reading'},checkbox,button]});
assert.equal(dialogItems()[0],readingField);assert.equal(document.activeElement,readingField);assert.equal(readingField.value,'Changed while reading');
const combo={...field,id:['dialog','field','2'],role:'combobox',name:'Encoding',value:'UTF-8',expanded:true};
const option={...field,id:['dialog','field','2','option','0'],parent:combo.id,role:'option',name:'UTF-8',value:null,selected:true,focused:false};
document.activeElement=dialogAccess;dialog({...modal,nodes:[dialogRoot,combo,option]});
const readingCombo=dialogItems()[0].children[0],comboWrapper=dialogItems()[0];document.activeElement=readingCombo;
dialog({...modal,nodes:[dialogRoot,{...combo,value:'UTF-16'}, {...option,name:'UTF-16',selected:false}]});
assert.equal(dialogItems()[0],comboWrapper);assert.equal(comboWrapper.children[0],readingCombo);assert.equal(document.activeElement,readingCombo);
assert.equal(readingCombo.value,'UTF-16');assert.equal(comboWrapper.children[1].children[0].getAttribute('aria-selected'),'false');

dialog({...modal,nodes:[]});assert.equal(dialogAccess.hidden,true);assert.equal(dialogItems().length,0);
assert.equal(sidebarAccess.getAttribute('aria-hidden'),'true'); // A private current modal still covers the underlying surface.
dialog({present:false,readOnly:true,truncated:false,nodes:[]});assert.equal(sidebarAccess.getAttribute('aria-hidden'),null);assert.equal(imageAccess.getAttribute('aria-hidden'),null);
for(const invalid of [
 {...modal,readOnly:false},{...modal,present:false}, {...modal,nodes:[dialogRoot,{...field,id:['dialog','field',0]}]}, {...modal,nodes:[dialogRoot,{...field,value:'x'.repeat(2049)}]},
 {...modal,nodes:[dialogRoot,{...field,name:'x'.repeat(257)}]}, {...modal,nodes:[dialogRoot,{...field,bounds:[79,2,2,1]}]},
 {...modal,nodes:[dialogRoot,{...field,parent:field.id}]},
 {...modal,nodes:[dialogRoot,...Array.from({length:17},(_,i)=>({...field,id:['dialog','field',String(i)],value:'x'.repeat(2048)}))]}, {...modal,nodes:Array(257).fill(dialogRoot)},
]){dialog(modal);assert.throws(()=>dialog(invalid),/Invalid dialog/);assert.equal(dialogAccess.hidden,true);assert.equal(dialogItems().length,0);}
assert.equal(packets.length,0);
// Use the production socket/frame merger to exercise delta, reset and reconnect invalidation.
const sockets=[],noOp=()=>{};
Object.assign(context,{URL,ArrayBuffer,location:{href:'http://localhost/'},navigator:{platform:'MacIntel'},
 WebSocket:class {static OPEN=1;constructor(){this.readyState=1;this.sent=[];sockets.push(this);}send(value){this.sent.push(value);}close(){this.readyState=3;this.onclose({code:1000,reason:''});}},
 closed:false,ready:false,sessionFrontend:false,peerAttachment:0,attachmentEpoch:0,glyphs:new Map(),tiles:new Map(),atlasEntries:new Map(),socket:null,
 images:{chunk:null,clear:noOp,receive:noOp,describe:noOp},downloadInfo:null,frame:null,mode:3,rows:[],unsaved:false,clipboard:'',mouse:[-1,-1],dirty:false,
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
await message({type:'frame',rows:[],semanticDialog:modal});assert.equal(dialogAccess.hidden,false);
await message({type:'frame',rows:[]});assert.equal(dialogAccess.hidden,false); // Omitted metadata retains the complete snapshot.
await message({type:'frame',rows:[],semanticDialog:{present:false,readOnly:true,truncated:false,nodes:[]}});assert.equal(dialogAccess.hidden,true);
await message({type:'frame',rows:[],semanticDialog:modal});await message({type:'frame',rows:[],reset:true});assert.equal(dialogAccess.hidden,true);
await message({type:'frame',rows:[],semanticDialog:modal});await message({type:'assets',glyphs:[]});assert.equal(dialogAccess.hidden,true);
await message({type:'frame',rows:[],semanticDialog:modal});await message({type:'connection',connected:false});assert.equal(dialogAccess.hidden,true);
await message({type:'frame',rows:[],semanticDialog:modal});sockets.at(-1).close();assert.equal(dialogAccess.hidden,true);
assert.ok(packets.every(packet=>['frontend','theme'].includes(packet.type))); // Projection handling emits no actions.
console.log('Browser sidebar/dialog semantics, read-only navigation, replacement, bounds and connection lifecycle checks passed');

// Ordinary human drops retain exact file bytes and their local attachment receipt.
const dropHandlers=new Map();let inputFocus=0;
Object.assign(context,{window:{addEventListener:(name,handler)=>dropHandlers.set(name,handler)},
 input:{focus:()=>inputFocus++},serial:0,pendingEdit:0,detaching:false});
vm.runInContext(source.slice(source.indexOf('function send(value)'),source.indexOf('function mods(e)')),context);
vm.runInContext(source.slice(source.indexOf('function semanticReadingTarget('),source.indexOf("window.addEventListener('keydown'")),context);
vm.runInContext(source.slice(source.indexOf("window.addEventListener('dragover'"),source.indexOf("fullscreen.addEventListener('click'")),context);
vm.runInContext('connect()',context);await message({type:'assets',glyphs:[[65,1,[0]]],scale:2});
const pngBytes=Uint8Array.from([137,80,78,71,13,10,26,10,0,255,128]).buffer;
const uploadFile={name:'safe λ.unknown',type:'text/plain',size:pngBytes.byteLength,arrayBuffer:async()=>pngBytes};
function drop(files,target={}){return dropHandlers.get('drop')({target,preventDefault(){},dataTransfer:{files}});}
function uploads(){return sockets.flatMap(socket=>socket.sent).filter(value=>typeof value!=='string'||JSON.parse(value).type==='upload');}
function clearSent(){for(const socket of sockets)socket.sent.length=0;inputFocus=0;}
clearSent();await drop([uploadFile]);
assert.equal(uploads().length,2);assert.equal(JSON.parse(uploads()[0]).name,uploadFile.name);assert.equal(JSON.parse(uploads()[0]).type,'upload');
assert.deepEqual(Array.from(new Uint8Array(uploads()[1])),Array.from(new Uint8Array(pngBytes)));assert.equal(inputFocus,1);
// Browser MIME/extension hints do not decide whether the host opens an image.
clearSent();await drop([{...uploadFile,size:16777217,arrayBuffer:()=>{throw Error('oversize file was read');}}]);assert.equal(uploads().length,0);
clearSent();await drop([uploadFile],dialogAccess);assert.equal(uploads().length,0);assert.equal(inputFocus,0);
let finishRead;
clearSent();const oldSocketDrop=drop([{...uploadFile,arrayBuffer:()=>new Promise(resolve=>{finishRead=resolve;})}]);
sockets.at(-1).close();vm.runInContext('connect()',context);await message({type:'assets',glyphs:[[65,1,[0]]],scale:2});clearSent();
finishRead(pngBytes);await oldSocketDrop;assert.equal(uploads().length,0);assert.equal(inputFocus,0);
clearSent();const relayDrop=drop([{...uploadFile,arrayBuffer:()=>new Promise(resolve=>{finishRead=resolve;})}]);
await message({type:'connection',connected:false});await message({type:'connection',connected:true});clearSent();
finishRead(pngBytes);await relayDrop;assert.equal(uploads().length,0);assert.equal(inputFocus,0);
clearSent();const resetDrop=drop([{...uploadFile,arrayBuffer:()=>new Promise(resolve=>{finishRead=resolve;})}]);
await message({type:'assets',glyphs:[[65,1,[0]]],scale:2});clearSent();finishRead(pngBytes);await resetDrop;
assert.equal(uploads().length,0);assert.equal(inputFocus,0);
clearSent();const resizeDrop=drop([{...uploadFile,arrayBuffer:()=>new Promise(resolve=>{finishRead=resolve;})}]);
await message({type:'frame',rows:[],reset:true});clearSent();finishRead(pngBytes);await resizeDrop;
assert.equal(uploads().length,2);assert.equal(inputFocus,1); // A normal display reset does not change attachment authority.
console.log('Browser ordinary raw-file drops, safe original names, size/read-only bounds and socket/relay/reset upload receipts passed');

// The same browser/socket survives a session handoff and waits for its RESET.
vm.runInContext('connect()',context);
const switchingSocket=sockets.at(-1),socketCount=sockets.length;
await message({type:'remote',host:'host',attachment:0});
await message({type:'assets',glyphs:[[65,1,[0]]],scale:2});
await message({type:'connection',connected:true,attachment:0});
assert.equal(context.ready,false);
await message({type:'frame',size:[80,25],mode:3,rows:[],reset:true,semanticSidebar:snapshot});
assert.equal(context.ready,true);
clearSent();const switchingDrop=drop([{...uploadFile,arrayBuffer:()=>new Promise(resolve=>{finishRead=resolve;})}]);
await message({type:'connection',connected:false,switching:true,attachment:1});
assert.equal(sidebarAccess.hidden,false); // Failed admission can restore the retained surface.
vm.runInContext("send({type:'key',key:'x'});send({type:'theme',dark:true})",context);
assert.equal(switchingSocket.sent.length,1);assert.equal(JSON.parse(switchingSocket.sent[0]).type,'theme');assert.equal(JSON.parse(switchingSocket.sent[0]).attachment,1);
await message({type:'session',session:'b'.repeat(48),attachment:2});
assert.equal(sidebarAccess.hidden,true);assert.equal(context.frame,null);
await message({type:'assets',glyphs:[[65,1,[0]]],scale:2});
await message({type:'connection',connected:true,attachment:2});
assert.equal(context.ready,false);
await message({type:'frame',size:[91,31],mode:3,rows:[],reset:true});
assert.equal(context.ready,true);assert.equal(sidebarAccess.hidden,true);
clearSent();finishRead(pngBytes);await switchingDrop;assert.equal(uploads().length,0);
vm.runInContext("send({type:'key',key:'y'})",context);
assert.equal(JSON.parse(switchingSocket.sent.at(-1)).attachment,2);
assert.equal(sockets.length,socketCount);assert.equal(sockets.at(-1),switchingSocket);
console.log('Browser session handoff retains its socket, gates keys until RESET, reports safe configuration and revokes old upload/semantic state');
