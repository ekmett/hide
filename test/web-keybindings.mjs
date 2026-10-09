// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
// Exercise production keyboard/clipboard event ownership using prepared frame
// chords. The protocol checks independently verify their resolved editor effects.
import fs from 'node:fs';
import vm from 'node:vm';
import assert from 'node:assert/strict';
const source=fs.readFileSync('assets/web/editor.js','utf8');
const handlers=new Map(),packets=[],commands=[];
const context=vm.createContext({
 window:{addEventListener:(name,handler)=>handlers.set(name,handler)},
 input:{addEventListener:()=>{},value:'',setSelectionRange:()=>{}},
 navigator:{platform:'MacIntel'},HTMLButtonElement:class {},performance:{now:()=>0},
 sessionFrontend:false,composing:false,cursorEpoch:0,frame:{bindingsActive:true,bindings:[]},
 fullscreen:{},downloadAction:{},sidebarAccess:{contains:target=>target===context.sidebarAccess},dialogAccess:{contains:target=>target===context.dialogAccess||target===context.dialogChild},dialogChild:{},clipboard:'selected',nativeCopies:[],send:packet=>packets.push(packet),command:name=>commands.push(name),
});
vm.runInContext(source.slice(source.indexOf('function mods(e)'),source.indexOf('function mods(e)')+source.slice(source.indexOf('function mods(e)')).indexOf('\n')),context);
vm.runInContext(source.slice(source.indexOf('function semanticReadingTarget('),source.indexOf("window.addEventListener('dragover'")),context);
function key(key,flags={}){
 packets.length=0;commands.length=0;
 const event={key,code:'',target:{},ctrlKey:false,metaKey:false,altKey:false,shiftKey:false,getModifierState:()=>false,preventDefault(){this.prevented=true;},...flags};
 handlers.get('keydown')(event);return event;
}
context.frame.bindings=[['Cmd+C','hide.edit.copy'],['Cmd+V','hide.edit.paste']];
assert.equal(key('Enter',{target:context.downloadAction}).prevented,undefined);assert.equal(packets.length,0); // Download keeps native keyboard activation.
assert.equal(key('ArrowDown',{target:context.sidebarAccess}).prevented,undefined);assert.equal(packets.length,0); // Read-only sidebar navigation stays local.
handlers.get('keyup')({target:context.sidebarAccess});assert.equal(packets.length,0);
for(const type of ['copy','cut','paste'])handlers.get(type)({target:context.sidebarAccess});
assert.equal(packets.length,0);assert.equal(commands.length,0); // Reading and browser clipboard gestures grant no host actions.
assert.equal(key('Enter',{target:context.dialogChild}).prevented,undefined);assert.equal(packets.length,0);
handlers.get('keyup')({target:context.dialogChild});assert.equal(packets.length,0);
for(const type of ['copy','cut','paste'])handlers.get(type)({target:context.dialogChild});
assert.equal(packets.length,0);assert.equal(commands.length,0);
context.sessionFrontend=true;
key(']',{target:context.dialogAccess,ctrlKey:true});assert.equal(packets.length,0); // Reading focus cannot detach the host either.
context.sessionFrontend=false;
assert.equal(key('ArrowDown').prevented,true);assert.equal(packets.at(-1).type,'key'); // Returning to the editor restores normal routing.
assert.equal(key('c',{metaKey:true}).prevented,undefined); // Native clipboard event owns activation.
context.frame.wordstar=true;context.frame.bindings=[['Ctrl+C','hide.edit.copy']];
assert.equal(key('c',{ctrlKey:true}).prevented,undefined); // Explicit WordStar or dialog Copy keeps its clipboard gesture.
context.frame.bindings=[['Y','hide.edit.copy'],['P','hide.edit.paste']];
assert.equal(key('y').prevented,true);assert.equal(packets.at(-1).type,'key');assert.equal(packets.at(-1).key,'y');
assert.equal(key('p').prevented,true);assert.equal(packets.at(-1).key,'p');
assert.equal(key('c',{ctrlKey:true}).prevented,true);assert.equal(packets.at(-1).type,'key'); // Removed prefix Copy does not delegate native activation.
context.frame.wordstar=false;
context.frame.bindings=[['Cmd+Shift+J','hide.edit.copy'],['Cmd+Shift+K','hide.edit.paste']];
assert.equal(key('c',{metaKey:true}).prevented,true); // Removed Cmd+C cannot trigger a browser Copy event.
assert.equal(packets.at(-1).type,'key');assert.deepEqual(Array.from(packets.at(-1).mods),['cmd']);
assert.equal(key('s',{metaKey:true}).prevented,true);assert.equal(packets.at(-1).key,'s');
assert.equal(key('p',{metaKey:true}).prevented,true);assert.equal(packets.at(-1).key,'p');
key('J',{metaKey:true,shiftKey:true});assert.equal(packets.at(-1).key,'J');assert.equal(commands.length,0);
key('K',{metaKey:true,shiftKey:true});assert.equal(packets.at(-1).type,'key');
context.frame.bindings=[['Cmd+Alt+F','hide.search.replace']];
key('ƒ',{metaKey:true,altKey:true,code:'KeyF'});assert.equal(packets.at(-1).key,'f');assert.deepEqual(Array.from(packets.at(-1).mods),['cmd','alt']);
assert.equal(key('≈',{altKey:true,code:'KeyX'}).prevented,undefined); // Option text stays composition-owned.
context.frame.terminal=true;key('c',{ctrlKey:true});assert.equal(packets.at(-1).type,'key');assert.deepEqual(Array.from(packets.at(-1).mods),['ctrl']);
assert.equal(key('r',{ctrlKey:true}).prevented,true);assert.equal(packets.at(-1).key,'r');assert.equal(key('R',{ctrlKey:true,shiftKey:true}).prevented,true);assert.equal(packets.at(-1).key,'R');
context.frame.terminal=false;assert.equal(key('r',{metaKey:true}).prevented,undefined);assert.equal(key('h',{metaKey:true}).prevented,undefined);
context.frame.bindings=[['Cmd+Shift+J','example.manual']];
context.frame.menuContributions=[{id:'example.manual',registry:'session-menu',generation:7,key:'⇧⌘J',enabled:true}];
key('J',{metaKey:true,shiftKey:true});
assert.equal(packets.at(-1).type,'menu');assert.equal(packets.at(-1).command,'example.manual');assert.equal(packets.at(-1).registry,'session-menu');assert.equal(packets.at(-1).generation,7);
context.frame.menuContributions[0].key='';key('J',{metaKey:true,shiftKey:true});assert.equal(packets.at(-1).type,'key'); // A contribution sharing a builtin ID cannot steal its chord.
context.frame.menuContributions[0].key='⇧⌘J';context.frame.menuContributions[0].enabled=false;
key('J',{metaKey:true,shiftKey:true});assert.equal(packets.at(-1).type,'menu');assert.equal(packets.at(-1).generation,7); // Disabled metadata still retains the exact lifetime; the host decides availability.
context.frame.bindings=[['Cmd+Shift+J','']];context.frame.menuContributions[0].generation=8;context.frame.menuContributions[0].key='';
key('J',{metaKey:true,shiftKey:true});assert.equal(packets.some(packet=>packet.type==='key'||packet.type==='menu'),false); // Actual pending/retired host frames retain an inert target, never raw fallback.
context.frame.bindings=[['Ctrl+Shift+U','hide.focus.source']];key('U',{ctrlKey:true,shiftKey:true});assert.equal(packets.at(-1).type,'key'); // Non-menu builtins retain keyboard transport.
context.frame.bindings=[];context.frame.menuContributions[0].enabled=true;key('J',{metaKey:true,shiftKey:true});assert.equal(packets.at(-1).type,'key'); // Unbinding removes stamped routing.
context.frame.bindingsActive=false;key('v',{metaKey:true});assert.deepEqual(commands,['hide.edit.paste']);
const exported={};handlers.get('copy')({preventDefault(){},clipboardData:{setData:(type,text)=>exported[type]=text}});
assert.equal(exported['text/plain'],'selected');assert.equal(commands.at(-1),'hide.edit.copy'); // Browser Edit > Copy remains semantic.
console.log('Browser configured-key, clipboard gesture, Option text and PTY ownership checks passed');

// Replies after user activation expires must expose the production click fallback.
let clipboardAllowed=false,click;
const clipboardPackets=[];
const action={hidden:true,addEventListener:(_,handler)=>click=handler};
const clipboardContext=vm.createContext({socket:{},ready:true,clipboardEpoch:0,
 navigator:{clipboard:{readText:async()=>{if(!clipboardAllowed)throw Error('activation expired');return 'authorized paste';},writeText:async()=>{if(!clipboardAllowed)throw Error('activation expired');}}},
 clipboardRequest:null,clipboardAction:action,status:{textContent:''},input:{focus:()=>{}},send:packet=>clipboardPackets.push(packet),
});
vm.runInContext(source.slice(source.indexOf('async function systemClipboard('),source.indexOf('// WebSocket replies')),clipboardContext);
await vm.runInContext("systemClipboard({type:'paste-request',request:'a'.repeat(48)})",clipboardContext);
assert.equal(action.hidden,false);assert.equal(action.textContent,'Paste from clipboard');assert.equal(clipboardPackets.length,0);
clipboardAllowed=true;await click();await new Promise(resolve=>setImmediate(resolve));
assert.equal(action.hidden,true);assert.equal(clipboardPackets.at(-1).text,'authorized paste');assert.equal(clipboardPackets.at(-1).type,'paste-reply');assert.equal(clipboardPackets.at(-1).request,'a'.repeat(48));
clipboardAllowed=false;await vm.runInContext("systemClipboard({type:'copy',text:'selected'})",clipboardContext);assert.equal(action.hidden,false);assert.equal(action.textContent,'Copy to clipboard');
console.log('Delayed clipboard permission-button fallback checks passed');

// A same relay socket may reconnect while a clipboard read is still pending.
clipboardPackets.length=0;
let finishRead;clipboardContext.navigator.clipboard.readText=()=>new Promise(resolve=>{finishRead=resolve;});
const pending=vm.runInContext("systemClipboard({type:'paste-request',request:'a'.repeat(48)})",clipboardContext);
vm.runInContext('clearClipboardRequest();ready=false;ready=true;',clipboardContext);
finishRead('stale');await pending;
assert.equal(clipboardPackets.length,0);assert.equal(clipboardContext.clipboardRequest,null);
console.log('requested clipboard reconnect checks passed');

// An old completion must not hide the permission button for a newer operation.
clipboardPackets.length=0;
let finishOld,reads=0;
clipboardContext.navigator.clipboard.readText=()=>++reads===1?new Promise(resolve=>{finishOld=resolve;}):Promise.reject(Error('click needed'));
const older=vm.runInContext("systemClipboard({type:'paste-request',request:'a'.repeat(48)})",clipboardContext);
await vm.runInContext("systemClipboard({type:'paste-request',request:'b'.repeat(48)})",clipboardContext);
finishOld('older');await older;
assert.equal(clipboardPackets.length,0);assert.equal(action.hidden,false);assert.equal(clipboardContext.clipboardRequest.request,'b'.repeat(48));
clipboardContext.navigator.clipboard.readText=async()=> 'newer';
click();await new Promise(setImmediate);
assert.equal(clipboardPackets.at(-1).request,'b'.repeat(48));assert.equal(action.hidden,true);
console.log('overlapping requested clipboard completion checks passed');
