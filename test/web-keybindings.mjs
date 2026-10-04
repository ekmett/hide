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
 fullscreen:{},clipboard:'selected',nativeCopies:[],send:packet=>packets.push(packet),command:name=>commands.push(name),
});
vm.runInContext(source.slice(source.indexOf('function mods(e)'),source.indexOf('function mods(e)')+source.slice(source.indexOf('function mods(e)')).indexOf('\n')),context);
vm.runInContext(source.slice(source.indexOf("window.addEventListener('keydown'"),source.indexOf("window.addEventListener('dragover'")),context);
function key(key,flags={}){
 packets.length=0;commands.length=0;
 const event={key,code:'',target:{},ctrlKey:false,metaKey:false,altKey:false,shiftKey:false,getModifierState:()=>false,preventDefault(){this.prevented=true;},...flags};
 handlers.get('keydown')(event);return event;
}
context.frame.bindings=[['Cmd+C','hide.edit.copy'],['Cmd+V','hide.edit.paste']];
assert.equal(key('c',{metaKey:true}).prevented,undefined); // Native clipboard event owns activation.
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
context.frame.bindingsActive=false;key('v',{metaKey:true});assert.deepEqual(commands,['hide.edit.paste']);
const exported={};handlers.get('copy')({preventDefault(){},clipboardData:{setData:(type,text)=>exported[type]=text}});
assert.equal(exported['text/plain'],'selected');assert.equal(commands.at(-1),'hide.edit.copy'); // Browser Edit > Copy remains semantic.
console.log('Browser configured-key, clipboard gesture, Option text and PTY ownership checks passed');

// Replies after user activation expires must expose the production click fallback.
let clipboardAllowed=false,click;
const clipboardPackets=[];
const action={hidden:true,addEventListener:(_,handler)=>click=handler};
const clipboardContext=vm.createContext({
 navigator:{clipboard:{readText:async()=>{if(!clipboardAllowed)throw Error('activation expired');return 'authorized paste';},writeText:async()=>{if(!clipboardAllowed)throw Error('activation expired');}}},
 clipboardRequest:null,clipboardAction:action,status:{textContent:''},input:{focus:()=>{}},send:packet=>clipboardPackets.push(packet),
});
vm.runInContext(source.slice(source.indexOf('async function systemClipboard('),source.indexOf('// WebSocket replies')),clipboardContext);
await vm.runInContext("systemClipboard({type:'paste-request'})",clipboardContext);
assert.equal(action.hidden,false);assert.equal(action.textContent,'Paste from clipboard');assert.equal(clipboardPackets.length,0);
clipboardAllowed=true;await click();await new Promise(resolve=>setImmediate(resolve));
assert.equal(action.hidden,true);assert.equal(clipboardPackets.at(-1).text,'authorized paste');
clipboardAllowed=false;await vm.runInContext("systemClipboard({type:'copy',text:'selected'})",clipboardContext);assert.equal(action.hidden,false);assert.equal(action.textContent,'Copy to clipboard');
console.log('Delayed clipboard permission-button fallback checks passed');
