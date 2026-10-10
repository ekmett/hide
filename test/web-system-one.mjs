// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
import fs from 'node:fs';import vm from 'node:vm';import assert from 'node:assert/strict';
const root=process.cwd(),sent=[],workers=[];let failures=0;
class Worker {constructor(url,options){this.url=url;this.options=options;this.messages=[];this.dead=false;workers.push(this);}postMessage(value){this.messages.push(value);}terminate(){this.dead=true;}emit(data){this.onmessage({data});}}
const context=vm.createContext({Worker,send:value=>sent.push(structuredClone(value)),fail:()=>failures++});
vm.runInContext(fs.readFileSync(root+'/assets/web/system-one-client.js','utf8')+'\nthis.Client=SystemOneClient;',context);
const c=new context.Client({send:context.send,onFailure:context.fail,workerURL:'/runtime/worker.js',modelBaseURL:'/model/',runtimeBaseURL:'/runtime/',manifestSHA256:'d'.repeat(64),allocationLimitBytes:3*1024**3,label:'local'});
const connection='a'.repeat(48),viewer='b'.repeat(48),supplier='c'.repeat(48),request='e'.repeat(48),next='f'.repeat(48);
assert.equal(c.connect(connection,viewer),true);const w=workers.at(-1);assert.equal(sent.length,0);assert.equal(w.messages[0].type,'initialize');
w.emit({type:'ready'});assert.equal(sent.at(-1).type,'system-one-offer');const offer=sent.at(-1).offer;
const identity={connection,viewer,offer,supplier,request};
c.receive({type:'system-one-request',...identity,input:{state:'s'}});assert.equal(w.messages.at(-1).type,'request');
c.receive({type:'system-one-cancel',...identity,request:next});assert.equal(w.messages.at(-1).type,'request');
c.receive({type:'system-one-cancel',...identity});assert.equal(w.messages.at(-1).type,'cancel');assert.equal(sent.filter(v=>v.type==='system-one-released').length,0);
w.emit({type:'result',request,output:{bad:true}});assert.equal(sent.filter(v=>v.type==='system-one-result').length,0);
w.emit({type:'released',request});assert.equal(sent.at(-1).type,'system-one-released');assert.deepEqual(Object.keys(sent.at(-1)).sort(),['connection','offer','request','supplier','type','viewer']);
w.emit({type:'released',request});assert.equal(sent.filter(v=>v.type==='system-one-released').length,1);
c.receive({type:'system-one-request',...identity,request:next,input:{}});w.emit({type:'result',request:next,failure:'invalid'});w.emit({type:'released',request:next});assert.equal(sent.at(-2).failure,'invalid');
c.receive({type:'system-one-retire',connection,viewer,offer,supplier});assert.equal(w.messages.at(-1).type,'retire');assert.equal(sent.filter(v=>v.type==='system-one-retired').length,0);
w.emit({type:'retired'});assert.equal(sent.at(-1).type,'system-one-retired');assert.equal(c.advertised,true);
const supplier2='9'.repeat(48);c.receive({type:'system-one-request',...identity,supplier:supplier2,input:{}});assert.equal(c.active.identity.supplier,supplier2);
c.close();assert.equal(w.dead,true);const n=sent.length;w.emit({type:'released',request});assert.equal(sent.length,n);
c.connect(connection,'8'.repeat(48));workers.at(-1).emit({type:'ready'});assert.ok(sent.at(-1).offer>offer);assert.equal(c.receive({type:'system-one-request',...identity,input:{}}),true);assert.equal(c.active,null);
workers.at(-1).onerror();assert.equal(failures,1);assert.equal(sent.at(-1).type,'system-one-withdraw');assert.equal(c.worker,null);
// Execute the production wire preflight. The Worker above is simulated to
// control reply ordering; pinned tokenizer and actual WebGPU evidence are
// separate and do not follow from these controller checks.
const worker=fs.readFileSync(root+'/assets/web/system-one-worker.js','utf8');
const valid=worker.match(/^function validateInput\([\s\S]*?^}/m)[0];
const v=vm.createContext({TextEncoder});vm.runInContext('class InvalidInput extends Error {}\n'+valid+'\nthis.validate=validateInput;',v);
const input={stateId:'snapshot',state:'',questions:[{name:'q',instructions:'does it?',kind:'binary',false:'no',true:'yes'}]};
assert.doesNotThrow(()=>v.validate(input));for(const bad of [{...input,state:'a'.repeat(65537)},{...input,questions:[...input.questions,...input.questions]},{...input,questions:[{...input.questions[0],kind:'choice',options:[{label:'x',criterion:''},{label:'x',criterion:''}]}]}])assert.throws(()=>v.validate(bad));
console.log('System-1 controller exact cancellation/retirement/stale lifetimes and input bounds passed.');

// Exercise the actual editor binding separately from its DOM renderer. A stale
// config fetch must not resurrect an old viewer; private controls bypass the
// editor's attachment/sequence envelope.
const editor=fs.readFileSync(root+'/assets/web/editor.js','utf8'),configuration=[];
const socket={readyState:1,sent:[],send(value){this.sent.push(JSON.parse(value));}};
const binding=vm.createContext({URL,location:{href:'http://127.0.0.1:1234/capability/'},connection:socket,socket,stopped:false,
 WebSocket:{OPEN:1},SystemOneClient:context.Client,fetch:()=>new Promise(resolve=>configuration.push(resolve))});
const bindingStart=editor.indexOf(' let inferenceClient=null,inferenceBinding=null;');
assert.ok(bindingStart>=0);
vm.runInContext(editor.slice(bindingStart,editor.indexOf(' function publishFrontend()',bindingStart))+'\nthis.bind=bindInference;this.close=closeInference;',binding);
const oldBinding=binding.bind({connection,viewer}),newBinding=binding.bind({connection,viewer:'7'.repeat(48)});
const config={workerURL:'system-one-runtime/system-one-runtime.js',runtimeBaseURL:'system-one-runtime/',modelBaseURL:'system-one-model/',manifestSHA256:'d'.repeat(64),allocationLimitBytes:3*1024**3,label:'local'};
const workerCount=workers.length;
configuration[0]({ok:true,json:async()=>({...config})});await oldBinding;assert.equal(workers.length,workerCount);
configuration[1]({ok:true,json:async()=>({...config})});await newBinding;
const boundWorker=workers.at(-1);assert.equal(boundWorker.url,'http://127.0.0.1:1234/capability/system-one-runtime/system-one-runtime.js');
assert.equal(boundWorker.messages[0].config.modelBaseURL,'http://127.0.0.1:1234/capability/system-one-model/');
boundWorker.emit({type:'ready'});assert.equal(socket.sent.length,1);
assert.deepEqual(Object.keys(socket.sent[0]).sort(),['allocationLimitBytes','backend','connection','label','manifestSHA256','offer','type','viewer']);
assert.equal(socket.sent[0].viewer,'7'.repeat(48));
await binding.bind({connection:null});assert.equal(boundWorker.dead,true);
const closedCount=socket.sent.length;boundWorker.emit({type:'ready'});assert.equal(socket.sent.length,closedCount);
console.log('System-1 editor binding uses raw private controls and rejects stale asynchronous configuration.');
