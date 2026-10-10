// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
// Optional build entry: bundle with pinned Laya, ORT Web 1.30.0 and hash-wasm.
// A request owns inference; cancellation drains it before releasing resources.
import * as ort from 'onnxruntime-web/webgpu';
import {createSHA256} from 'hash-wasm';
import {Agent,toInternal,QTYPES} from 'pinned-laya/agent';
import {buildSequence,renderOptions} from 'pinned-laya/common';
import {parseTokenizerJson,encodeWithData} from 'pinned-laya/tokenizer';
import {feed,feedHead} from 'pinned-laya/providers';

const payloadNames=['LICENSE','MODEL_CARD.md','encoder.onnx','encoder.onnx.data','encoder_config.json','head.onnx','head.onnx.data','rl_agent_config.json','tokenizer.json','tokenizer_config.json'];
const retainedNames=new Set(['encoder.onnx','encoder.onnx.data','head.onnx','head.onnx.data','rl_agent_config.json','tokenizer.json']);
const digest=value=>typeof value==='string'&&/^[a-f0-9]{64}$/.test(value);
const same=(a,b)=>JSON.stringify(a)===JSON.stringify(b);
class InvalidInput extends Error {}
let config,manifest,resources,active,retiring=false,initialized=false;

async function readBytes(url,limit,expected,keep=true,signal){
 const response=await fetch(url,{cache:'no-store',signal});
 if(!response.ok||!response.body)throw new Error('Artifact unavailable');
 const hash=await createSHA256();hash.init();
 const chunks=keep&&!expected?[]:null,result=keep&&expected?new Uint8Array(limit):null;
 const reader=response.body.getReader();let count=0;
 try{for(;;){const {done,value}=await reader.read();if(done)break;
   if(count+value.length>limit)throw new Error('Artifact bound');
   hash.update(value);result?.set(value,count);chunks?.push(value);count+=value.length;
  }}finally{await reader.cancel().catch(()=>{});reader.releaseLock();}
 if(expected&&(count!==limit||hash.digest()!==expected))throw new Error('Artifact identity');
 if(!keep)return;
 if(result)return result;
 const bytes=new Uint8Array(count);let offset=0;
 for(const chunk of chunks){bytes.set(chunk,offset);offset+=chunk.length;}
 return bytes;
}

async function initialize(value){
 if(initialized)throw new Error('Already initialized');initialized=true;
 if(!digest(value.manifestSHA256)||!Number.isSafeInteger(value.allocationLimitBytes)||value.allocationLimitBytes<1)throw new Error('Configuration');
 const model=new URL(value.modelBaseURL,location.href),runtime=new URL(value.runtimeBaseURL,location.href);
 if(model.origin!==location.origin||runtime.origin!==location.origin||!model.pathname.endsWith('/')||!runtime.pathname.endsWith('/'))throw new Error('Asset origin');
 config={...value,modelBaseURL:model.href,runtimeBaseURL:runtime.href};
 const bytes=await readBytes(new URL('manifest.json',model),1048576);
 const hash=await createSHA256();hash.update(bytes);
 if(hash.digest()!==config.manifestSHA256)throw new Error('Manifest identity');
 manifest=JSON.parse(new TextDecoder().decode(bytes));
 if(manifest.format!=='laya-split-onnx'||manifest.version!==1||!Array.isArray(manifest.files)||manifest.files.length!==payloadNames.length)throw new Error('Manifest');
 const names=new Set();
 for(const file of manifest.files){
  if(!payloadNames.includes(file.path)||names.has(file.path)||!Number.isSafeInteger(file.bytes)||file.bytes<0||file.bytes>2147483648||!digest(file.sha256))throw new Error('Manifest entry');
  names.add(file.path);
 }
 if(!isSecureContext||!crossOriginIsolated||!navigator.gpu)throw new Error('WebGPU unavailable');
 // An offer verifies only this small manifest. No weights, GPU device or ORT
 // sessions are acquired until a selected request arrives.
 postMessage({type:'ready'});
}

function validateInput(input){
 let remaining=131072;
 const text=(value,limit,name=false)=>{
  if(typeof value!=='string'||value.length>limit||value.includes('\0')||(name&&(!value.length||/[\x00-\x1f]/.test(value))))throw new InvalidInput();
  const bytes=new TextEncoder().encode(value).length;remaining-=bytes;
  if(bytes>limit||remaining<0)throw new InvalidInput();return value;
 };
 const list=(value,max)=>{if(!Array.isArray(value)||!value.length||value.length>max)throw new InvalidInput();return value;};
 if(!input||typeof input!=='object')throw new InvalidInput();
 text(input.stateId,256,true);text(input.state,65536);
 const names=new Set();
 for(const q of list(input.questions,16)){
  if(!q||typeof q!=='object')throw new InvalidInput();
  text(q.name,128,true);if(names.has(q.name))throw new InvalidInput();names.add(q.name);
  text(q.instructions,131072);if(!q.instructions.trim())throw new InvalidInput();
  if(q.kind==='binary'){text(q.false,131072);text(q.true,131072);}
  else if(q.kind==='choice'){
   const labels=new Set();for(const option of list(q.options,255)){
    if(!option||typeof option!=='object')throw new InvalidInput();
    text(option.label,256,true);text(option.criterion,131072);
    if(labels.has(option.label))throw new InvalidInput();labels.add(option.label);
   }
  }else if(q.kind==='score'){for(const level of list(q.levels,255))text(level,131072);}
  else throw new InvalidInput();
 }
}

function prepare(tok,state,question){
 const definition={type:question.kind==='binary'?'noul':question.kind,instructions:question.instructions,
  criteria:question.kind==='binary'?{false:question.false,true:question.true}:question.kind==='score'?question.levels:Object.fromEntries(question.options.map(o=>[o.label,o.criterion]))};
 const q=toInternal(definition),clean=value=>value.replaceAll(tok.maskToken,' ');
 const stateIds=tok.encode(clean(state)),instruction=tok.encode(`${q.t} question: ${clean(q.ins)}`);
 const options=renderOptions(q).map(value=>tok.encode(' '+clean(value)));
 if(!options.length||options.length>255||options.some(ids=>ids.length>48))throw new InvalidInput();
 const marked=options.map(ids=>[tok.maskId,...ids]),budget=192-marked.reduce((sum,row)=>sum+row.length,0),per=Math.max(4,Math.floor(176/marked.length));
 if((budget<16&&marked.some(row=>row.length>per))||instruction.length>Math.max(8,budget))throw new InvalidInput();
 const ids=[tok.clsId,...instruction,tok.sepId],markers=[];
 for(const row of marked){markers.push(ids.length);ids.push(...row);}ids.push(tok.sepId,...stateIds,tok.sepId);
 if(ids.length>512)throw new InvalidInput();
 const actual=buildSequence(tok,state,q,512,192);
 if(!same(ids,actual.ids)||!same(markers,actual.markers)||actual.stats.truncated)throw new InvalidInput();
 return {question,definition,ids,markers,qtype:QTYPES[q.t]};
}

async function release(){
 const owned=resources;resources=null;if(!owned)return;
 // release() may enqueue work; wait for it before acknowledging retirement.
 try{try{await owned.head?.release();}finally{await owned.encoder?.release();}}
 finally{try{await owned.device?.queue.onSubmittedWorkDone();}finally{owned.device?.destroy();}}
}

function checkCurrent(call){if(call.cancelled||retiring)throw new Error('Cancelled');}

async function acquire(call){
 if(resources)return resources;
 const payload=new Map();
 for(const file of manifest.files){
  checkCurrent(call);
  const data=await readBytes(new URL(file.path,config.modelBaseURL),file.bytes,file.sha256,retainedNames.has(file.path),call.abort.signal);
  if(data)payload.set(file.path,data);
 }
 checkCurrent(call);
 const cfg=JSON.parse(new TextDecoder().decode(payload.get('rl_agent_config.json')));
 if(cfg.max_len!==512||cfg.head_max_len!==192)throw new Error('Context configuration');
 const raw=JSON.parse(new TextDecoder().decode(payload.get('tokenizer.json')));
 if(raw.truncation||raw.padding)throw new Error('Implicit tokenizer truncation');
 const data=parseTokenizerJson(raw);if(!data)throw new Error('Tokenizer unavailable');
 const tok={clsId:data.ids.cls,sepId:data.ids.sep,maskId:data.ids.mask,padId:data.ids.pad,maskToken:data.maskToken,encode:text=>encodeWithData(data,text)};
 const adapter=await navigator.gpu.requestAdapter({powerPreference:'high-performance'});checkCurrent(call);
 if(!adapter||adapter.info.isFallbackAdapter||/swiftshader|llvmpipe|software/i.test([adapter.info.vendor,adapter.info.architecture,adapter.info.device,adapter.info.description].join(' ')))throw new Error('Hardware WebGPU unavailable');
 const requiredFeatures=['timestamp-query','shader-f16','subgroups'].filter(name=>adapter.features.has(name));
 const requiredLimits={maxBufferSize:adapter.limits.maxBufferSize,maxStorageBufferBindingSize:adapter.limits.maxStorageBufferBindingSize,maxComputeWorkgroupStorageSize:adapter.limits.maxComputeWorkgroupStorageSize};
 const device=await adapter.requestDevice({requiredFeatures,requiredLimits});
 resources={device,tok,cfg};checkCurrent(call);
 // This bound meters owned WebGPU buffer declarations only. WASM/JS heaps,
 // decoded payloads and driver/process RSS are separate, unmetered resources.
 let liveBytes=0;
 const create=device.createBuffer.bind(device);
 device.createBuffer=descriptor=>{
  const size=Number(descriptor.size);
  if(!Number.isSafeInteger(size)||size<0||liveBytes+size>config.allocationLimitBytes)throw new Error('GPU allocation bound');
  const buffer=create(descriptor);liveBytes+=size;
  let live=true;const destroy=buffer.destroy.bind(buffer);
  buffer.destroy=()=>{if(live){live=false;liveBytes-=size;}destroy();};return buffer;
 };
 ort.env.wasm.wasmPaths=config.runtimeBaseURL;ort.env.wasm.numThreads=4;
 resources.encoder=await ort.InferenceSession.create(payload.get('encoder.onnx'),{executionProviders:[{name:'webgpu',device}],graphOptimizationLevel:'basic',enableCpuMemArena:false,externalData:[{path:'encoder.onnx.data',data:payload.get('encoder.onnx.data')}]});
 payload.delete('encoder.onnx');payload.delete('encoder.onnx.data');checkCurrent(call);
 resources.head=await ort.InferenceSession.create(payload.get('head.onnx'),{executionProviders:['wasm'],graphOptimizationLevel:'basic',enableCpuMemArena:false,externalData:[{path:'head.onnx.data',data:payload.get('head.onnx.data')}]});
 payload.clear();checkCurrent(call);return resources;
}

async function infer(call,input){
 validateInput(input);const owned=await acquire(call);checkCurrent(call);
 // Validate every question before running any of them. No input is truncated.
 const prepared=input.questions.map(q=>prepare(owned.tok,input.state,q));
 let expected;
 const provider={
  async runEncoder(batch){
   checkCurrent(call);
   if(!same(batch.inputIds,[expected.ids])||!same(batch.markerPos,[expected.markers])||!same(batch.qtype,[expected.qtype]))throw new Error('Packing mismatch');
   const inputs=feed(ort,batch);let outputs;
   try{outputs=await owned.encoder.run(inputs);checkCurrent(call);
    const tensor=outputs.last_hidden_state;
    if(!same(tensor.dims,[1,expected.ids.length,1024]))throw new Error('Encoder shape');
    return {lastHidden:[Array.from({length:tensor.dims[1]},(_,i)=>Array.from(tensor.data.slice(i*1024,(i+1)*1024)))]};
   }finally{for(const value of Object.values(inputs))value.dispose();if(outputs)for(const value of Object.values(outputs))value.dispose();}
  },
  async runHead(hidden,batch){
   checkCurrent(call);
   if(batch.markerPos[0].length===1)batch={...batch,markerPos:[[...batch.markerPos[0],0]],markerMask:[[true,false]]};
   const inputs=feedHead(ort,hidden,batch);let outputs;
   try{outputs=await owned.head.run(inputs);checkCurrent(call);
    return {logits:[Array.from(outputs.logits.data)],act:[Array.from(outputs.act_logits.data)]};
   }finally{for(const value of Object.values(inputs))value.dispose();if(outputs)for(const value of Object.values(outputs))value.dispose();}
  }
 };
 const agent=new Agent({provider,tok:owned.tok,cfg:owned.cfg,revision:config.manifestSHA256});
 const answers=[];let inputTokens=0;
 for(const item of prepared){
  expected=item;checkCurrent(call);
  const result=await agent.systemOne(input.state,{answer:item.definition});checkCurrent(call);
  if(result.usage.truncated)throw new Error('Unexpected truncation');
  const answer=result.answers.answer,q=item.question;
  const probabilities=q.kind==='binary'?[1-answer.noul,answer.noul]:q.kind==='choice'?q.options.map(o=>answer.probabilities[o.label]):q.levels.map((_,i)=>answer.probabilities[String(i)]);
  if(!probabilities.every(p=>Number.isFinite(p)&&p>=0&&p<=1)||!Number.isFinite(answer.confidence))throw new Error('Invalid distribution');
  answers.push({question:q.name,kind:q.kind,probabilities,confidence:answer.confidence});inputTokens+=result.usage.input_tokens;
 }
 return {manifestSHA256:config.manifestSHA256,answers,usage:{inputTokens,outputTokens:0}};
}

async function execute(message){
 const call={request:message.request,cancelled:false,abort:new AbortController()};active=call;
 let failure;
 try{const output=await infer(call,message.input);if(!call.cancelled&&!retiring)postMessage({type:'result',request:call.request,output});}
 catch(error){failure=error instanceof InvalidInput?'invalid':'failed';if(!call.cancelled&&!retiring)postMessage({type:'result',request:call.request,failure});}
 finally{
  try{
   await resources?.device.queue.onSubmittedWorkDone();
   if(call.cancelled||retiring||failure==='failed')await release();
   active=null;postMessage({type:'released',request:call.request});
   if(retiring){retiring=false;postMessage({type:'retired'});}
  }catch{active=null;postMessage({type:'unavailable'});}
 }
}

self.onmessage=event=>{
 const message=event.data;
 if(message?.type==='initialize')void initialize(message.config).catch(()=>postMessage({type:'unavailable'}));
 else if(message?.type==='request'&&config&&manifest&&!active&&!retiring)void execute(message);
 else if(message?.type==='cancel'&&active?.request===message.request){active.cancelled=true;active.abort.abort();}
 else if(message?.type==='retire'){
  retiring=true;
  if(active){active.cancelled=true;active.abort.abort();}
  else void release().then(()=>{retiring=false;postMessage({type:'retired'});},()=>postMessage({type:'unavailable'}));
 }
};
