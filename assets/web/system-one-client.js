// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
// Frontend connection owner for the optional local System-1 Worker. The host
// schedules requests; this controller holds one request and no pending queue.
class SystemOneClient {
 constructor({send,workerURL,modelBaseURL,runtimeBaseURL,manifestSHA256,allocationLimitBytes,label,onFailure=()=>{}}){
  this.send=send;this.onFailure=onFailure;this.workerURL=workerURL;
  this.config={modelBaseURL,runtimeBaseURL,manifestSHA256,allocationLimitBytes};this.label=label;
  this.connection=null;this.viewer=null;this.offer=0;this.advertised=false;this.worker=null;
  this.supplier=null;this.active=null;this.retiring=null;
 }
 connect(connection,viewer){
  const id=value=>typeof value==='string'&&/^[0-9a-f]{48}$/.test(value);
  if(!id(connection)||!id(viewer))return false;
  if(this.connection===connection&&this.viewer===viewer)return true;
  this.close();this.connection=connection;this.viewer=viewer;
  if(!Number.isSafeInteger(++this.offer))return false;
  try{
   const worker=new Worker(this.workerURL,{type:'module'});this.worker=worker;
   worker.onmessage=event=>{if(this.worker===worker)this.message(event.data);};
   worker.onerror=()=>{if(this.worker===worker)this.failed();};
   worker.onmessageerror=()=>{if(this.worker===worker)this.failed();};
   worker.postMessage({type:'initialize',config:this.config});return true;
  }catch{this.failed();return false;}
 }
 identity(type,request){
  return {type,connection:this.connection,viewer:this.viewer,offer:this.offer,supplier:this.supplier,...(request?{request}:{} )};
 }
 message(message){
  if(message?.type==='ready'){
   if(this.advertised)return;
   this.advertised=true;
   this.send({type:'system-one-offer',connection:this.connection,viewer:this.viewer,offer:this.offer,label:this.label,
    manifestSHA256:this.config.manifestSHA256,allocationLimitBytes:this.config.allocationLimitBytes,backend:'webgpu'});
  }else if(message?.type==='result'&&this.active?.request===message.request){
   const call=this.active;
   if(call.result||call.cancelled)return;
   call.result=true;
   if(message.failure==='invalid'||message.failure==='failed')this.send({...call.identity,type:'system-one-result',failure:message.failure});
   else if(message.output)this.send({...call.identity,type:'system-one-result',output:message.output});
   else this.failed();
  }else if(message?.type==='released'&&this.active?.request===message.request){
   const call=this.active;this.active=null;
   this.send({...call.identity,type:'system-one-released'});
  }else if(message?.type==='retired'&&this.retiring){
   const identity=this.retiring;this.retiring=null;this.supplier=null;
   this.send({...identity,type:'system-one-retired'});
  }else if(message?.type==='unavailable')this.failed();
 }
 receive(message){
  if(!['system-one-request','system-one-cancel','system-one-retire'].includes(message?.type))return false;
  const id=value=>typeof value==='string'&&/^[0-9a-f]{48}$/.test(value);
  if(!this.advertised||!this.worker||message.connection!==this.connection||message.viewer!==this.viewer||message.offer!==this.offer||!id(message.supplier))return true;
  if(this.supplier&&message.supplier!==this.supplier)return true;
  if(message.type==='system-one-request'){
   if(!id(message.request)||this.retiring)return true;
   if(this.active){
    // A duplicate of the admitted request cannot create a premature release.
    if(message.request!==this.active.request)this.send({...this.identity('system-one-result',message.request),failure:'failed'});
    if(message.request!==this.active.request)this.send(this.identity('system-one-released',message.request));
    return true;
   }
   this.supplier=message.supplier;
   this.active={request:message.request,identity:this.identity('system-one-request',message.request),cancelled:false,result:false};
   this.worker.postMessage({type:'request',request:message.request,input:message.input});
  }else if(message.type==='system-one-cancel'){
   if(this.active?.request!==message.request)return true;
   this.active.cancelled=true;this.worker.postMessage({type:'cancel',request:message.request});
  }else{
   if(this.retiring)return true;
   this.supplier=message.supplier;this.retiring=this.identity('system-one-retire');
   if(this.active)this.active.cancelled=true;
   this.worker.postMessage({type:'retire'});
  }
  return true;
 }
 failed(){
  // A crashed Worker or failed GPU drain is not a hardware-release receipt.
  // The owner may report the lost supplier through onFailure; the editor
  // connection remains independent.
  this.close();this.onFailure();
 }
 close(){
  const worker=this.worker;this.worker=null;
  if(this.advertised)this.send({type:'system-one-withdraw',connection:this.connection,viewer:this.viewer,offer:this.offer});
  this.advertised=false;this.active=null;this.retiring=null;this.supplier=null;
  this.connection=this.viewer=null;
  // Disconnect abandons this local lifetime. It never sends released/retired
  // based merely on terminate(), which has no GPU-drain acknowledgement.
  worker?.terminate();
 }
}
