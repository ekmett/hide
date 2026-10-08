// SPDX-License-Identifier: BSD-3-Clause
// Immutable RGBA resources and the complete, policy-filtered character-grid scene.
class CanvasImages {
 constructor(gl,access){
  this.gl=gl;this.access=access;this.epoch=null;this.resources=new Map();
  this.upload=null;this.chunk=null;this.bytes=0;this.scene=null;this.gpu=null;
 }
 clear(){
  for(const resource of this.resources.values())if(resource.texture)this.gl.deleteTexture(resource.texture);
  this.resources.clear();this.upload=this.chunk=this.scene=null;this.epoch=null;this.bytes=0;
  this.access.replaceChildren();this.access.hidden=true;
 }
 control(message){
  const integer=n=>Number.isSafeInteger(n)&&n>=0,id=n=>typeof n==='string'&&/^[0-9a-f]{48}$/.test(n);
  const invalid=()=>{throw new Error('Invalid canvas resource');};
  if(!id(message.epoch))invalid();
  if(message.type==='canvas-reset'){this.clear();this.epoch=message.epoch;return;}
  if(message.epoch!==this.epoch||!id(message.id))invalid();
  if(message.type==='canvas-resource'){
   const {width,height,bytes}=message;
   if(this.upload||this.resources.size>=64||this.resources.has(message.id)||!integer(width)||!integer(height)||width<1||height<1||width>4096||height>4096||width*height>4194304||bytes!==width*height*4||this.bytes+bytes>67108864)invalid();
   this.bytes+=bytes;
   this.upload={id:message.id,width,height,data:new Uint8Array(bytes),received:0};
  }else if(message.type==='canvas-chunk'){
   if(this.chunk||!this.upload||this.upload.id!==message.id||!integer(message.offset)||message.offset!==this.upload.received||!integer(message.length)||message.length<1||message.length>262144||message.offset+message.length>this.upload.data.length)invalid();
   this.chunk=message;
  }else if(message.type==='canvas-release'){
   const resource=this.resources.get(message.id);
   if(resource){if(resource.texture)this.gl.deleteTexture(resource.texture);this.bytes-=resource.data.length;this.resources.delete(message.id);}
   if(this.upload?.id===message.id){this.bytes-=this.upload.data.length;this.upload=null;}
   // A release may cancel an upload between pairs, never within a pair.
   if(this.chunk?.id===message.id)invalid();
   this.describe();
  }else invalid();
 }
 binary(bytes){
  const chunk=this.chunk,upload=this.upload;
  this.chunk=null;
  if(!chunk||!upload||bytes.byteLength!==chunk.length)throw new Error('Invalid canvas chunk bytes');
  upload.data.set(new Uint8Array(bytes),chunk.offset);upload.received+=chunk.length;
  if(upload.received===upload.data.length){
   this.upload=null;const resource={width:upload.width,height:upload.height,data:upload.data,texture:null};
   this.resources.set(upload.id,resource);if(this.gpu)this.texture(resource);this.describe();
  }
 }
 receive(value,cols,lines){
  const invalid=()=>{this.scene=null;this.describe();throw new Error('Invalid canvas scene');};
  if(!Number.isInteger(cols)||cols<1||cols>512||!Number.isInteger(lines)||lines<1||lines>256||!value||value.epoch!==this.epoch||!Array.isArray(value.surfaces)||value.surfaces.length>64||typeof value.mask!=='string')invalid();
  if(!value.surfaces.length&&!value.mask){this.scene=null;this.describe();return;}
  if(value.mask.length!==4*Math.ceil(cols*lines*2/3))invalid();
  let raw;try{raw=atob(value.mask);}catch{invalid();}
  if(raw.length!==cols*lines*2)invalid();
  const slots=new Set(),ids=new Set();
  for(const surface of value.surfaces){
   if(!surface||!Number.isSafeInteger(surface.id)||surface.id<1||ids.has(surface.id)||typeof surface.resource!=='string'||!/^[0-9a-f]{48}$/.test(surface.resource)||!Number.isInteger(surface.slot)||surface.slot<1||surface.slot>64||slots.has(surface.slot)||
     !Array.isArray(surface.rect)||surface.rect.length!==4||!surface.rect.every(Number.isSafeInteger)||surface.rect[0]<0||surface.rect[1]<0||surface.rect[2]<1||surface.rect[3]<1||surface.rect[0]+surface.rect[2]>cols||surface.rect[1]+surface.rect[3]>lines||
     !Array.isArray(surface.target)||surface.target.length!==4||!surface.target.every(n=>Number.isFinite(n)&&Math.abs(n)<=1000000)||surface.target[2]<=0||surface.target[3]<=0||
     typeof surface.name!=='string'||Array.from(surface.name).length>256||typeof surface.description!=='string'||Array.from(surface.description).length>1024)invalid();
   slots.add(surface.slot);ids.add(surface.id);
  }
  const mask=new Uint32Array(cols*lines),visible=new Set();
  for(let i=0;i<mask.length;i++){
   const cell=raw.charCodeAt(i*2)|(raw.charCodeAt(i*2+1)<<8),slot=cell&32767;
   if(slot&&!slots.has(slot))invalid();
   mask[i]=cell;if(slot)visible.add(slot);
  }
  this.scene={surfaces:value.surfaces,mask,visible,cols,lines};this.mask();this.describe();
 }
 describe(){
  this.access.replaceChildren();
  for(const surface of this.scene?.surfaces||[])if(this.scene.visible.has(surface.slot)){
   const item=document.createElement('div');item.setAttribute('role','img');item.setAttribute('aria-label',surface.name);item.setAttribute('aria-description',surface.description);item.textContent=surface.name;this.access.append(item);
  }
  this.access.hidden=!this.access.children.length;
 }
 texture(resource){
  const gl=this.gl;resource.texture=gl.createTexture();gl.activeTexture(gl.TEXTURE2);gl.bindTexture(gl.TEXTURE_2D,resource.texture);
  for(const p of [gl.TEXTURE_MIN_FILTER,gl.TEXTURE_MAG_FILTER])gl.texParameteri(gl.TEXTURE_2D,p,gl.NEAREST);
  for(const p of [gl.TEXTURE_WRAP_S,gl.TEXTURE_WRAP_T])gl.texParameteri(gl.TEXTURE_2D,p,gl.CLAMP_TO_EDGE);
  gl.texImage2D(gl.TEXTURE_2D,0,gl.RGBA8,resource.width,resource.height,0,gl.RGBA,gl.UNSIGNED_BYTE,resource.data);
 }
 mask(){
  if(!this.gpu||!this.scene)return;
  const gl=this.gl;gl.activeTexture(gl.TEXTURE3);gl.bindTexture(gl.TEXTURE_2D,this.gpu.mask);
  gl.texImage2D(gl.TEXTURE_2D,0,gl.R32UI,this.scene.cols,this.scene.lines,0,gl.RED_INTEGER,gl.UNSIGNED_INT,this.scene.mask);
 }
 lost(){this.gpu=null;for(const resource of this.resources.values())resource.texture=null;}
 restore(makeProgram,vertices){
  const gl=this.gl,program=makeProgram(hideCanvasFragment),vao=gl.createVertexArray(),uniforms=gl.createBuffer(),mask=gl.createTexture();
  gl.bindVertexArray(vao);gl.bindBuffer(gl.ARRAY_BUFFER,vertices);
  const pos=gl.getAttribLocation(program,'position');gl.enableVertexAttribArray(pos);gl.vertexAttribPointer(pos,2,gl.FLOAT,false,0,0);
  gl.useProgram(program);gl.uniformBlockBinding(program,gl.getUniformBlockIndex(program,'type_Canvas'),1);
  gl.bindBuffer(gl.UNIFORM_BUFFER,uniforms);gl.bufferData(gl.UNIFORM_BUFFER,32,gl.DYNAMIC_DRAW);gl.bindBufferBase(gl.UNIFORM_BUFFER,1,uniforms);
  gl.uniform1i(gl.getUniformLocation(program,'SPIRV_Cross_CombinedcanvasImagecanvasSampler'),2);
  gl.uniform1i(gl.getUniformLocation(program,'SPIRV_Cross_CombinedcanvasMaskSPIRV_Cross_DummySampler'),3);
  gl.activeTexture(gl.TEXTURE3);gl.bindTexture(gl.TEXTURE_2D,mask);
  for(const p of [gl.TEXTURE_MIN_FILTER,gl.TEXTURE_MAG_FILTER])gl.texParameteri(gl.TEXTURE_2D,p,gl.NEAREST);
  for(const p of [gl.TEXTURE_WRAP_S,gl.TEXTURE_WRAP_T])gl.texParameteri(gl.TEXTURE_2D,p,gl.CLAMP_TO_EDGE);
  this.gpu={program,vao,uniforms,mask};for(const resource of this.resources.values())this.texture(resource);this.mask();
 }
 draw(){
  if(!this.scene||!this.gpu)return;
  const gl=this.gl,{program,vao,uniforms,mask}=this.gpu;
  gl.useProgram(program);gl.bindVertexArray(vao);gl.activeTexture(gl.TEXTURE3);gl.bindTexture(gl.TEXTURE_2D,mask);gl.bindBuffer(gl.UNIFORM_BUFFER,uniforms);
  gl.enable(gl.SCISSOR_TEST);
  try{for(const surface of this.scene.surfaces){
   const resource=this.resources.get(surface.resource);if(!resource?.texture||!this.scene.visible.has(surface.slot))continue;
   const [x,y,w,h]=surface.rect,cw=gl.drawingBufferWidth/this.scene.cols,ch=gl.drawingBufferHeight/this.scene.lines;
   // Conservative pixel edges leave fractional boundary ownership to the mask.
   const left=Math.floor(x*cw),top=Math.floor(y*ch),right=Math.ceil((x+w)*cw),bottom=Math.ceil((y+h)*ch);
   gl.scissor(left,gl.drawingBufferHeight-bottom,right-left,bottom-top);
   gl.activeTexture(gl.TEXTURE2);gl.bindTexture(gl.TEXTURE_2D,resource.texture);
   gl.bufferSubData(gl.UNIFORM_BUFFER,0,new Float32Array([this.scene.cols,this.scene.lines,surface.slot,0,...surface.target]));gl.drawArrays(gl.TRIANGLE_STRIP,0,4);
  }}finally{gl.disable(gl.SCISSOR_TEST);}
 }
}
