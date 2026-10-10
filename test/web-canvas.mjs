// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
// Production resource/scene handlers: bounded assembly and replacement lifecycle.
import fs from 'node:fs';
import vm from 'node:vm';
import assert from 'node:assert/strict';
class Element {
 constructor(tag='div'){this.tagName=tag;this.children=[];this.attributes={};this.hidden=false;this.parentElement=null;}
 replaceChildren(){for(const item of [...this.children])item.remove();}append(item){item.remove();this.children.push(item);item.parentElement=this;}
 remove(){if(this.contains(document.activeElement))document.activeElement=null;if(this.parentElement)this.parentElement.children.splice(this.parentElement.children.indexOf(this),1);this.parentElement=null;}
 setAttribute(key,value){this.attributes[key]=value;}
 getAttribute(key){return this.attributes[key]??null;}removeAttribute(key){delete this.attributes[key];}
 contains(item){return item===this||this.children.some(child=>child.contains(item));}
 focus(){document.activeElement=this;}
 click(){this.onclick?.();}
}
const document={activeElement:null,createElement:tag=>new Element(tag)},access=new Element(),removed=[],sent=[];
const context=vm.createContext({Uint8Array,Uint32Array,ArrayBuffer,atob,document,gl:{deleteTexture:t=>removed.push(t)},access,send:value=>sent.push(JSON.parse(JSON.stringify(value)))});
vm.runInContext(fs.readFileSync('assets/web/canvas-images.js','utf8')+'\nconst images=new CanvasImages(gl,access,send);',context);
const epoch='a'.repeat(48),rid='b'.repeat(48),other='c'.repeat(48);
function run(code){return vm.runInContext(code,context);}
function control(value){context.value=value;run('images.control(value)');}
function binary(bytes){context.bytes=Uint8Array.from(bytes).buffer;run('images.binary(bytes)');}
function resource(id=rid,w=2,h=2){control({type:'canvas-resource',epoch,id,width:w,height:h,bytes:w*h*4});}
function chunk(id,offset,bytes){control({type:'canvas-chunk',epoch,id,offset,length:bytes.length});binary(bytes);}
function scene(surfaces,mask=[1,0x8001,0,0]){
 const bytes=Buffer.alloc(8);mask.forEach((v,i)=>bytes.writeUInt16LE(v,i*2));
 context.value={epoch,surfaces,mask:bytes.toString('base64')};run('images.receive(value,2,2)');
}
const surface={id:1,resource:rid,rect:[0,0,2,2],target:[0,0,2,2],slot:1,name:'safe <script> λ.png',description:'2 × 2 PNG',controls:null};
control({type:'canvas-reset',epoch});resource();
scene([surface]);assert.equal(access.children[0].children[0].attributes['aria-label'],surface.name);assert.equal(access.children[0].children[0].textContent,surface.name);
assert.deepEqual(Array.from(run('images.scene.mask')),[1,0x8001,0,0]);
chunk(rid,0,[1,2,3,4]);assert.equal(run('images.resources.size'),0);
chunk(rid,4,Array(12).fill(5));assert.equal(run('images.resources.size'),1);assert.equal(run('images.bytes'),16);
assert.deepEqual(Array.from(run('images.resources.values().next().value.data')),[1,2,3,4,...Array(12).fill(5)]);
assert.throws(()=>resource(),/Invalid canvas/); // Duplicate live identity.
scene([{...surface,target:[-1.25,0.5,4,3]}]);assert.equal(run('images.bytes'),16);assert.equal(run('images.resources.size'),1); // Pan/zoom never uploads.
scene([surface],[0,0,0,0]);assert.equal(access.hidden,true);assert.equal(run('images.resources.size'),1); // Occlusion retains pixels.
resource(other);chunk(other,0,[9,9,9,9]);control({type:'canvas-release',epoch,id:other});assert.equal(run('images.upload'),null);assert.equal(run('images.bytes'),16);
assert.throws(()=>control({type:'canvas-chunk',epoch,id:other,offset:4,length:4}),/Invalid canvas/); // Tail cannot resurrect.
control({type:'canvas-release',epoch,id:other});control({type:'canvas-release',epoch,id:rid});assert.equal(run('images.bytes'),0);
resource(other,1,1);assert.throws(()=>binary([1,2,3,4]),/Invalid canvas/);
control({type:'canvas-chunk',epoch,id:other,offset:0,length:4});assert.throws(()=>binary([1]),/Invalid canvas/);
control({type:'canvas-reset',epoch});resource(other,1,1);chunk(other,0,[1,2,3,4]);
run('images.resources.get("'+other+'").texture={old:true};images.lost()');assert.equal(run('images.resources.get("'+other+'").texture'),null);assert.equal(run('images.bytes'),4);
control({type:'canvas-reset',epoch:'d'.repeat(48)});assert.equal(run('images.bytes'),0);assert.equal(access.hidden,true);
assert.throws(()=>control({type:'canvas-chunk',epoch,id:other,offset:0,length:4}),/Invalid canvas/);
control({type:'canvas-reset',epoch});
for(const bad of [
 {width:0,height:2,bytes:0},{width:4097,height:1,bytes:16388},{width:4096,height:4096,bytes:67108864},{width:2,height:2,bytes:15}
])assert.throws(()=>control({type:'canvas-resource',epoch,id:rid,...bad}),/Invalid canvas/);
for(let i=0;i<4;i++){resource(i.toString(16).padStart(48,'0'),2048,2048);run('images.upload.received=images.upload.data.length-1');chunk(i.toString(16).padStart(48,'0'),16777215,[0]);}
assert.equal(run('images.bytes'),67108864);assert.throws(()=>resource(),/Invalid canvas/);
control({type:'canvas-reset',epoch});resource();
for(const bad of [
 {...surface,slot:65},{...surface,target:[0,0,NaN,2]},{...surface,target:[0,0,0,2]}, {...surface,rect:[1,0,2,2]}, {...surface,name:'x'.repeat(257)}, {...surface,description:'x'.repeat(1025)}
]){assert.throws(()=>scene([bad]),/Invalid canvas/);assert.equal(access.hidden,true);}
assert.throws(()=>scene([surface],[2,0,0,0]),/Invalid canvas/);
scene([surface],[0x8000,0,0,0]);assert.equal(access.hidden,true); // A halo bit without an image owner has no image paint.
assert.throws(()=>scene([surface,surface]),/Invalid canvas/);
context.value={epoch,surfaces:[],mask:''};run('images.receive(value,2,2)');assert.equal(access.hidden,true);assert.equal(run('images.scene'),null);
context.value={epoch,surfaces:[surface],mask:''};assert.throws(()=>run('images.receive(value,2,2)'),/Invalid canvas/);
run('images.clear()');assert.equal(run('images.upload'),null);assert.equal(run('images.resources.size'),0);assert.equal(run('images.epoch'),null);
// Live handlers target the published image, preserve native focus, and cannot be
// redirected by replacement metadata, occlusion, a modal, or attachment reset.
control({type:'canvas-reset',epoch});
const controls={id:1,view:'18446744073709551615',resource:rid,anchor:[0,0]},interactive={...surface,controls};
scene([interactive]);
const group=access.children[0],buttons=group.children.filter(item=>item.tagName==='button');
assert.equal(buttons.length,4,'the image exposes four native action buttons');
assert.deepEqual(buttons.map(item=>item.textContent),['Fit Image','Actual Size','Zoom In','Zoom Out']);
const editorSource=fs.readFileSync('assets/web/editor.js','utf8'),handlers=new Map();
Object.assign(context,{window:{addEventListener:(name,handler)=>handlers.set(name,handler)},imageAccess:access,
 sidebarAccess:new Element(),dialogAccess:new Element(),sourceAccess:new Element()});
vm.runInContext(editorSource.slice(editorSource.indexOf('function semanticReadingTarget('),editorSource.indexOf("window.addEventListener('keyup'")),context);
for(const key of ['Enter',' '])handlers.get('keydown')({target:buttons[0],key,preventDefault:()=>assert.fail('image controls retain native keyboard activation')});
assert.equal(sent.length,0);
buttons.forEach(button=>button.click());
assert.deepEqual(sent,['fit','actual-size','zoom-in','zoom-out'].map(action=>({type:'canvas-action',target:controls,action})));
buttons[2].focus();const oldAnchor=buttons[2].onclick;
const movedControls={...controls,anchor:[1,0]};
scene([{...interactive,name:'moved image',target:[-1,0,4,4],controls:movedControls}]);
assert.equal(access.children[0],group);assert.equal(document.activeElement,buttons[2]);
oldAnchor();assert.equal(sent.length,4);buttons[2].click();assert.deepEqual(sent.at(-1),{type:'canvas-action',target:movedControls,action:'zoom-in'});
const liveClick=buttons[0].onclick;
access.setAttribute('aria-hidden','true');buttons[0].click();assert.equal(sent.length,5);access.removeAttribute('aria-hidden');
access.hidden=true;buttons[0].click();assert.equal(sent.length,5);access.hidden=false;
scene([{...interactive,controls:null}]);liveClick();assert.equal(sent.length,5);assert.equal(access.children[0].children.filter(item=>item.tagName==='button').length,0);
scene([interactive]);const retired=access.children[0].children.find(item=>item.tagName==='button').onclick;
scene([{...interactive,controls:null}],[0,0,0,0]);retired();assert.equal(sent.length,5);assert.equal(access.hidden,true);
scene([{...interactive,controls:{...controls,view:'2'}}]);retired();assert.equal(sent.length,5);
const replaced=access.children[0].children.find(item=>item.tagName==='button').onclick;
scene([{...interactive,resource:other,controls:{...controls,view:'2',resource:other}}]);replaced();assert.equal(sent.length,5);
const reset=access.children[0].children.find(item=>item.tagName==='button').onclick;
control({type:'canvas-reset',epoch});scene([{...interactive,resource:other,controls:{...controls,view:'2',resource:other}}]);reset();assert.equal(sent.length,5);
for(const invalidControls of [{}, {...controls,id:2},{...controls,resource:other},
 {...controls,view:'01'},{...controls,view:'0'},{...controls,view:'1'.repeat(21)},{...controls,view:1},
 {...controls,anchor:[0,1]},{...controls,anchor:[2,0]},{...controls,anchor:[0.5,0]}, {...controls,anchor:[0,0,0]}]){
 assert.throws(()=>scene([{...interactive,controls:invalidControls}]),/Invalid canvas/);assert.equal(access.hidden,true);
}
run('images.clear()');
console.log('Canvas resources and controls: bounded bytes and scenes, retained focus, exact action targets, hidden/stale rejection, and epoch cleanup passed');
