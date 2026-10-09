// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
// Production resource/scene handlers: bounded assembly and replacement lifecycle.
import fs from 'node:fs';
import vm from 'node:vm';
import assert from 'node:assert/strict';
class Element {
 constructor(){this.children=[];this.attributes={};this.hidden=true;}
 replaceChildren(){this.children=[];}append(item){this.children.push(item);}
 setAttribute(key,value){this.attributes[key]=value;}
}
const access=new Element(),removed=[];
const context=vm.createContext({Uint8Array,Uint32Array,ArrayBuffer,atob,document:{createElement:()=>new Element()},gl:{deleteTexture:t=>removed.push(t)},access});
vm.runInContext(fs.readFileSync('assets/web/canvas-images.js','utf8')+'\nconst images=new CanvasImages(gl,access);',context);
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
const surface={id:1,resource:rid,rect:[0,0,2,2],target:[0,0,2,2],slot:1,name:'safe <script> λ.png',description:'2 × 2 PNG'};
control({type:'canvas-reset',epoch});resource();
scene([surface]);assert.equal(access.children[0].attributes['aria-label'],surface.name);assert.equal(access.children[0].textContent,surface.name);
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
console.log('Canvas resources: bounded contiguous bytes, scene/mask validation, inert names, occlusion retention, release/reset/old-epoch cleanup passed');
