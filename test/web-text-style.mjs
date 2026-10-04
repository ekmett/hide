// Exercise the actual browser tile builder: bitmap pixels, font selection,
// unchanged tile geometry, and cache identity. No visible browser is launched.
import fs from 'node:fs';
import vm from 'node:vm';
import assert from 'node:assert/strict';
const source=fs.readFileSync('assets/web/editor.js','utf8');
const document={createElement:()=>{
 const canvas={width:0,height:0};
 const context={createImageData:(w,h)=>({data:new Uint8ClampedArray(w*h*4)}),putImageData:image=>canvas.pixels=Array.from(image.data),scale:(x,y)=>canvas.transform=[x,y],fillText:()=>canvas.font=context.font};
 canvas.getContext=()=>context;return canvas;
}};
const context=vm.createContext({document,glyphs:new Map([['A',[8,Array(16).fill(0x1000)]]]),tiles:new Map(),scale:1,devicePixelRatio:1,cellHeight:()=>16,rgb:()=> '#fff'});
vm.runInContext(source.slice(source.indexOf('function bitmapInk('),source.indexOf('function drawRows(')),context);
for(const glyph of ['A','f']){
 const tiles=[0,1,2,3].map(traits=>vm.runInContext(`tile('${glyph}',0xffffff,false,8,16,${traits})`,context));
 for(const tile of tiles){assert.equal(tile.width,8);assert.equal(tile.height,16);}
 assert.equal(vm.runInContext(`tile('${glyph}',0xffffff,false,8,16,0)`,context),tiles[0]);
 assert.equal(new Set(tiles).size,4);
 if(glyph==='A')assert.equal(new Set(tiles.map(tile=>JSON.stringify(tile.pixels))).size,4);
 else assert.deepEqual(tiles.map(tile=>tile.font),['13.6px monospace','bold 13.6px monospace','italic 13.6px monospace','italic bold 13.6px monospace']);
}
console.log('Browser font traits, bitmap pixels, unchanged geometry and cache checks passed');

const wide=vm.runInContext("tile('f',0xffffff,false,16,16,4)",context);
assert.equal(wide.width,16);assert.deepEqual(wide.transform,[2,1]);
assert.notEqual(wide,vm.runInContext("tile('f',0xffffff,false,16,16,0)",context));
console.log('Browser two-cell glyph stretch and cache checks passed');
