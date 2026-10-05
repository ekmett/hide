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

// Exercise actual row drawing with a half-visible semantic glyph. Shape/cache
// the original full tile, crop source pixels, then advance only visible cells.
const drawn=[],fills=[];
Object.assign(context,{frame:{pixelated:false},metrics:()=>[8,16],ctx:{fillRect:(...args)=>fills.push(args),drawImage:(...args)=>drawn.push(args)},surface:{width:40},performance:{now:()=>0},gl:{TEXTURE_2D:0,RGBA:0,UNSIGNED_BYTE:0,bindTexture:()=>{},texImage2D:()=>{}},texture:{},rasterTime:0,dirty:false});
vm.runInContext(source.slice(source.indexOf('function drawRows('),source.indexOf('function present(')),context);
for(const clipStart of [0,1]){
 drawn.length=0;fills.length=0;
 vm.runInContext(`drawRows([[0,[[0,0xffffff,0,0,[['f',2,true,${clipStart},1],['A',1,false,0,1]]]]]])`,context);
 assert.equal(drawn[0][0].width,16);
 assert.deepEqual(drawn[0].slice(1),[clipStart*8,0,8,16,0,0,8,16]);
 assert.deepEqual(fills.slice(1),[[0,0,8,16],[8,0,8,16]]);
 assert.equal(drawn[1][5],8);
}
console.log('Browser partial glyph UV crop, original tile and following-cell placement checks passed');
