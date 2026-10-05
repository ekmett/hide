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
 for(const flags of [8,16,24])assert.equal(vm.runInContext(`tile('${glyph}',0xffffff,false,8,16,${flags})`,context),tiles[0]);
 assert.equal(vm.runInContext(`tile('${glyph}',0xffffff,false,8,16,27)`,context),tiles[3]);
 if(glyph==='A')assert.equal(new Set(tiles.map(tile=>JSON.stringify(tile.pixels))).size,4);
 else assert.deepEqual(tiles.map(tile=>tile.font),['13.6px monospace','bold 13.6px monospace','italic 13.6px monospace','italic bold 13.6px monospace']);
}
console.log('Browser font traits, bitmap pixels, unchanged geometry and cache checks passed');

const wide=vm.runInContext("tile('f',0xffffff,false,16,16,4)",context);
assert.equal(wide.width,16);assert.deepEqual(wide.transform,[2,1]);
assert.notEqual(wide,vm.runInContext("tile('f',0xffffff,false,16,16,0)",context));
assert.equal(wide,vm.runInContext("tile('f',0xffffff,false,16,16,28)",context));
console.log('Browser two-cell glyph stretch and cache checks passed');

// Exercise the production row packer: preserve full glyph geometry while
// advancing by visible cells, then upload only that row's compact metadata.
const uploads=[],shaped=[];
Object.assign(context,{frame:{pixelated:false},metrics:()=>[8,16],cols:5,lines:1,gridCols:5,gridRows:1,cellGrid:new Uint32Array(40),atlasEntry:(text,fg,pixelated,w,h,traits)=>{shaped.push([text,w,h,traits]);return [11|(22<<16),16|(16<<16),2];},performance:{now:()=>0},gl:{TEXTURE1:1,TEXTURE_2D:2,RGBA_INTEGER:3,UNSIGNED_INT:4,activeTexture:()=>{},bindTexture:()=>{},texSubImage2D:(...args)=>uploads.push(Array.from(args.at(-1)))},cellTexture:{},atlasStats:{gridBytes:0},rasterTime:0,dirty:false});
vm.runInContext(source.slice(source.indexOf('function drawRows('),source.indexOf('function present(')),context);
for(const clipStart of [0,1]){
 uploads.length=0;shaped.length=0;
 vm.runInContext(`drawRows([[0,[[0,0xffffff,0,24,[['f',2,true,${clipStart},1],['A',1,false,0,1]]]]]])`,context);
 assert.equal(uploads.length,1);assert.equal(uploads[0].length,5*8);
 assert.equal(uploads[0][2],2|(clipStart<<16));
 assert.equal(uploads[0][6],27);assert.equal(uploads[0][8+6],27);assert.equal(uploads[0][16+6],3);
 assert.equal(uploads[0][8+2],1);assert.equal(uploads[0][16+2],1);
 assert.deepEqual(shaped.find(([text])=>text==='f'),['f',16,16,4]);
}
console.log('Browser original glyph UV offsets, following-cell placement and bounded grid upload checks passed');

vm.runInContext(source.slice(source.indexOf('const scriptSegments='),source.indexOf('function command(')),context);
for(const mode of ['sup','sub'])for(const [text,natural] of [['A',1],['界',2],['é',1],['👩🏽‍💻',2]]){
 uploads.length=0;shaped.length=0;
 const script=mode==='sup'?1:2;
 vm.runInContext(`drawRows(decodeRows([[0,[[0,0xffffff,0,27,[[${JSON.stringify(text)},${natural},'${mode}'],'Z']]]]]))`,context);
 assert.equal(uploads[0][2],1|(natural<<2)|(script<<4));
 assert.equal(uploads[0][8+2],1); // Following text starts in the next cell.
 assert.deepEqual(shaped.find(([glyph])=>glyph===text),[text,natural*8,16,3]);
}
for(const run of [['',1,'sup'],['AB',1,'sup'],['A',0,'sup'],['A',3,'sub'],['A',1,'bad'],['\n',1,'sup']]){
 assert.throws(()=>vm.runInContext(`decodeRows([[0,[[0,0,0,0,[${JSON.stringify(run)}]]]]])`,context));
}
for(const flags of [4,32,-1])assert.throws(()=>vm.runInContext(`decodeRows([[0,[[0,0,0,${flags},['A']]]]])`,context));
console.log('Browser script runs preserve natural atlas size, one-cell advance and paint; invalid runs rejected');
