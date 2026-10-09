// Run against packets from tools/web-wire-trial.hs. Exercises the production
// decoder with actual Haskell packets, both encodings, resets, and >32 KiB screens.
import fs from 'node:fs';
import assert from 'node:assert/strict';
const source=fs.readFileSync('assets/web/editor.js','utf8');
const decoder=source.match(/^async function decodeFrame\([\s\S]*?^}/m)?.[0];
assert.ok(decoder,'Production decodeFrame function is present');
const decode=new Function(decoder+';return decodeFrame;')();
const states=new Map(), totals=new Map();
let frames=0;
for(const line of fs.readFileSync(process.argv[2],'utf8').trim().split('\n')){
 const sample=JSON.parse(line), key=sample.screen.join('x');
 const previous=states.get(key)||[];
 const packet=Uint8Array.from(sample.packet);
 assert.equal(packet.length,Math.min(...sample.candidates.map(x=>x.length)));
 for(const candidate of sample.candidates){
   const scratch=[...previous];
   await decode(Uint8Array.from(candidate),scratch);
   assert.deepEqual(scratch,sample.rows);
 }
 await decode(packet,previous);
 assert.deepEqual(previous,sample.rows);
 states.set(key,previous);
 const count=totals.get(key)||{bytes:0,modes:[0,0,0],frames:0};
 count.bytes+=packet.length+(packet.length<126?2:packet.length<65536?4:10);
 count.modes[packet[0]]++;count.frames++;totals.set(key,count);frames++;
}
console.log(JSON.stringify(Object.fromEntries(totals),null,2));
console.log(`Native browser decoder verified ${frames} Haskell frames and every candidate.`);
