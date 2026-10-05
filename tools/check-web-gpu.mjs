// SPDX-License-Identifier: BSD-3-Clause
// Execute the production WebGL2 grid shader headlessly; no server or visible UI.
import fs from 'node:fs/promises';
import os from 'node:os';
import path from 'node:path';
import {spawn} from 'node:child_process';
const browser=process.env.HIDE_TEST_BROWSER||(process.platform==='darwin'?'/Applications/Google Chrome.app/Contents/MacOS/Google Chrome':'chromium');
const source=await fs.readFile('assets/web/editor.js','utf8');
const shader=await fs.readFile('assets/web/cell-shader.js','utf8');
const checks=String.raw`
try {
 glyphs=new Map([['A',[8,Array(16).fill(0x9000)]],['H',[16,Array(16).fill(0x00ff)]]]);
 frame={title:'GPU fixture',size:[80,25],cursor:null,blink:false,crt:false,pixelated:false};
 scale=1;cols=80;lines=25;
 rows=Array.from({length:25},()=>[]);
 rows[1]=[[0,0xffffff,0x0000aa,0,[['中',2,false,1,1]]],[2,0xffffff,0x0000aa,0,[['👩🏽‍💻',2,false,1,1]]],[4,0xffffff,0x0000aa,0,[['中',2,false,0,2]]],[8,0xffffff,0x0000aa,0,[['👩🏽‍💻',2,false,0,2]]],[12,0xffff55,0x0000aa,3,[['é',1,false,0,1]]]];
 rows[0]=[[0,0xffffff,0x0000aa,0,[['A',1,false,0,1]]],[2,0xffffff,0x0000aa,0,[['H',2,false,1,1]]]];
 allocate();present(performance.now());
 function check(condition,message){if(!condition)throw new Error(message);}
 function pixel(x,y){const p=new Uint8Array(4);gl.readPixels(x,canvas.height-y-1,1,1,gl.RGBA,gl.UNSIGNED_BYTE,p);return Array.from(p).slice(0,3).join(',');}
 check(gl.getError()===gl.NO_ERROR,'GL initialization/draw error');
 check(pixel(0,0)==='255,255,255','known foreground '+pixel(0,0));
 check(pixel(1,0)==='0,0,170','known background '+pixel(1,0));
 check(pixel(16,0)==='255,255,255','visible right half '+pixel(16,0));
 check(pixel(24,0)==='0,0,170','neighbor after clipped glyph '+pixel(24,0));
 check((cellGrid[2*8+2]>>>16)===1,'full glyph UV offset retained');
 for(let y=16;y<32;++y)for(let x=0;x<8;++x){check(pixel(x,y)===pixel(40+x,y),'CJK half UV mismatch');check(pixel(16+x,y)===pixel(72+x,y),'ZWJ emoji half UV mismatch');}
 const tilesBefore=atlasStats.tiles,gridBefore=atlasStats.gridBytes;
 dirty=true;present(performance.now());
 check(atlasStats.tiles===tilesBefore&&atlasStats.gridBytes===gridBefore,'warm present uploaded cells/tiles');
 drawRows([[0,rows[0]]]);present(performance.now());
 check(atlasStats.tiles===tilesBefore,'warm row reshaped glyphs');
 check(atlasStats.gridBytes-gridBefore===80*32,'changed row upload is bounded metadata');
 mouse=[0,0];dirty=true;present(performance.now());check(pixel(0,0)==='85,85,85','mouse palette '+pixel(0,0));
 mouse=[-1,-1];frame.cursor=[0,0];dirty=true;present(performance.now());check(pixel(0,15)==='0,0,0','cursor shader '+pixel(0,15));
 frame.cursor=null;
 // Blank bitmap/shaped glyphs make line pixels independent of font contours.
 glyphs.set('_',[8,Array(16).fill(0)]);
 const decorations=flags=>[[0,0xffffff,0x0000aa,flags&8,[['_',1,false,0,1]]],[2,0xffffff,0x0000aa,flags&16,[['_',1,false,0,1]]],[4,0xffffff,0x0000aa,flags,[['_',1,false,0,1]]],[8,0xffffff,0x0000aa,flags,[['_',2,true,0,2]]],[12,0xffffff,0x0000aa,flags,[['_',2,true,1,1]]],[14,0xffffff,0x0000aa,flags,[['_',2,true,0,1]]],[17,0xffffff,0x0000aa,3|flags,[[' ',2,true,0,2]]]];
 drawRows([[3,decorations(0)]]);present(performance.now());const undecoratedTiles=atlasStats.tiles;
 drawRows([[3,decorations(24)]]);present(performance.now());
 check(atlasStats.tiles===undecoratedTiles,'decorations allocated atlas tiles');
 check(pixel(0,63)==='255,255,255'&&pixel(0,55)==='0,0,170','underline row');
 check(pixel(16,55)==='255,255,255'&&pixel(16,63)==='0,0,170','strike row');
 for(let x=32;x<40;++x)check(pixel(x,55)==='255,255,255'&&pixel(x,63)==='255,255,255','combined lines');
 for(let x=64;x<80;++x)check(pixel(x,63)==='255,255,255','two-cell underline');
 check(pixel(96,63)==='255,255,255'&&pixel(104,63)==='0,0,170','right-half decoration clip');
 check(pixel(112,63)==='255,255,255'&&pixel(120,63)==='0,0,170','left-half decoration clip');
 for(let x=136;x<152;++x)check(pixel(x,55)==='255,255,255','shaped two-cell strike');
 check(pixel(32,53)==='0,0,170','decorations changed other rows');
 frame.cursor=[4,3];dirty=true;present(performance.now());check(pixel(32,63)==='0,0,0','decorated cursor paint');
 frame.cursor=null;mouse=[4,3];dirty=true;present(performance.now());check(pixel(32,63)==='85,85,85','decorated mouse paint');mouse=[-1,-1];
 check(atlasStats.tiles===undecoratedTiles,'cursor/mouse reshaped decorated glyph');
 mode=259;rows[3]=decorations(24);allocate();present(performance.now());
 check(pixel(0,31)==='255,255,255'&&pixel(16,27)==='255,255,255'&&pixel(16,31)==='0,0,170','8-row decoration mode');
 // A solid normal-resolution tile makes the half-size ink geometry exact.
 glyphs.set('S',[8,Array(16).fill(0xff00)]);glyphs.set('界',[16,Array(16).fill(0xffff)]);
 for(const pixelated of [false,true]){
   frame.pixelated=pixelated;
   drawRows([[5,[[0,0xffffff,0x0000aa,0,[['S',1,false,0,1],['界',2,false,0,2]]]]]]);
   const scriptTiles=atlasStats.tiles;
   rows[6]=decodeRows([[6,[[0,0xffffff,0x0000aa,0,[['S',1,'sup'],['S',1,'sub'],['界',2,'sup'],['界',2,'sub'],' ']]]]])[0][1];
   drawRows([[6,rows[6]]]);present(performance.now());
   check(atlasStats.tiles===scriptTiles,'script allocated a new atlas tile');
   const h=cellHeight(),top=6*h;
   for(let y=0;y<h;++y)for(let x=0;x<40;++x){
     const cell=Math.floor(x/8),within=x%8;
     const ink=(cell<2?within<4:true)&&(cell%2===0?y<h/2:y>=h/2)&&cell<4;
     check(pixel(x,top+y)===(ink?'255,255,255':'0,0,170'),'script ink geometry '+[pixelated,x,y,pixel(x,top+y)]);
   }
   mouse=[3,6];dirty=true;present(performance.now());
   check(pixel(24,top)==='170,85,0'&&pixel(24,top+h-1)==='85,85,85','script mouse transforms the allocated cell');
   mouse=[-1,-1];frame.cursor=[0,6];dirty=true;present(performance.now());
   check(pixel(0,top+h-1)==='255,255,85','script cursor keeps the allocated bottom band');frame.cursor=null;
 }
 for(let i=0;i<520;++i)atlasEntry('e',i,false,128,64,0);
 check(atlasSize>2048,'GPU atlas growth did not occur');dirty=true;present(performance.now());
 check(pixel(0,0)==='255,255,255','growth changed earlier tile rectangle');
 check(gl.getError()===gl.NO_ERROR,'GL final error');
 document.body.textContent='PASS WebGL2 generated HLSL execution: foreground/background, full-origin half clip, warm atlas/grid reuse, bounded row upload, mouse/cursor, cell underline/strike and one-cell script ink without new atlas tiles; '+JSON.stringify(atlasStats)+'; '+gl.getParameter(gl.getExtension('WEBGL_debug_renderer_info').UNMASKED_RENDERER_WEBGL);
} catch(error){document.body.textContent='FAIL '+error.stack;}
`;
const fixture=await fs.mkdtemp(path.join(os.tmpdir(),'hide-web-gpu-'));
try {
 const html='<html><body>'+['screen','status','input','fullscreen','clipboard-action','open-resource'].map(id=>`<div id="${id}"></div>`).join('')+'<canvas id="display"></canvas><script>window.requestAnimationFrame=()=>0;</script><script>'+shader+'</script><script>'+source.slice(0,source.indexOf('function command(name)'))+'</script><script>'+checks+'</script></body></html>';
 const file=path.join(fixture,'fixture.html');await fs.writeFile(file,html);
 const args=['--headless=new','--no-first-run','--no-default-browser-check',`--user-data-dir=${path.join(fixture,'profile')}`,'--dump-dom',`file://${file}`];
 if(process.env.HIDE_TEST_ANGLE)args.unshift(`--use-angle=${process.env.HIDE_TEST_ANGLE}`);
 await new Promise((resolve,reject)=>{
   const child=spawn(browser,args,{detached:process.platform!=='win32'});
   const stop=signal=>{try{if(process.platform==='win32')child.kill(signal);else process.kill(-child.pid,signal);}catch(error){if(error.code!=='ESRCH')throw error;}};
   let output='',errors='',result=null,killer=null,timedOut=false;
   const timeout=setTimeout(()=>{timedOut=true;stop('SIGKILL');},30000);
   child.stderr.on('data',data=>errors+=data);
   child.on('error',error=>{clearTimeout(timeout);reject(error);});
   child.stdout.on('data',data=>{
     output+=data;
     const completed=output.match(/<body>(PASS[^<]*|FAIL[\s\S]*?)<\/body>/);
     if(completed&&!result){result=completed[1];stop('SIGTERM');killer=setTimeout(()=>stop('SIGKILL'),2000);}
   });
   // The fixture owns the whole isolated process group. Wait for its pipes to
   // close before removing the profile, rather than racing Chrome's teardown.
   child.on('close',code=>{
     clearTimeout(timeout);clearTimeout(killer);
     if(result?.startsWith('PASS')){console.log(result);resolve();}
     else reject(new Error(result||(timedOut?'Headless GPU check timed out':`Browser exited ${code}: ${output} ${errors}`)));
   });
 });
} finally {await fs.rm(fixture,{recursive:true,force:true});}
