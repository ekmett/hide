'use strict';
const canvas = document.querySelector('#display');
const screen = document.querySelector('#screen');
const status = document.querySelector('#status');
const input = document.querySelector('#input');
const fullscreen = document.querySelector('#fullscreen');
const clipboardAction = document.querySelector('#clipboard-action');
const gl = canvas.getContext('webgl', {alpha:false, antialias:false, preserveDrawingBuffer:true});
if (!gl) {status.textContent='WebGL is unavailable in this browser.';throw new Error(status.textContent);}
const vertex = `attribute vec2 position; varying vec2 uv; void main(){uv=(position+1.0)*0.5; gl_Position=vec4(position,0,1);}`;
const fragment = `precision highp float;
varying vec2 uv; uniform sampler2D image; uniform vec2 resolution,grid; uniform vec2 mouse,caret;
uniform bool crt,caretOn; uniform float glyphPitch;
vec3 palette(float i){
 float high=floor(i/8.0)*85.0; float low=mod(i,8.0);
 vec3 c=vec3(mod(low,2.0),mod(floor(low/2.0),2.0),mod(floor(low/4.0),2.0))*170.0+high;
 if(i==3.0)c.g=85.0;return c/255.0;
}
void main(){
 vec2 p=vec2(uv.x,1.0-uv.y); vec3 c=texture2D(image,p).rgb;
 vec2 cell=floor(p*grid); vec2 within=fract(p*grid);
 if(caretOn && all(equal(cell,caret)) && within.y>=0.875) c=1.0-c;
 if(all(equal(cell,mouse))) {
   vec3 b=floor(c*255.0+0.5); vec3 mask=vec3(170.0);
   vec3 original=c;
   c=(b+mask-2.0*(mod(floor(b/2.0),2.0)*2.0+mod(floor(b/8.0),2.0)*8.0+mod(floor(b/32.0),2.0)*32.0+mod(floor(b/128.0),2.0)*128.0))/255.0;
   for(int j=0;j<16;j++){float i=float(j);if(all(lessThan(abs(original-palette(i)),vec3(0.001))))c=palette(i<8.0?7.0-i:23.0-i);}
 }
 if(crt){
   vec2 n=p*2.0-1.0; float radius=dot(n,n)/2.0;
   if(glyphPitch>=2.0 && mod(floor(p.y*resolution.y)+1.0,glyphPitch)<1.0) c*=1.0-24.0/255.0;
   c*=1.0-(100.0/255.0)*radius*radius;
 }
 gl_FragColor=vec4(c,1);
}`;
function shader(type, source) {
 const s=gl.createShader(type); gl.shaderSource(s,source); gl.compileShader(s);
 if(!gl.getShaderParameter(s,gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(s));
 return s;
}
const program=gl.createProgram(); gl.attachShader(program,shader(gl.VERTEX_SHADER,vertex)); gl.attachShader(program,shader(gl.FRAGMENT_SHADER,fragment)); gl.linkProgram(program);
if(!gl.getProgramParameter(program,gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(program));
gl.useProgram(program);
const vertices=gl.createBuffer(); gl.bindBuffer(gl.ARRAY_BUFFER,vertices); gl.bufferData(gl.ARRAY_BUFFER,new Float32Array([-1,-1,1,-1,-1,1,1,1]),gl.STATIC_DRAW);
const pos=gl.getAttribLocation(program,'position'); gl.enableVertexAttribArray(pos); gl.vertexAttribPointer(pos,2,gl.FLOAT,false,0,0);
const texture=gl.createTexture(); gl.bindTexture(gl.TEXTURE_2D,texture);
gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MIN_FILTER,gl.NEAREST); gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_MAG_FILTER,gl.NEAREST);
gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_WRAP_S,gl.CLAMP_TO_EDGE); gl.texParameteri(gl.TEXTURE_2D,gl.TEXTURE_WRAP_T,gl.CLAMP_TO_EDGE);
const uniforms=Object.fromEntries(['resolution','grid','mouse','caret','crt','caretOn','glyphPitch'].map(k=>[k,gl.getUniformLocation(program,k)]));
const surface=document.createElement('canvas'); const ctx=surface.getContext('2d',{alpha:false});
let glyphs=new Map(), tiles=new Map(), rows=[], frame=null, scale=2, initialScale=2, cols=80, lines=25, mode=3;
let socket, ready=false, closed=false, mouse=[-1,-1], cursorEpoch=performance.now(), blinkPhase=-1, dirty=true, composing=false, clipboard='', lastSize='';
let remoteHost="", sessionFrontend=false, detaching=false, detached=false;
const drawTimes=[];
let titleTick=0, timingText='', rasterTime=0;
function updateTitle(){
 if(!frame)return;
 const base=remoteHost?frame.title.replace(/^th(?: |$)/,`th ${remoteHost}:`):frame.title;
 document.title=base+timingText;
}
let downloadName=null, clipboardRequest=null, nativeCopies=[];
let unsaved=false, serial=0, pendingEdit=0, acknowledged=0;
function beforeLeave(event){event.preventDefault();event.returnValue=true;}
function guardLeave(){
 const warn=!closed&&(unsaved||pendingEdit>acknowledged);
 if(warn)window.addEventListener('beforeunload',beforeLeave);
 else window.removeEventListener('beforeunload',beforeLeave);
}
const rgb=n=>`#${n.toString(16).padStart(6,'0')}`;
function send(value){
 if(socket?.readyState!==WebSocket.OPEN||!ready||detaching)return;
 value.seq=++serial;
 if(['key','paste','command','upload','mouse'].includes(value.type)){pendingEdit=serial;guardLeave();}
 socket.send(JSON.stringify(value));
}
function mods(e){return [e.shiftKey?'shift':null,(e.ctrlKey||e.metaKey)?'ctrl':null,e.altKey&&!e.getModifierState?.('AltGraph')?'alt':null].filter(Boolean);}
function cellHeight(){return mode===259?8:16;}
function metrics(){return [surface.width/cols,surface.height/lines];}
function resize(){
 if(!ready)return;
 const w=Math.max(40,Math.min(512,Math.floor(screen.clientWidth/(8*scale))));
 const h=Math.max(12,Math.min(256,Math.floor(screen.clientHeight/(cellHeight()*scale))));
 const key=`${w},${h}`;
 if(key!==lastSize){lastSize=key;send({type:'resize',width:w,height:h});}
 allocate();
}
function allocate(){
 if(!frame)return;
 const dpr=window.devicePixelRatio||1;
 const width=Math.round(cols*8*scale*dpr), height=Math.round(lines*cellHeight()*scale*dpr);
 canvas.style.width=`${cols*8*scale}px`; canvas.style.height=`${lines*cellHeight()*scale}px`;
 if(canvas.width!==width||canvas.height!==height){
   canvas.width=surface.width=width;canvas.height=surface.height=height;
   ctx.imageSmoothingEnabled=false;gl.viewport(0,0,width,height);drawRows(rows.map((r,y)=>[y,r]));
 }
 dirty=true;
}
function tile(text,fg,pixelated,w,h){
 const bitmap=glyphs.get(text), key=JSON.stringify([text,fg,pixelated,w,h]);
 if(tiles.has(key))return tiles.get(key);
 const t=document.createElement('canvas');
 if(bitmap){
   t.width=bitmap[0];t.height=16;const c=t.getContext('2d'), image=c.createImageData(t.width,16);
   for(let y=0;y<16;y++)for(let x=0;x<t.width;x++)if(bitmap[1][y]&(1<<(15-x))){const i=(y*t.width+x)*4;image.data.set([fg>>16,(fg>>8)&255,fg&255,255],i);}
   c.putImageData(image,0,0);
 }else{
   // Shape an entire grapheme in one canvas operation, preserving emoji sequences.
   t.width=pixelated?Math.max(8,Math.round(w/(8*scale*(devicePixelRatio||1)))*8):Math.max(1,Math.ceil(w));
   t.height=pixelated?16:Math.max(16,Math.ceil(h*16/cellHeight()));
   const c=t.getContext('2d');c.fillStyle=rgb(fg);c.textBaseline='alphabetic';c.font=`${t.height*0.85}px monospace`;
   c.fillText(text,0,t.height*0.82,t.width);
 }
 if(tiles.size>=1024)tiles.clear();tiles.set(key,t);return t;
}
function drawRows(changed){
 if(!frame)return;const started=performance.now();const [cw,ch]=metrics();
 for(const [y,spans] of changed){
   const y0=Math.round(y*ch), y1=Math.round((y+1)*ch);ctx.fillStyle='#0000aa';ctx.fillRect(0,y0,surface.width,y1-y0);
   for(const [start,fg,bg,clusters] of spans||[]){let x=start;
     for(const [text,width] of clusters){
       const x0=Math.round(x*cw),x1=Math.round((x+width)*cw);
       ctx.fillStyle=rgb(bg);ctx.fillRect(x0,y0,x1-x0,y1-y0);
       if(width>0&&text!==' ')ctx.drawImage(tile(text,fg,frame.pixelated,x1-x0,y1-y0),x0,y0,x1-x0,y1-y0);
       x+=width;
     }
   }
 }
 gl.bindTexture(gl.TEXTURE_2D,texture);gl.texImage2D(gl.TEXTURE_2D,0,gl.RGBA,gl.RGBA,gl.UNSIGNED_BYTE,surface);
 rasterTime+=performance.now()-started;dirty=true;
}
function present(now){
 const phase=!frame?.blink||Math.floor((now-cursorEpoch)/500)%2===0;
 if(frame&&(dirty||phase!==blinkPhase)){
   const started=performance.now();
   gl.uniform2f(uniforms.resolution,canvas.width,canvas.height);gl.uniform2f(uniforms.grid,cols,lines);
   gl.uniform2f(uniforms.mouse,...mouse);gl.uniform2f(uniforms.caret,...(frame.cursor||[-1,-1]));
   gl.uniform1i(uniforms.caretOn,phase&&!!frame.cursor);gl.uniform1i(uniforms.crt,frame.crt);
   gl.uniform1f(uniforms.glyphPitch,canvas.height/(lines*16));gl.drawArrays(gl.TRIANGLE_STRIP,0,4);dirty=false;blinkPhase=phase;
   drawTimes.push(rasterTime+performance.now()-started);rasterTime=0;if(drawTimes.length>60)drawTimes.shift();
 }
 if(now-titleTick>=1000&&drawTimes.length){
   timingText=` | ${(drawTimes.reduce((a,b)=>a+b,0)/drawTimes.length).toFixed(1)} ms/frame`;
   titleTick=now;updateTitle();
 }
 requestAnimationFrame(present);
}
requestAnimationFrame(present);
function command(name){send({type:'command',command:name});}
async function systemClipboard(request){
 try{
   if(request.type==='copy')await navigator.clipboard.writeText(request.text);
   else send({type:'paste',text:await navigator.clipboard.readText()});
   clipboardRequest=null;clipboardAction.hidden=true;input.focus({preventScroll:true});
 }catch(error){
   clipboardRequest=request;clipboardAction.textContent=request.type==='copy'?'Copy to clipboard':'Paste from clipboard';clipboardAction.hidden=false;
   status.textContent='Clipboard access needs a click, or use the browser Copy/Paste shortcut.';
 }
}
clipboardAction.addEventListener('click',()=>{if(clipboardRequest)systemClipboard(clipboardRequest);});
// A stored, nonfinal DEFLATE block seeds the native decoder with the previous
// screen. Only the compressed tail travels over the socket. RFC 1951 section 3.2.4.
async function decodeFrame(bytes, previous){
 const tag=bytes[0];
 if(tag>2)throw new Error('Unknown display encoding');
 const dictionary=tag===0?new Uint8Array():new TextEncoder().encode(JSON.stringify(previous)).slice(-32768);
 const n=dictionary.length, prefix=new Uint8Array(5+n);
 prefix.set([0,n&255,n>>8,(~n)&255,((~n)>>8)&255]);prefix.set(dictionary,5);
 const stream=new Blob([prefix,bytes.subarray(1)]).stream().pipeThrough(new DecompressionStream('deflate-raw'));
 const decoded=new Uint8Array(await new Response(stream).arrayBuffer());
 const message=JSON.parse(new TextDecoder('utf-8',{fatal:true}).decode(decoded.subarray(n)));
 if(tag===0||tag===1)previous.length=0;
 for(const [y,row] of message.rows)previous[y]=row;
 return message;
}
const systemTheme=matchMedia('(prefers-color-scheme: dark)');
systemTheme.addEventListener('change',()=>send({type:'theme',dark:systemTheme.matches}));
function connect(){
 if(closed)return;
 socket=new WebSocket(new URL('socket',location.href).href.replace(/^http/,'ws'));
 socket.binaryType='arraybuffer';
 const wireRows=[];let incoming=Promise.resolve();
 socket.onmessage=e=>{incoming=incoming.then(()=>receive(e)).catch(error=>{status.textContent=`Display error: ${error.message}`;socket.close();});};
 async function receive(e){
   let message;
   if(e.data instanceof ArrayBuffer){
     if(downloadName!==null){
       const url=URL.createObjectURL(new Blob([e.data],{type:'application/octet-stream'}));
       const link=document.createElement('a');link.href=url;link.download=downloadName;document.body.append(link);link.click();link.remove();
       setTimeout(()=>URL.revokeObjectURL(url),60000);downloadName=null;
       return;
     }
     message=await decodeFrame(new Uint8Array(e.data),wireRows);
   }else message=JSON.parse(e.data);
   if(message.type==='remote'){remoteHost=message.host;sessionFrontend=true;
   }else if(message.type==='connection'){
     ready=message.connected&&glyphs.size>0;status.textContent=message.message|| (ready?'Connected':'Reconnecting…');
   }else if(message.type==='assets'){
     glyphs=new Map(message.glyphs.map(([c,w,rs])=>[c,[w,rs]]));tiles.clear();scale=initialScale=message.scale||2;ready=true;status.textContent='Connected';lastSize='';send({type:'theme',dark:systemTheme.matches});
   }else if(message.type==='frame'){
     message.rows=message.rows.map(([y,spans])=>[y,spans.map(([x,fg,bg,runs])=>[x,fg,bg,runs.flatMap(run=>typeof run==='string'?Array.from(run,c=>[c,1]):[run])])]);
     const oldCursor=JSON.stringify(frame?.cursor);
     const changedMode=Object.hasOwn(message,'mode')&&mode!==message.mode;
     frame={...frame,...message};updateTitle();unsaved=frame.dirty;guardLeave();[cols,lines]=frame.size;mode=frame.mode||3;clipboard=frame.selection;
     if(message.reset){rows=Array(lines).fill(null);tiles.clear();}
     for(const [y,r] of message.rows)rows[y]=r;
     if(oldCursor!==JSON.stringify(frame.cursor))cursorEpoch=performance.now();
     allocate();drawRows(message.rows);
     if(changedMode)lastSize='';resize();
   }else if(message.type==='download'){
     downloadName=message.name;
   }else if(message.type==='copy'){
     if(nativeCopies.shift()!==message.text)systemClipboard(message);
   }else if(message.type==='paste-request'){
     systemClipboard(message);
   }else if(message.type==='ack'){
     acknowledged=Math.max(acknowledged,message.seq);if(Object.hasOwn(message,"dirty"))unsaved=message.dirty;guardLeave();
   }else if(message.type==='detached'){
     detached=true;closed=true;ready=false;guardLeave();fullscreen.disabled=true;
     status.textContent='Session detached. Resume from your terminal with thc-edit --resume.';
     navigator.keyboard?.unlock?.();socket.close();
   }else if(message.type==='closed'){
     closed=true;ready=false;guardLeave();fullscreen.disabled=true;
     status.textContent='Editor closed. You can close this tab.';
     navigator.keyboard?.unlock?.();socket.close();window.close();
   }
 };
 socket.onclose=event=>{console.info('Editor connection closed',event.code,event.reason);ready=false;mouse=[-1,-1];dirty=true;if(!closed){detaching=false;status.textContent='Disconnected — reconnecting…';setTimeout(connect,1000);}};
 socket.onerror=()=>{status.textContent='Connection unavailable';};
}
connect();
function point(e){const r=canvas.getBoundingClientRect();return [Math.floor((e.clientX-r.left)*cols/r.width),Math.floor((e.clientY-r.top)*lines/r.height)];}
function mouseEvent(action,e,extra={}){const [px,py]=point(e),x=Math.max(-1,Math.min(511,px)),y=Math.max(-1,Math.min(255,py));send({type:'mouse',action,x,y,button:e.button===2?2:0,clicks:Math.min(3,e.detail||1),mods:mods(e),...extra});}
canvas.addEventListener('pointerdown',e=>{e.preventDefault();input.focus({preventScroll:true});canvas.setPointerCapture(e.pointerId);mouseEvent('down',e);});
canvas.addEventListener('pointerup',e=>{mouseEvent('up',e);if(canvas.hasPointerCapture(e.pointerId))canvas.releasePointerCapture(e.pointerId);});
canvas.addEventListener('pointermove',e=>{const p=point(e);if(p[0]!==mouse[0]||p[1]!==mouse[1]){mouse=p;dirty=true;mouseEvent('move',e,{clicks:0});}});
canvas.addEventListener('pointerleave',()=>{mouse=[-1,-1];dirty=true;});
canvas.addEventListener('dblclick',e=>mouseEvent('down',e,{clicks:2}));
canvas.addEventListener('contextmenu',e=>e.preventDefault());
canvas.addEventListener('wheel',e=>{e.preventDefault();mouseEvent(e.deltaY<0?'wheel-up':'wheel-down',e);},{passive:false});
function release(){send({type:'blur'});mouse=[-1,-1];dirty=true;}
window.addEventListener('blur',release);document.addEventListener('visibilitychange',()=>{if(document.hidden)release();});
window.addEventListener('resize',resize);new ResizeObserver(resize).observe(screen);
window.addEventListener('keydown',e=>{
 if(sessionFrontend&&e.key===']'&&e.ctrlKey&&!e.metaKey&&!e.altKey&&!e.shiftKey){
   e.preventDefault();
   if(!closed&&!detaching&&socket?.readyState===WebSocket.OPEN){
     socket.send(JSON.stringify({type:'detach'}));detaching=true;ready=false;
     status.textContent='Detaching session…';
   }
   return;
 }
 if(e.target instanceof HTMLButtonElement)return;
 send({type:'modifiers',mods:mods(e)});
 if(composing||e.isComposing||e.key==='Process'||e.key==='Dead')return;
 if(['Control','Shift','Alt','Meta','CapsLock'].includes(e.key))return;
 const control=e.ctrlKey||e.metaKey;
 if(frame?.terminal&&e.ctrlKey&&!e.metaKey&&!e.shiftKey&&!e.altKey){
   e.preventDefault();send({type:'key',key:e.key,mods:mods(e)});return;
 }
 // Option+F produces ƒ on macOS; recognize the physical key for Replace.
 if(e.metaKey&&e.altKey&&e.code==='KeyF'){e.preventDefault();command('replace');return;}
 // Keep native Hide/Reload shortcuts, and never consume plain PTY control keys above.
 if(e.metaKey&&e.key.toLowerCase()==='h'||control&&!e.altKey&&e.key.toLowerCase()==='r')return;
 if(control&&e.shiftKey&&!e.altKey&&(!frame?.terminal||e.metaKey)&&['c','n'].includes(e.key.toLowerCase())){
   e.preventDefault();command(e.key.toLowerCase()==='c'?'conversation':'newConversation');return;
 }
 if(control&&['f','h','g','a','z','y'].includes(e.key.toLowerCase())&&(!frame?.wordstar||e.metaKey)){
   e.preventDefault();const k=e.key.toLowerCase();
   command(k==='h'||k==='f'&&e.altKey?'replace':k==='f'?'find':k==='g'?(e.shiftKey?'findPrevious':'findNext'):k==='a'?'selectAll':k==='y'||e.shiftKey?'redo':'undo');return;
 }
 if((control||e.altKey)&&['+','=','-','0'].includes(e.key)){
   e.preventDefault();scale=e.key==='0'?initialScale:Math.max(1,Math.min(8,scale+(e.key==='-'?-0.125:0.125)));tiles.clear();resize();return;
 }
 // Native clipboard events retain browser permission/user-activation semantics.
 if(control&&['c','x','v'].includes(e.key.toLowerCase())&&(!frame?.wordstar||e.metaKey))return;
 if(e.getModifierState('AltGraph'))return;
 if(e.altKey&&!control&&navigator.platform.includes('Mac')&&e.key.length===1&&!/^[a-z0-9]$/i.test(e.key))return;
 e.preventDefault();cursorEpoch=performance.now();send({type:'key',key:e.key,mods:mods(e)});
});
window.addEventListener('keyup',e=>send({type:'modifiers',mods:mods(e)}));
input.addEventListener('compositionstart',()=>{composing=true;});
input.addEventListener('compositionend',e=>{composing=false;if(e.data)send({type:'paste',text:e.data});input.value='';});
input.addEventListener('input',e=>{if(!composing){const text=input.value.replace(/^\u200b/,'');if(text)send({type:'paste',text});input.value='\u200b';input.setSelectionRange(1,1);}});
window.addEventListener('paste',e=>{if(e.target===fullscreen)return;e.preventDefault();send({type:'paste',text:e.clipboardData.getData('text/plain')});});
window.addEventListener('copy',e=>{e.preventDefault();e.clipboardData.setData('text/plain',clipboard);nativeCopies.push(clipboard);command('copy');});
window.addEventListener('cut',e=>{e.preventDefault();e.clipboardData.setData('text/plain',clipboard);nativeCopies.push(clipboard);command('cut');});
input.addEventListener('beforeinput',e=>{
 const name={historyUndo:'undo',historyRedo:'redo'}[e.inputType];
 if(name){e.preventDefault();command(name);}
});
// A nonempty selection target lets browser Edit > Select All reach the canvas editor.
input.addEventListener('select',()=>{
 if(input.value==='\u200b'&&input.selectionStart===0&&input.selectionEnd===1){command('selectAll');input.setSelectionRange(1,1);}
});
input.addEventListener('focus',()=>{if(!input.value){input.value='\u200b';input.setSelectionRange(1,1);}});
window.addEventListener('dragover',e=>{if([...e.dataTransfer.types].includes('Files')){e.preventDefault();e.dataTransfer.dropEffect='copy';}});
window.addEventListener('drop',async e=>{
 e.preventDefault();const files=[...e.dataTransfer.files];
 for(const file of files){
   if(file.size>16*1024*1024){status.textContent=`${file.name}: browser drops are limited to 16 MiB per file.`;continue;}
   try{
     const bytes=await file.arrayBuffer();
     if(!ready){status.textContent='Reconnect before dropping files.';break;}
     send({type:'upload',name:file.name});socket.send(bytes);
   }catch(error){status.textContent=`Cannot open ${file.name}: ${error.message}`;}
 }
 input.focus({preventScroll:true});
});
fullscreen.addEventListener('click',async()=>{
 try{
   if(document.fullscreenElement){await document.exitFullscreen();return;}
   await document.documentElement.requestFullscreen();
   if(navigator.keyboard?.lock){await navigator.keyboard.lock();status.textContent='Keyboard captured — hold Esc to leave fullscreen';}
   else status.textContent='Fullscreen — this browser cannot capture reserved shortcuts';
 }catch(error){status.textContent=`Keyboard capture unavailable: ${error.message}`;}
 input.focus({preventScroll:true});
});
document.addEventListener('fullscreenchange',()=>{if(!document.fullscreenElement){navigator.keyboard?.unlock?.();status.textContent=detached?'Session detached. Resume with thc-edit --resume.':closed?'Editor closed. You can close this tab.':ready?'Connected':'Disconnected';}resize();});
canvas.addEventListener('webglcontextlost',e=>{e.preventDefault();status.textContent='WebGL context lost — reload to reconnect; buffers stay in the editor.';});
input.focus({preventScroll:true});
