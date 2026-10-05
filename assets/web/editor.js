'use strict';
const canvas = document.querySelector('#display');
const screen = document.querySelector('#screen');
const status = document.querySelector('#status');
const input = document.querySelector('#input');
const fullscreen = document.querySelector('#fullscreen');
const clipboardAction = document.querySelector('#clipboard-action');
const resourceAction = document.querySelector('#open-resource');
const gl = canvas.getContext('webgl2', {alpha:false, antialias:false, preserveDrawingBuffer:true});
if (!gl) {status.textContent='WebGL2 is unavailable in this browser.';throw new Error(status.textContent);}
const vertex = `#version 300 es
in vec2 position; out highp vec2 vertexUV;
void main(){vertexUV=vec2((position.x+1.0)*0.5,(1.0-position.y)*0.5);gl_Position=vec4(position,0,1);}`;
const fragment = hideCellFragment;
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
function nearestTexture(){
 const texture=gl.createTexture();gl.bindTexture(gl.TEXTURE_2D,texture);
 for(const [parameter,value] of [[gl.TEXTURE_MIN_FILTER,gl.NEAREST],[gl.TEXTURE_MAG_FILTER,gl.NEAREST],[gl.TEXTURE_WRAP_S,gl.CLAMP_TO_EDGE],[gl.TEXTURE_WRAP_T,gl.CLAMP_TO_EDGE]])gl.texParameteri(gl.TEXTURE_2D,parameter,value);
 return texture;
}
let atlasTexture=nearestTexture(),atlasSize=2048,atlasX=1,atlasY=0,atlasRow=0,atlasEntries=new Map();
gl.texStorage2D(gl.TEXTURE_2D,1,gl.RGBA8,atlasSize,atlasSize);
const cellTexture=nearestTexture(),displayUniforms=gl.createBuffer();
gl.bindBuffer(gl.UNIFORM_BUFFER,displayUniforms);gl.bufferData(gl.UNIFORM_BUFFER,48,gl.DYNAMIC_DRAW);gl.bindBufferBase(gl.UNIFORM_BUFFER,0,displayUniforms);
gl.uniformBlockBinding(program,gl.getUniformBlockIndex(program,'type_Display'),0);
gl.uniform1i(gl.getUniformLocation(program,'SPIRV_Cross_CombinedglyphAtlasglyphSampler'),0);
gl.uniform1i(gl.getUniformLocation(program,'SPIRV_Cross_CombinedcellDataSPIRV_Cross_DummySampler'),1);
let cellGrid=new Uint32Array(),gridCols=0,gridRows=0;
const atlasStats={tiles:0,tileBytes:0,gridBytes:0,draws:0};
let glyphs=new Map(), tiles=new Map(), rows=[], frame=null, scale=2, initialScale=2, cols=80, lines=25, mode=3;
let socket, ready=false, closed=false, mouse=[-1,-1], leftDown=false, cursorEpoch=performance.now(), blinkPhase=-1, dirty=true, composing=false, clipboard='', lastSize='';
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
function mods(e){return [e.shiftKey?'shift':null,e.ctrlKey?'ctrl':null,e.metaKey?'cmd':null,e.altKey&&!e.getModifierState?.('AltGraph')?'alt':null].filter(Boolean);}
function cellHeight(){return mode===259?8:16;}
function metrics(){return [canvas.width/cols,canvas.height/lines];}
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
   canvas.width=width;canvas.height=height;
   gl.viewport(0,0,width,height);drawRows(rows.map((r,y)=>[y,r]));dirty=true;return true;
 }
 dirty=true;return false;
}
function bitmapInk(bitmap,y,x,traits){
 const source=x-((traits&2)?Math.floor((15-y)/4):0),row=bitmap[1][y];
 return (source>=0&&source<bitmap[0]&&!!(row&(1<<(15-source))))||
   ((traits&1)&&source>0&&source<=bitmap[0]&&!!(row&(1<<(16-source))));
}
function tile(text,fg,pixelated,w,h,traits){
 traits&=7; // Line decorations are cell paint, not shaped glyph identity.
 const bitmap=glyphs.get(text), key=JSON.stringify([text,fg,pixelated,w,h,traits]);
 if(tiles.has(key))return tiles.get(key);
 const t=document.createElement('canvas');
 if(bitmap){
   t.width=bitmap[0];t.height=16;const c=t.getContext('2d'), image=c.createImageData(t.width,16);
   for(let y=0;y<16;y++)for(let x=0;x<t.width;x++)if(bitmapInk(bitmap,y,x,traits)){const i=(y*t.width+x)*4;image.data.set([fg>>16,(fg>>8)&255,fg&255,255],i);}
   c.putImageData(image,0,0);
 }else{
   // Shape an entire grapheme in one canvas operation, preserving emoji sequences.
   t.width=pixelated?Math.max(8,Math.round(w/(8*scale*(devicePixelRatio||1)))*8):Math.max(1,Math.ceil(w));
   t.height=pixelated?16:Math.max(16,Math.ceil(h*16/cellHeight()));
   const c=t.getContext('2d');c.fillStyle=rgb(fg);c.textBaseline='alphabetic';c.font=`${traits&2?'italic ':''}${traits&1?'bold ':''}${t.height*0.85}px monospace`;
   if(traits&4)c.scale(2,1);
   c.fillText(text,0,t.height*0.82,(traits&4)?t.width/2:t.width);
 }
 if(tiles.size>=1024)tiles.clear();tiles.set(key,t);return t;
}
function atlasEntry(text,fg,pixelated,w,h,traits){
 traits&=7;
 const bitmap=glyphs.has(text),paint=bitmap?0xffffff:fg;
 const key=JSON.stringify([text,paint,pixelated,w,h,bitmap?traits&3:traits]);
 if(atlasEntries.has(key))return atlasEntries.get(key);
 const image=tile(text,paint,pixelated,w,h,bitmap?traits&3:traits);
 const width=image.width,height=image.height;
 if(atlasX+width>atlasSize){atlasX=0;atlasY+=atlasRow;atlasRow=0;}
 while(atlasY+height>atlasSize||width>atlasSize){
   const size=Math.min(atlasSize*2,8192,gl.getParameter(gl.MAX_TEXTURE_SIZE));
   if(size<=atlasSize)throw new Error('Glyph atlas full');
   // Grow by GPU copy; existing grid records keep their pixel rectangles.
   const previous=atlasTexture,copy=gl.createFramebuffer();
   gl.bindFramebuffer(gl.FRAMEBUFFER,copy);gl.framebufferTexture2D(gl.FRAMEBUFFER,gl.COLOR_ATTACHMENT0,gl.TEXTURE_2D,previous,0);
   atlasTexture=nearestTexture();gl.texStorage2D(gl.TEXTURE_2D,1,gl.RGBA8,size,size);
   gl.copyTexSubImage2D(gl.TEXTURE_2D,0,0,0,0,0,atlasSize,atlasSize);
   gl.bindFramebuffer(gl.FRAMEBUFFER,null);gl.deleteFramebuffer(copy);gl.deleteTexture(previous);atlasSize=size;
 }
 gl.activeTexture(gl.TEXTURE0);gl.bindTexture(gl.TEXTURE_2D,atlasTexture);
 gl.texSubImage2D(gl.TEXTURE_2D,0,atlasX,atlasY,gl.RGBA,gl.UNSIGNED_BYTE,image);
 ++atlasStats.tiles;atlasStats.tileBytes+=width*height*4;
 const entry=[atlasX|(atlasY<<16),width|(height<<16),bitmap?2:0];
 atlasX+=width;atlasRow=Math.max(atlasRow,height);
 if(atlasEntries.size>=3072)atlasEntries.clear();
 atlasEntries.set(key,entry);return entry;
}
function drawRows(changed,retried=false){
 if(!frame)return;const started=performance.now(),[cw,ch]=metrics();
 if(gridCols!==cols||gridRows!==lines){
   gridCols=cols;gridRows=lines;cellGrid=new Uint32Array(cols*lines*8);
   gl.activeTexture(gl.TEXTURE1);gl.bindTexture(gl.TEXTURE_2D,cellTexture);
   gl.texImage2D(gl.TEXTURE_2D,0,gl.RGBA32UI,cols*2,lines,0,gl.RGBA_INTEGER,gl.UNSIGNED_INT,null);
   changed=rows.map((r,y)=>[y,r]);
 }
 try{
   const blank=atlasEntry(' ',0xffffff,frame.pixelated,cw,ch,0);
   for(const [y,spans] of changed){
     const start=y*cols*8;
     for(let x=0;x<cols;++x)cellGrid.set([blank[0],blank[1],1,0,0,0x0000aa,1|blank[2],1],start+x*8);
     for(const [origin,fg,bg,traits,clusters] of spans||[]){let x=origin;
       for(const [text,fullWidth,stretched,clipStart,clipWidth,script=0,natural=fullWidth] of clusters){
         const entry=atlasEntry(text,fg,frame.pixelated,natural*cw,ch,(traits&3)|(stretched?4:0));
         for(let cell=0;cell<clipWidth;++cell)if(x+cell>=0&&x+cell<cols)
           cellGrid.set([entry[0],entry[1],fullWidth|(script?(natural<<2)|(script<<4):0)|((clipStart+cell)<<16),0,fg,bg,1|entry[2]|(traits&24),1],start+(x+cell)*8);
         x+=clipWidth;
       }
     }
     gl.activeTexture(gl.TEXTURE1);gl.bindTexture(gl.TEXTURE_2D,cellTexture);
     const row=cellGrid.subarray(start,start+cols*8);
     gl.texSubImage2D(gl.TEXTURE_2D,0,0,y,cols*2,1,gl.RGBA_INTEGER,gl.UNSIGNED_INT,row);
     atlasStats.gridBytes+=row.byteLength;
   }
 }catch(error){
   if(error.message!=='Glyph atlas full'||retried)throw error;
   // Retire old rectangles only before a complete replay, never midway through
   // an adopted grid. A single frame larger than the bound fails explicitly.
   atlasEntries.clear();atlasX=1;atlasY=atlasRow=0;
   return drawRows(rows.map((r,y)=>[y,r]),true);

 }
 rasterTime+=performance.now()-started;dirty=true;
}
function present(now){
 const phase=!frame?.blink||Math.floor((now-cursorEpoch)/500)%2===0;
 if(frame&&(dirty||phase!==blinkPhase)){
   const started=performance.now();
   const caret=phase&&frame.cursor?frame.cursor:[-1,-1],pointer=leftDown?[-1,-1]:mouse;
   gl.bindBuffer(gl.UNIFORM_BUFFER,displayUniforms);
   gl.bufferSubData(gl.UNIFORM_BUFFER,0,new Float32Array([cols,lines,atlasSize,0,...caret,...pointer,canvas.width,canvas.height,frame.crt?1:0,canvas.height/(lines*16)]));
   gl.activeTexture(gl.TEXTURE0);gl.bindTexture(gl.TEXTURE_2D,atlasTexture);
   gl.activeTexture(gl.TEXTURE1);gl.bindTexture(gl.TEXTURE_2D,cellTexture);
   gl.drawArrays(gl.TRIANGLE_STRIP,0,4);++atlasStats.draws;dirty=false;blinkPhase=phase;
   drawTimes.push(rasterTime+performance.now()-started);rasterTime=0;if(drawTimes.length>60)drawTimes.shift();
 }
 if(now-titleTick>=1000&&drawTimes.length){
   timingText=` | ${(drawTimes.reduce((a,b)=>a+b,0)/drawTimes.length).toFixed(1)} ms/frame`;
   titleTick=now;updateTitle();
 }
 requestAnimationFrame(present);
}
requestAnimationFrame(present);
// Script runs retain semantic text and natural atlas size while occupying one
// cell. The host supplies Unicode width; the browser never measures font advance.
const scriptSegments=new Intl.Segmenter(undefined,{granularity:'grapheme'});
function decodeRows(changed){
 return changed.map(([y,spans])=>[y,spans.map(span=>{
   if(span.length!==5||!Number.isInteger(span[3])||span[3]<0||(span[3]&27)!==span[3])throw new Error('Invalid font traits');
   const [x,fg,bg,traits,runs]=span;
   return [x,fg,bg,traits,runs.flatMap(run=>{
     if(typeof run==='string')return Array.from(run,c=>[c,1,false,0,1]);
     if(Array.isArray(run)&&run.length===3){
       const [text,natural,mode]=run;
       if(typeof text!=='string'||!text.length||text.length>8192||/[\u0000-\u001f\u007f]/u.test(text)||
         ![1,2].includes(natural)||!['sup','sub'].includes(mode)||
         scriptSegments.segment(text)[Symbol.iterator]().next().value.segment!==text)throw new Error('Invalid script glyph');
       return [[text,1,false,0,1,mode==='sup'?1:2,natural]];
     }
     if(!Array.isArray(run)||run.length!==5||typeof run[0]!=='string'||!Number.isInteger(run[1])||run[1]<1||run[1]>2||typeof run[2]!=='boolean'||!Number.isInteger(run[3])||!Number.isInteger(run[4])||run[3]<0||run[4]<1||run[3]+run[4]>run[1])throw new Error('Invalid glyph geometry');
     return [run];
   })];
 })]);
}
function command(name){send({type:'command',command:name});}
async function systemClipboard(request){
 try{
   if(request.type==='copy')await navigator.clipboard.writeText(request.text);
   else send({type:'paste',text:await navigator.clipboard.readText()});
   clipboardRequest=null;clipboardAction.hidden=true;input.focus({preventScroll:true});
 }catch(error){
   clipboardRequest=request;clipboardAction.textContent=request.type==='copy'?'Copy to clipboard':'Paste from clipboard';clipboardAction.hidden=false;
   status.textContent='Clipboard access needs a click, or use the browser Edit menu.';
 }
}
clipboardAction.addEventListener('click',()=>{if(clipboardRequest)systemClipboard(clipboardRequest);});
// WebSocket replies may arrive after browser user activation expires. Keep an
// explicit button available when opening a new tab needs another human click.
let pendingResource=null,pendingResourceMime=null;
function receiveResource(message){
  let url;
  try{
    if(message.url){
      const parsed=new URL(message.url);
      if(!['http:','https:'].includes(parsed.protocol))throw new Error('Unsupported link');
      url=parsed.href;
    }else{
      if(!['image/png','image/jpeg','image/gif','image/webp','image/bmp','image/svg+xml','application/pdf'].includes(message.mime))throw new Error('Unsupported file');
      if(typeof message.data!=='string'||message.data.length>11184812)throw new Error('File too large');
      const bytes=Uint8Array.from(atob(message.data),c=>c.charCodeAt(0));
      if(bytes.length>8388608)throw new Error('File too large');
      url=URL.createObjectURL(new Blob([bytes],{type:message.mime}));
    }
  }catch(error){status.textContent='Cannot open link: '+error.message;return;}
  if(pendingResource?.startsWith('blob:'))URL.revokeObjectURL(pendingResource);
  pendingResource=url;pendingResourceMime=message.mime||null;
  openResourceTab();
}
function openResourceTab(){
  if(!pendingResource)return;
  const tab=window.open('about:blank','_blank');
  if(!tab){resourceAction.hidden=false;status.textContent='Click Open link to open this resource in a new tab.';return;}
  const url=pendingResource;
  tab.opener=null;
  if(url.startsWith('blob:')&&pendingResourceMime?.startsWith('image/')){
    // Embed images rather than navigating to SVG as an active same-origin document.
    const img=tab.document.createElement('img');img.src=url;img.style.maxWidth='100%';
    tab.document.title='Image';tab.document.body.appendChild(img);
  }else tab.location.replace(url);
  pendingResource=null;resourceAction.hidden=true;
  if(url.startsWith('blob:'))setTimeout(()=>URL.revokeObjectURL(url),60000);
}
resourceAction.addEventListener('click',openResourceTab);

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
 const wireRows=[];let incoming=Promise.resolve(),platformSent=false;
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
     glyphs=new Map(message.glyphs.map(([c,w,rs])=>[c,[w,rs]]));tiles.clear();atlasEntries.clear();scale=initialScale=message.scale||2;ready=true;status.textContent='Connected';lastSize='';send({type:'theme',dark:systemTheme.matches});
   }else if(message.type==='frame'){
     if(!platformSent){platformSent=true;send({type:'frontend',mode:message.mode||3,mac:navigator.platform.includes('Mac')});}
     message.rows=decodeRows(message.rows);
     const oldCursor=JSON.stringify(frame?.cursor);
     const changedMode=Object.hasOwn(message,'mode')&&mode!==message.mode;
     frame={...frame,...message};updateTitle();unsaved=frame.dirty;guardLeave();[cols,lines]=frame.size;mode=frame.mode||3;clipboard=frame.selection;
     if(message.reset){rows=Array(lines).fill(null);}
     for(const [y,r] of message.rows)rows[y]=r;
     if(oldCursor!==JSON.stringify(frame.cursor))cursorEpoch=performance.now();
     if(!allocate())drawRows(message.rows);
     if(changedMode)lastSize='';resize();
   }else if(message.type==='download'){
     downloadName=message.name;
   }else if(message.type==='open-resource'){
     receiveResource(message);
   }else if(message.type==='copy'){
     if(nativeCopies.shift()!==message.text)systemClipboard(message);
   }else if(message.type==='paste-request'){
     systemClipboard(message);
   }else if(message.type==='ack'){
     acknowledged=Math.max(acknowledged,message.seq);if(Object.hasOwn(message,"dirty"))unsaved=message.dirty;guardLeave();
   }else if(message.type==='detached'){
     detached=true;closed=true;ready=false;guardLeave();fullscreen.disabled=true;
     status.textContent='Session detached. Resume from your terminal with hide --resume.';
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
canvas.addEventListener('pointerdown',e=>{e.preventDefault();if(e.button===0){leftDown=true;dirty=true;}input.focus({preventScroll:true});canvas.setPointerCapture(e.pointerId);mouseEvent('down',e);});
canvas.addEventListener('pointerup',e=>{if(e.button===0){leftDown=false;dirty=true;}mouseEvent('up',e);if(canvas.hasPointerCapture(e.pointerId))canvas.releasePointerCapture(e.pointerId);});
canvas.addEventListener('pointermove',e=>{const p=point(e);if(p[0]!==mouse[0]||p[1]!==mouse[1]){mouse=p;dirty=true;mouseEvent('move',e,{clicks:0});}});
canvas.addEventListener('pointercancel',release);
canvas.addEventListener('pointerleave',()=>{mouse=[-1,-1];dirty=true;});
canvas.addEventListener('dblclick',e=>mouseEvent('down',e,{clicks:2}));
canvas.addEventListener('contextmenu',e=>e.preventDefault());
canvas.addEventListener('wheel',e=>{e.preventDefault();mouseEvent(e.deltaY<0?'wheel-up':'wheel-down',e);},{passive:false});
function release(){send({type:'blur'});leftDown=false;mouse=[-1,-1];dirty=true;}
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
 if(frame?.terminal&&e.ctrlKey&&!e.metaKey&&!e.altKey){
   e.preventDefault();send({type:'key',key:e.key,mods:mods(e)});return;
 }
 // Browser and OS reservations remain outside editor authority after PTY input.
 if(e.metaKey&&e.key.toLowerCase()==='h'||control&&!e.altKey&&e.key.toLowerCase()==='r')return;
 if((e.ctrlKey||e.altKey&&!e.metaKey)&&['+','=','-','0'].includes(e.key)){
   e.preventDefault();scale=e.key==='0'?initialScale:Math.max(1,Math.min(8,scale+(e.key==='-'?-0.125:0.125)));tiles.clear();resize();return;
 }
 if(e.getModifierState('AltGraph'))return;
 // Option is text input; Command+Option chords use the unmodified key label.
 if(e.altKey&&!control&&navigator.platform.includes('Mac')&&e.key.length===1)return;
 const key=e.metaKey&&e.altKey&&/^Key[A-Z]$/.test(e.code)?e.code.slice(3).toLowerCase():e.key;
 const names={ArrowUp:'Up',ArrowDown:'Down',ArrowLeft:'Left',ArrowRight:'Right',' ':'Space',Esc:'Escape'};
 const chord=[e.ctrlKey?'Ctrl':null,e.metaKey?'Cmd':null,e.altKey?'Alt':null,e.shiftKey?'Shift':null,names[key]|| (key.length===1?key.toUpperCase():key)].filter(Boolean).join('+');
 const action=frame?.bindings?.find(([candidate])=>candidate===chord)?.[1];
 // An inert contributed owner consumes its chord across publication/map changes.
 if(action===''){e.preventDefault();return;}
 // Only a matching default clipboard action may delegate its keyboard gesture
 // to the browser clipboard event. Unbinding/remapping suppresses that default.
 const nativeClipboard={c:'hide.edit.copy',x:'hide.edit.cut',v:'hide.edit.paste'}[key.toLowerCase()];
 if(control&&!e.altKey&&!e.shiftKey&&nativeClipboard&&action===nativeClipboard)return;
 // Modal editing keeps its platform clipboard shortcuts and authority checks.
 if(control&&!e.altKey&&!e.shiftKey&&!frame?.bindingsActive&&nativeClipboard){e.preventDefault();command(nativeClipboard);return;}
 e.preventDefault();cursorEpoch=performance.now();
 const contribution=action&&frame?.menuContributions?.find(item=>item.id===action&&item.key);
 if(contribution)send({type:'menu',command:contribution.id,registry:contribution.registry,generation:contribution.generation});
 else send({type:'key',key,mods:mods(e)});
 return;
});
window.addEventListener('keyup',e=>send({type:'modifiers',mods:mods(e)}));
input.addEventListener('compositionstart',()=>{composing=true;});
input.addEventListener('compositionend',e=>{composing=false;if(e.data)send({type:'paste',text:e.data});input.value='';});
input.addEventListener('input',e=>{if(!composing){const text=input.value.replace(/^\u200b/,'');if(text)send({type:'paste',text});input.value='\u200b';input.setSelectionRange(1,1);}});
window.addEventListener('paste',e=>{if(e.target===fullscreen)return;e.preventDefault();send({type:'paste',text:e.clipboardData.getData('text/plain')});});
window.addEventListener('copy',e=>{e.preventDefault();e.clipboardData.setData('text/plain',clipboard);nativeCopies.push(clipboard);command('hide.edit.copy');});
window.addEventListener('cut',e=>{e.preventDefault();e.clipboardData.setData('text/plain',clipboard);nativeCopies.push(clipboard);command('hide.edit.cut');});
input.addEventListener('beforeinput',e=>{
 const name={historyUndo:'hide.edit.undo',historyRedo:'hide.edit.redo'}[e.inputType];
 if(name){e.preventDefault();command(name);}
});
// A nonempty selection target lets browser Edit > Select All reach the canvas editor.
input.addEventListener('select',()=>{
 if(input.value==='\u200b'&&input.selectionStart===0&&input.selectionEnd===1){command('hide.edit.select-all');input.setSelectionRange(1,1);}
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
document.addEventListener('fullscreenchange',()=>{if(!document.fullscreenElement){navigator.keyboard?.unlock?.();status.textContent=detached?'Session detached. Resume with hide --resume.':closed?'Editor closed. You can close this tab.':ready?'Connected':'Disconnected';}resize();});
canvas.addEventListener('webglcontextlost',e=>{e.preventDefault();status.textContent='WebGL context lost — reload to reconnect; buffers stay in the editor.';});
input.focus({preventScroll:true});
