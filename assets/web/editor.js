'use strict';
const canvas = document.querySelector('#display');
const screen = document.querySelector('#screen');
const status = document.querySelector('#status');
const input = document.querySelector('#input');
const fullscreen = document.querySelector('#fullscreen');
const clipboardAction = document.querySelector('#clipboard-action');
const resourceAction = document.querySelector('#open-resource');
const downloadAction = document.querySelector('#download-file');
const sidebarAccess = document.querySelector('#semantic-sidebar');
const dialogAccess = document.querySelector('#semantic-dialog');
const sourceAccess = document.querySelector('#semantic-source');
const sourceText = document.querySelector('#semantic-source-text');
const imageAccess = document.querySelector('#semantic-images');
const gl = canvas.getContext('webgl2', {alpha:false, antialias:false, preserveDrawingBuffer:true});
if (!gl) {status.textContent='WebGL2 is unavailable in this browser.';throw new Error(status.textContent);}
const images = new CanvasImages(gl,imageAccess);
let contextLost=false;
const vertex = `#version 300 es
in vec2 position; out highp vec2 vertexUV;
void main(){vertexUV=vec2((position.x+1.0)*0.5,(1.0-position.y)*0.5);gl_Position=vec4(position,0,1);}`;
const fragment = hideCellFragment;
function shader(type, source) {
 const s=gl.createShader(type); gl.shaderSource(s,source); gl.compileShader(s);
 if(!gl.getShaderParameter(s,gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(s));
 return s;
}
function makeProgram(fragment){
 const program=gl.createProgram(),vs=shader(gl.VERTEX_SHADER,vertex),fs=shader(gl.FRAGMENT_SHADER,fragment);
 gl.attachShader(program,vs);gl.attachShader(program,fs);gl.linkProgram(program);gl.deleteShader(vs);gl.deleteShader(fs);
 if(!gl.getProgramParameter(program,gl.LINK_STATUS))throw new Error(gl.getProgramInfoLog(program));return program;
}
let program,vertices,cellVAO,atlasTexture,cellTexture,displayUniforms;
function initializeGPU(){
 program=makeProgram(fragment);gl.useProgram(program);
 vertices=gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER,vertices);gl.bufferData(gl.ARRAY_BUFFER,new Float32Array([-1,-1,1,-1,-1,1,1,1]),gl.STATIC_DRAW);
 cellVAO=gl.createVertexArray();gl.bindVertexArray(cellVAO);
 const pos=gl.getAttribLocation(program,'position');gl.enableVertexAttribArray(pos);gl.vertexAttribPointer(pos,2,gl.FLOAT,false,0,0);
 gl.activeTexture(gl.TEXTURE0);atlasTexture=nearestTexture();gl.texStorage2D(gl.TEXTURE_2D,1,gl.RGBA8,atlasSize,atlasSize);
 cellTexture=nearestTexture();displayUniforms=gl.createBuffer();
 gl.bindBuffer(gl.UNIFORM_BUFFER,displayUniforms);gl.bufferData(gl.UNIFORM_BUFFER,48,gl.DYNAMIC_DRAW);gl.bindBufferBase(gl.UNIFORM_BUFFER,0,displayUniforms);
 gl.uniformBlockBinding(program,gl.getUniformBlockIndex(program,'type_Display'),0);
 gl.uniform1i(gl.getUniformLocation(program,'SPIRV_Cross_CombinedglyphAtlasglyphSampler'),0);
 gl.uniform1i(gl.getUniformLocation(program,'SPIRV_Cross_CombinedcellDataSPIRV_Cross_DummySampler'),1);
 images.restore(makeProgram,vertices);
}
function nearestTexture(){
 const texture=gl.createTexture();gl.bindTexture(gl.TEXTURE_2D,texture);
 for(const [parameter,value] of [[gl.TEXTURE_MIN_FILTER,gl.NEAREST],[gl.TEXTURE_MAG_FILTER,gl.NEAREST],[gl.TEXTURE_WRAP_S,gl.CLAMP_TO_EDGE],[gl.TEXTURE_WRAP_T,gl.CLAMP_TO_EDGE]])gl.texParameteri(gl.TEXTURE_2D,parameter,value);
 return texture;
}
let atlasSize=2048,atlasX=1,atlasY=0,atlasRow=0,atlasEntries=new Map();
initializeGPU();
let cellGrid=new Uint32Array(),gridCols=0,gridRows=0;
const atlasStats={tiles:0,tileBytes:0,gridBytes:0,draws:0};
let glyphs=new Map(), tiles=new Map(), rows=[], frame=null, scale=2, initialScale=2, cols=80, lines=25, mode=3;
let socket, attachmentEpoch=0, ready=false, closed=false, mouse=[-1,-1], leftDown=false, cursorEpoch=performance.now(), blinkPhase=-1, dirty=true, composing=false, clipboard='', lastSize='';
let remoteHost="", sessionFrontend=false, detaching=false, detached=false;
const drawTimes=[];
let titleTick=0, timingText='', rasterTime=0;
function updateTitle(){
 if(!frame)return;
 const base=remoteHost?frame.title.replace(/^th(?: |$)/,`th ${remoteHost}:`):frame.title;
 document.title=base+timingText;
}
let downloadInfo=null, clipboardRequest=null, clipboardEpoch=0, nativeCopies=[];
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
 if(!frame||contextLost)return;const started=performance.now(),[cw,ch]=metrics();
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
 const phase=!frame?.cursor||!frame.blink||Math.floor((now-cursorEpoch)/500)%2===0;
 if(frame&&!contextLost&&(dirty||phase!==blinkPhase)){
   gl.useProgram(program);gl.bindVertexArray(cellVAO);
   const started=performance.now();
   const caret=phase&&frame.cursor?frame.cursor:[-1,-1],pointer=leftDown?[-1,-1]:mouse;
   gl.bindBuffer(gl.UNIFORM_BUFFER,displayUniforms);
   gl.bufferSubData(gl.UNIFORM_BUFFER,0,new Float32Array([cols,lines,atlasSize,0,...caret,...pointer,canvas.width,canvas.height,frame.crt?1:0,canvas.height/(lines*16)]));
   gl.activeTexture(gl.TEXTURE0);gl.bindTexture(gl.TEXTURE_2D,atlasTexture);
   gl.activeTexture(gl.TEXTURE1);gl.bindTexture(gl.TEXTURE_2D,cellTexture);
   gl.drawArrays(gl.TRIANGLE_STRIP,0,4);images.draw();++atlasStats.draws;dirty=false;blinkPhase=phase;
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
function boundedGlyphText(text){
 if(typeof text!=='string'||!text.length||text.length>64)return false;
 let scalars=0,bytes=0;
 for(const char of text){
   const point=char.codePointAt(0);
   if(point<32||point===127||(point>=0xd800&&point<=0xdfff))return false;
   bytes+=point<0x80?1:point<0x800?2:point<0x10000?3:4;
   if(++scalars>32||bytes>128)return false;
 }
 return true;
}
function decodeRows(changed){
 return changed.map(([y,spans])=>[y,spans.map(span=>{
   if(span.length!==5||!Number.isInteger(span[3])||span[3]<0||(span[3]&27)!==span[3])throw new Error('Invalid font traits');
   const [x,fg,bg,traits,runs]=span;
   return [x,fg,bg,traits,runs.flatMap(run=>{
     if(typeof run==='string')return Array.from(run,c=>[c,1,false,0,1]);
     if(Array.isArray(run)&&run.length===3){
       const [text,natural,mode]=run;
       if(!boundedGlyphText(text)||
         ![1,2].includes(natural)||!['sup','sub'].includes(mode)||
         scriptSegments.segment(text)[Symbol.iterator]().next().value.segment!==text)throw new Error('Invalid script glyph');
       return [[text,1,false,0,1,mode==='sup'?1:2,natural]];
     }
     if(!Array.isArray(run)||run.length!==5||!boundedGlyphText(run[0])||!Number.isInteger(run[1])||run[1]<1||run[1]>2||typeof run[2]!=='boolean'||!Number.isInteger(run[3])||!Number.isInteger(run[4])||run[3]<0||run[4]<1||run[3]+run[4]>run[1])throw new Error('Invalid glyph geometry');
     return [run];
   })];
 })]);
}
function command(name){send({type:'command',command:name});}
async function systemClipboard(request){
 const connection=socket,epoch=++clipboardEpoch;
 try{
   if(request.type==='copy')await navigator.clipboard.writeText(request.text);
   else {const text=await navigator.clipboard.readText();if(connection!==socket||epoch!==clipboardEpoch||!ready)return;send({type:'paste-reply',request:request.request,text});}
   if(connection!==socket||epoch!==clipboardEpoch||!ready)return;
   clipboardRequest=null;clipboardAction.hidden=true;input.focus({preventScroll:true});
 }catch(error){
   if(connection!==socket||epoch!==clipboardEpoch||!ready)return;
   clipboardRequest=request;clipboardAction.textContent=request.type==='copy'?'Copy to clipboard':'Paste from clipboard';clipboardAction.hidden=false;
   status.textContent='Clipboard access needs a click, or use the browser Edit menu.';
 }
}
function clearClipboardRequest(){clipboardEpoch++;clipboardRequest=null;clipboardAction.hidden=true;}
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
// Complete sidebar metadata describes the painted, policy-masked viewport.
// Local reading focus cannot select a host node or dispatch an editor action.
let sidebarReadingKey=null;
function clearSidebar(){
 sidebarAccess.replaceChildren();sidebarAccess.hidden=true;sidebarReadingKey=null;
 sidebarAccess.removeAttribute('aria-activedescendant');
}
function readSidebarItem(item){
 if(!item)return;
 sidebarReadingKey=item.dataset.sidebarKey;
 sidebarAccess.setAttribute('aria-activedescendant',item.id);
}
function receiveSidebar(value){
 const invalid=()=>{clearSidebar();throw new Error('Invalid sidebar metadata');};
 const integer=n=>Number.isSafeInteger(n)&&n>=0;
 const validId=id=>Array.isArray(id)&&(id.length===1&&id[0]==='sidebar'||id.length===3&&id[0]==='tree'&&
   typeof id[1]==='string'&&/^[0-9a-f]{48}$/.test(id[1])&&typeof id[2]==='string'&&Array.from(id[2]).length>0&&Array.from(id[2]).length<=128);
 if(!value||value.readOnly!==true||!integer(value.revision)||!Array.isArray(value.layout)||value.layout.length!==7||
   !value.layout.every(integer)||value.layout[0]!==cols||value.layout[1]!==lines||
   ![value.visibleStart,value.visibleCount,value.logicalRows].every(integer)||value.logicalRows>65536||
   !Array.isArray(value.nodes)||value.nodes.length>512)invalid();
 const nodes=new Map();
 for(const node of value.nodes){
   if(!node||!validId(node.id)||!['tree','treeitem'].includes(node.role)||typeof node.name!=='string'||Array.from(node.name).length>256||
     ![node.selected,node.focused,node.loading,node.moreChildren].every(v=>typeof v==='boolean')||
     !(node.expanded===null||typeof node.expanded==='boolean')||![node.generation,node.level,node.posInSet,node.childrenKnown].every(integer)||
     node.childrenKnown>32768||!(node.setSize===-1||integer(node.setSize)&&node.setSize<=32768)||
     !(node.index===null||integer(node.index)&&node.index<value.logicalRows))invalid();
   if(node.bounds!==null){
     const r=node.bounds;if(!Array.isArray(r)||r.length!==4||!r.every(integer)||r[2]<1||r[3]<1||r[0]+r[2]>cols||r[1]+r[3]>lines)invalid();
   }
   const key=JSON.stringify(node.id);
   if(nodes.has(key))invalid();nodes.set(key,node);
 }
 if(value.visibleCount===0||!value.nodes.some(node=>node.role==='treeitem')){clearSidebar();return;}
 const root=nodes.get('["sidebar"]');
 if(!root||root.role!=='tree'||root.parent!==null)invalid();
 const items=value.nodes.filter(node=>node.role==='treeitem').sort((a,b)=>a.index-b.index);
 for(const node of items){
   if(node.id[0]!=='tree'||node.level<1||node.posInSet<1||!validId(node.parent))invalid();
   let parent=nodes.get(JSON.stringify(node.parent)),level=1;
   while(parent&&parent!==root){
     if(parent.role!=='treeitem'||++level>65||!validId(parent.parent))invalid();
     parent=nodes.get(JSON.stringify(parent.parent));
   }
   if(!parent||node.level!==level)invalid();
 }
 if(value.nodes.length!==items.length+1)invalid();
 const elements=new Map(),groups=new Map(),fragment=document.createElement('div');
 groups.set('["sidebar"]',fragment);
 for(const [i,node] of items.entries()){
   const item=document.createElement('div'),label=document.createElement('span');
   item.id=`sidebar-item-${i}`;item.dataset.sidebarKey=JSON.stringify(node.id);item.setAttribute('role','treeitem');item.setAttribute('aria-label',node.name);
   for(const [name,val] of [['selected',node.selected],['busy',node.loading],['level',node.level],['posinset',node.posInSet],['setsize',node.setSize]])item.setAttribute(`aria-${name}`,val);
   if(node.expanded!==null)item.setAttribute('aria-expanded',node.expanded);
   item.setAttribute('aria-description',[node.focused?'Focused in editor':'',node.loading?'Loading children':'',node.moreChildren?'Children are not fully loaded':''].filter(Boolean).join('. '));
   label.textContent=node.name;item.append(label);elements.set(item.dataset.sidebarKey,item);
   if(node.expanded!==null){const group=document.createElement('div');group.setAttribute('role','group');item.append(group);groups.set(item.dataset.sidebarKey,group);}
 }
 for(const node of items){const parent=groups.get(JSON.stringify(node.parent));if(!parent)invalid();parent.append(elements.get(JSON.stringify(node.id)));}
 const retained=document.activeElement===sidebarAccess?elements.get(sidebarReadingKey):null;
 sidebarAccess.replaceChildren(...fragment.children);sidebarAccess.setAttribute('aria-label',root.name);sidebarAccess.hidden=false;
 readSidebarItem(retained||elements.get(JSON.stringify(items.find(node=>node.selected)?.id))||elements.values().next().value);
}
sidebarAccess.addEventListener('focus',()=>{
 const items=[...sidebarAccess.querySelectorAll('[role="treeitem"]')];
 readSidebarItem(items.find(item=>item.getAttribute('aria-selected')==='true')||items[0]);
 status.textContent='Sidebar reading focus — arrows browse; Tab leaves.';
});
sidebarAccess.addEventListener('keydown',event=>{
 if(event.ctrlKey||event.metaKey||event.altKey)return;
 const items=[...sidebarAccess.querySelectorAll('[role="treeitem"]')],index=items.findIndex(item=>item.id===sidebarAccess.getAttribute('aria-activedescendant'));
 const current=items[index];let next;
 switch(event.key){
   case 'ArrowDown':next=items[Math.min(items.length-1,index+1)];break;
   case 'ArrowUp':next=items[Math.max(0,index-1)];break;
   case 'Home':next=items[0];break;
   case 'End':next=items.at(-1);break;
   case 'ArrowRight':next=current?.children[1]?.children[0];break;
   case 'ArrowLeft':next=current?.parentElement?.parentElement;if(next?.getAttribute('role')!=='treeitem')next=null;break;
   default:return;
 }
 event.preventDefault();readSidebarItem(next);
});
// Source text is a complete, bounded excerpt from the host's focused viewport.
// Reading never grants input authority or rebuilds an inferred editor tree.
let sourceCovered=false;
function clearSource(){
 sourceText.value='';sourceAccess.hidden=true;
 for(const element of [sourceAccess,sourceText]){element.removeAttribute('aria-label');element.removeAttribute('aria-description');}
}
function receiveSource(value){
 const invalid=()=>{clearSource();throw new Error('Invalid source metadata');};
 const integer=n=>Number.isSafeInteger(n)&&n>=0;
 const textWithin=(text,limit,multiline=false,expectedLines=1)=>{
   if(typeof text!=='string'||text.length>limit*2)return false;
   let length=0,lineCount=1,lineLength=0;
   for(const scalar of text){
     const code=scalar.codePointAt(0);
     if(++length>limit||code>=0xd800&&code<=0xdfff||(code<32||code>=127&&code<=159)&&!(multiline&&code===10))return false;
     if(code===10){if(++lineCount>expectedLines)return false;lineLength=0;}
     else if(multiline&&++lineLength>2048)return false;
   }
   return lineCount===expectedLines;
 };
 if(value===null||value===undefined){clearSource();return;}
 if(!value||value.readOnly!==true||typeof value.present!=='boolean')invalid();
 if(!value.present){clearSource();return;}
 const id=value.id,r=value.bounds,lastLine=value.firstLine+(value.lineCount-1);
 if(!Array.isArray(id)||id.length!==3||id[0]!=='source'||!id.slice(1).every(part=>typeof part==='string'&&/^(0|[1-9][0-9]{0,19})$/.test(part))||
   !integer(value.revision)||typeof value.truncated!=='boolean'||
   ![value.firstLine,value.firstColumn,value.lineCount].every(integer)||value.firstLine<1||value.lineCount<1||value.lineCount>256||
   !Number.isSafeInteger(lastLine)||!Array.isArray(r)||r.length!==4||!r.every(integer)||
   r[2]<1||r[3]<1||r[0]+r[2]>cols||r[1]+r[3]>lines||value.lineCount>r[3]||
   !textWithin(value.name,256)||!textWithin(value.value,32768,true,value.lineCount))invalid();
 if(sourceCovered){clearSource();return;}
 const name=value.name||'Source';
 const description=`Read-only visible source excerpt. Lines ${value.firstLine} to ${lastLine}, display column offset ${value.firstColumn}. Limited to 256 lines, 2,048 characters per line and 32,768 characters total. `+
   (value.truncated?'More source text is not included. ':'Only the visible excerpt is included. ')+'Use editor controls to edit.';
 sourceAccess.setAttribute('aria-label',`Visible source: ${name}`);
 sourceText.setAttribute('aria-label',`Source excerpt: ${name}`);sourceText.setAttribute('aria-description',description);
 sourceText.readOnly=true;sourceText.setAttribute('aria-readonly','true');sourceText.setAttribute('aria-multiline','true');
 if(sourceText.value!==value.value)sourceText.value=value.value;
 sourceAccess.hidden=false;
}
// A complete current-modal projection grants reading, never editor actions.
function clearDialog(){
 dialogAccess.replaceChildren();dialogAccess.hidden=true;dialogAccess.removeAttribute('aria-label');
 sourceCovered=false;sourceAccess.removeAttribute('aria-hidden');
 sidebarAccess.removeAttribute('aria-hidden');imageAccess.removeAttribute('aria-hidden');
}
function receiveDialog(value){
 const invalid=()=>{clearDialog();clearSource();throw new Error('Invalid dialog metadata');};
 const integer=n=>Number.isSafeInteger(n)&&n>=0;
 const validId=id=>Array.isArray(id)&&id.every(part=>typeof part==='string'&&part.length<=24)&&id[0]==='dialog'&&(id.length===1||
   id.length===3&&['field','body','button'].includes(id[1])&&/^[0-9]{1,24}$/.test(id[2])||
   id.length===5&&id[1]==='field'&&/^[0-9]{1,24}$/.test(id[2])&&id[3]==='option'&&/^[0-9]{1,24}$/.test(id[4]));
 if(!value||value.readOnly!==true||typeof value.present!=='boolean'||typeof value.truncated!=='boolean'||
   !Array.isArray(value.nodes)||value.nodes.length>256||!value.present&&value.nodes.length)invalid();
 const nodes=new Map();let textSize=0;
 for(const node of value.nodes){
   if(!node||!validId(node.id)||!['dialog','text','textbox','checkbox','radiogroup','radio','listbox','option','combobox','button'].includes(node.role)||
     typeof node.name!=='string'||Array.from(node.name).length>256||!(node.value===null||typeof node.value==='string'&&Array.from(node.value).length<=2048)||
     typeof node.focused!=='boolean'||typeof node.multiline!=='boolean'||![node.checked,node.selected,node.expanded].every(v=>v===null||typeof v==='boolean'))invalid();
   const r=node.bounds;if(!Array.isArray(r)||r.length!==4||!r.every(integer)||r[2]<1||r[3]<1||r[0]+r[2]>cols||r[1]+r[3]>lines)invalid();
   textSize+=Array.from(node.name).length+(node.value===null?0:Array.from(node.value).length);
   const key=JSON.stringify(node.id);if(nodes.has(key)||textSize>32768)invalid();nodes.set(key,node);
 }
 const root=nodes.get('["dialog"]');
 if(value.nodes.length&&(!root||root.role!=='dialog'||root.parent!==null))invalid();
 for(const node of value.nodes)if(node!==root){
   if(node.role==='dialog'||!validId(node.parent))invalid();
   const parent=nodes.get(JSON.stringify(node.parent));
   if(!parent||node.id.length<=parent.id.length||!parent.id.every((part,i)=>part===node.id[i])||
      parent!==root&&!['radiogroup','listbox','combobox'].includes(parent.role))invalid();
 }
 if(!value.present){clearDialog();return;}
 const reading=dialogAccess.contains(document.activeElement)&&document.activeElement.tagName==='TEXTAREA'?document.activeElement:null;
 if(!root)clearDialog();
 sourceCovered=true;clearSource();sourceAccess.setAttribute('aria-hidden','true');
 sidebarAccess.setAttribute('aria-hidden','true');imageAccess.setAttribute('aria-hidden','true');
 if(!root)return; // A private modal still covers the underlying semantic surface.
 const elements=new Map(),containers=new Map(),rootItems=[];let retainedRoot=null;containers.set('["dialog"]',dialogAccess);
 for(const [index,node] of value.nodes.entries())if(node!==root){
   const key=JSON.stringify(node.id),retained=reading?.dataset.dialogKey===key&&reading.getAttribute('role')===node.role;
   let item=retained?reading:document.createElement(node.role==='textbox'||node.role==='combobox'?'textarea':'div'),container=item;
   item.dataset.dialogKey=key;
   for(const state of ['checked','selected','expanded'])item.removeAttribute(`aria-${state}`);
   item.setAttribute('role',node.role==='text'?'none':node.role);
   if(node.role!=='text')item.setAttribute('aria-label',node.name);
   item.setAttribute('aria-description',[node.focused?'Focused in editor':'',node.value===null&&['textbox','combobox'].includes(node.role)?'Value hidden':''].filter(Boolean).join('. '));
   for(const state of ['checked','selected','expanded'])if(node[state]!==null)item.setAttribute(`aria-${state}`,node[state]);
   if(['textbox','checkbox','radiogroup','listbox','combobox'].includes(node.role))item.setAttribute('aria-readonly','true');
   if(node.role==='textbox'||node.role==='combobox'){
     item.readOnly=true;item.tabIndex=-1;item.value=node.value||'';item.rows=node.multiline?3:1;item.setAttribute('aria-multiline',node.multiline);
   }else item.textContent=node.name+(node.value===null?'':`: ${node.value}`);
   if(node.role==='combobox'){
     const wrapper=retained?reading.parentElement:document.createElement('div'),options=document.createElement('div');
     if(retained){for(const child of [...wrapper.children])if(child!==reading)child.remove();}
     options.id=`dialog-options-${index}`;options.setAttribute('role','listbox');
     options.setAttribute('aria-label',node.name);item.setAttribute('aria-haspopup','listbox');item.setAttribute('aria-controls',options.id);if(!retained)wrapper.append(item);wrapper.append(options);item=wrapper;container=options;
   }
   if(retained)retainedRoot=item;elements.set(key,item);containers.set(key,container);
 }
 for(const node of value.nodes)if(node!==root){const parent=containers.get(JSON.stringify(node.parent)),item=elements.get(JSON.stringify(node.id));if(parent===dialogAccess)rootItems.push(item);else parent.append(item);}
 if(retainedRoot){
   // Do not detach the user's already-focused read-only control or move focus.
   for(const child of [...dialogAccess.children])if(child!==retainedRoot)child.remove();
   let before=true;for(const item of rootItems){if(item===retainedRoot){before=false;continue;}if(before)dialogAccess.insertBefore(item,retainedRoot);else dialogAccess.append(item);}
 }else dialogAccess.replaceChildren(...rootItems);
 dialogAccess.setAttribute('aria-label',root.name);
 dialogAccess.setAttribute('aria-description','Read-only current dialog. Use editor controls to edit or activate.'+(value.truncated?' More dialog content is not included.':''));
 dialogAccess.hidden=false;
}
// Authorized download bytes arrive over the authenticated socket, never a host
// path. Chromium's DownloadURL is synchronous; keep an explicit click fallback.
let preparedDownload=null,draggedDownload=null;
const draggedDownloads=new Map();
function clearDownload(){
 if(preparedDownload)URL.revokeObjectURL(preparedDownload.url);
 for(const [url,timer] of draggedDownloads){clearTimeout(timer);URL.revokeObjectURL(url);}
 draggedDownloads.clear();preparedDownload=null;draggedDownload=null;downloadInfo=null;
 downloadAction.hidden=true;downloadAction.href='';downloadAction.removeAttribute?.('download');
}
function receiveDownload(metadata,bytes){
 if(preparedDownload&&!draggedDownloads.has(preparedDownload.url))URL.revokeObjectURL(preparedDownload.url);
 const basename=String(metadata.name||'download').split(/[\/\\]/).at(-1);
 const name=basename.replace(/[\x00-\x1f\x7f:]/g,'_').slice(0,255);
 const blob=new Blob([bytes],{type:'application/octet-stream'});
 preparedDownload={url:URL.createObjectURL(blob),name:name&&name!=='.'&&name!=='..'?name:'download',size:blob.size};
 downloadAction.href=preparedDownload.url;downloadAction.download=preparedDownload.name;
 downloadAction.textContent=`Download ${preparedDownload.name}`;downloadAction.hidden=false;
 if(metadata.purpose==='file-export')status.textContent='Snapshot ready. Drag Download in Chromium, or click it to export a copy.';
 else downloadAction.click();
}
downloadAction.addEventListener('click',()=>{status.textContent='Download requested; the source and its saved state are unchanged.';});
downloadAction.addEventListener('dragstart',event=>{
 if(!preparedDownload||!event.dataTransfer){event.preventDefault();return;}
 const {url,name,size}=preparedDownload;
 if(size>16*1024*1024||!draggedDownloads.has(url)&&draggedDownloads.size>=4){
   event.preventDefault();status.textContent='Drag export is unavailable; click Download instead.';return;
 }
 event.dataTransfer.clearData();
 event.dataTransfer.setData('DownloadURL',`application/octet-stream:${name}:${url}`);
 event.dataTransfer.effectAllowed='copy';
 clearTimeout(draggedDownloads.get(url));draggedDownloads.set(url,null);draggedDownload=url;
 status.textContent='Exporting a copy; click Download if the destination cannot receive the drag.';
});
downloadAction.addEventListener('dragend',event=>{
 const url=draggedDownload;draggedDownload=null;
 if(url&&draggedDownloads.has(url))draggedDownloads.set(url,setTimeout(()=>{
   draggedDownloads.delete(url);if(preparedDownload?.url!==url)URL.revokeObjectURL(url);
 },5*60*1000));
 status.textContent=event.dataTransfer?.dropEffect==='none'?'Export cancelled; Download remains available.':
   'Drag ended; Download remains available if the destination could not receive the copy.';
});
const systemTheme=matchMedia('(prefers-color-scheme: dark)');
systemTheme.addEventListener('change',()=>send({type:'theme',dark:systemTheme.matches}));
function connect(){
 if(closed)return;
 attachmentEpoch++;
 socket=new WebSocket(new URL('socket',location.href).href.replace(/^http/,'ws'));
 socket.binaryType='arraybuffer';
 const connection=socket,wireRows=[];let incoming=Promise.resolve(),platformSent=false,stopped=false;
 socket.onmessage=e=>{incoming=incoming.then(()=>{if(!stopped&&connection===socket)return receive(e);}).catch(error=>{if(stopped||connection!==socket)return;status.textContent=`Display error: ${error.message}`;connection.close();});};
 async function receive(e){
   let message;
   if(e.data instanceof ArrayBuffer){
     if(images.chunk!==null){images.binary(e.data);dirty=true;return;}
     if(downloadInfo!==null){
       receiveDownload(downloadInfo,e.data);downloadInfo=null;
       return;
     }
     message=await decodeFrame(new Uint8Array(e.data),wireRows);
   }else {
     if(images.chunk!==null||downloadInfo!==null)throw new Error('Expected binary payload');
     message=JSON.parse(e.data);
   }
   if(stopped||connection!==socket)return;
   if(message.type?.startsWith('canvas-')){images.control(message);dirty=true;
   }else if(message.type==='remote'){remoteHost=message.host;sessionFrontend=true;
   }else if(message.type==='connection'){
     ready=message.connected&&glyphs.size>0;if(!ready){attachmentEpoch++;clearClipboardRequest();clearSidebar();clearDialog();clearSource();images.clear();dirty=true;}status.textContent=message.message|| (ready?'Connected':'Reconnecting…');
   }else if(message.type==='assets'){
     attachmentEpoch++;clearSidebar();clearDialog();clearSource();images.clear();dirty=true;
     glyphs=new Map(message.glyphs.map(([c,w,rs])=>[c,[w,rs]]));tiles.clear();atlasEntries.clear();scale=initialScale=message.scale||2;ready=true;status.textContent='Connected';lastSize='';send({type:'theme',dark:systemTheme.matches});
   }else if(message.type==='frame'){
     if(!platformSent){platformSent=true;send({type:'frontend',mode:message.mode||3,mac:navigator.platform.includes('Mac')});}
     message.rows=decodeRows(message.rows);
     const oldCursor=JSON.stringify(frame?.cursor);
     const changedMode=Object.hasOwn(message,'mode')&&mode!==message.mode;
     frame={...frame,...message};updateTitle();unsaved=frame.dirty;guardLeave();[cols,lines]=frame.size;mode=frame.mode||3;clipboard=frame.selection;
     if(Object.hasOwn(message,'semanticDialog'))receiveDialog(message.semanticDialog);
     else if(message.reset)clearDialog();
     if(Object.hasOwn(message,'semanticSource'))receiveSource(frame.semanticSource);
     else if(message.reset)clearSource();
     if(Object.hasOwn(message,'semanticSidebar'))receiveSidebar(message.semanticSidebar);
     else if(message.reset)clearSidebar();
     if(Object.hasOwn(message,'canvas'))images.receive(message.canvas,cols,lines);
     else if(message.reset){images.scene=null;images.describe();}
     if(message.reset){rows=Array(lines).fill(null);}
     for(const [y,r] of message.rows)rows[y]=r;
     if(oldCursor!==JSON.stringify(frame.cursor))cursorEpoch=performance.now();
     if(!allocate())drawRows(message.rows);
     if(changedMode)lastSize='';resize();
   }else if(message.type==='download'){
     downloadInfo=message;
   }else if(message.type==='open-resource'){
     receiveResource(message);
   }else if(message.type==='copy'){
     if(nativeCopies.shift()!==message.text)systemClipboard(message);
   }else if(message.type==='paste-request'){
     systemClipboard(message);
   }else if(message.type==='ack'){
     acknowledged=Math.max(acknowledged,message.seq);if(Object.hasOwn(message,"dirty"))unsaved=message.dirty;guardLeave();
   }else if(message.type==='detached'){
     attachmentEpoch++;detached=true;closed=true;ready=false;clearDownload();clearSidebar();clearDialog();clearSource();images.clear();dirty=true;guardLeave();fullscreen.disabled=true;
     status.textContent='Session detached. Resume from your terminal with hide --resume.';
     navigator.keyboard?.unlock?.();socket.close();
   }else if(message.type==='closed'){
     attachmentEpoch++;closed=true;ready=false;clearDownload();clearSidebar();clearDialog();clearSource();images.clear();dirty=true;guardLeave();fullscreen.disabled=true;
     status.textContent='Editor closed. You can close this tab.';
     navigator.keyboard?.unlock?.();socket.close();window.close();
   }
 };
 socket.onclose=event=>{stopped=true;if(connection!==socket)return;attachmentEpoch++;console.info('Editor connection closed',event.code,event.reason);ready=false;downloadInfo=null;clearClipboardRequest();clearSidebar();clearDialog();clearSource();images.clear();dirty=true;mouse=[-1,-1];dirty=true;if(!closed){detaching=false;status.textContent='Disconnected — reconnecting…';setTimeout(connect,1000);}};
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
function semanticReadingTarget(target){return sidebarAccess.contains(target)||dialogAccess.contains(target)||sourceAccess.contains(target);}
window.addEventListener('keydown',e=>{
 if(semanticReadingTarget(e.target))return;
 if(sessionFrontend&&e.key===']'&&e.ctrlKey&&!e.metaKey&&!e.altKey&&!e.shiftKey){
   e.preventDefault();
   if(!closed&&!detaching&&socket?.readyState===WebSocket.OPEN){
     socket.send(JSON.stringify({type:'detach'}));detaching=true;ready=false;
     status.textContent='Detaching session…';
   }
   return;
 }
 if(e.target instanceof HTMLButtonElement||e.target===downloadAction)return;
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
window.addEventListener('keyup',e=>{if(!semanticReadingTarget(e.target))send({type:'modifiers',mods:mods(e)});});
input.addEventListener('compositionstart',()=>{composing=true;});
input.addEventListener('compositionend',e=>{composing=false;if(e.data)send({type:'paste',text:e.data});input.value='';});
input.addEventListener('input',e=>{if(!composing){const text=input.value.replace(/^\u200b/,'');if(text)send({type:'paste',text});input.value='\u200b';input.setSelectionRange(1,1);}});
window.addEventListener('paste',e=>{if(e.target===fullscreen||semanticReadingTarget(e.target))return;e.preventDefault();send({type:'paste',text:e.clipboardData.getData('text/plain')});});
window.addEventListener('copy',e=>{if(semanticReadingTarget(e.target))return;e.preventDefault();e.clipboardData.setData('text/plain',clipboard);nativeCopies.push(clipboard);command('hide.edit.copy');});
window.addEventListener('cut',e=>{if(semanticReadingTarget(e.target))return;e.preventDefault();e.clipboardData.setData('text/plain',clipboard);nativeCopies.push(clipboard);command('hide.edit.cut');});
input.addEventListener('beforeinput',e=>{
 const name={historyUndo:'hide.edit.undo',historyRedo:'hide.edit.redo'}[e.inputType];
 if(name){e.preventDefault();command(name);}
});
// A nonempty selection target lets browser Edit > Select All reach the canvas editor.
input.addEventListener('select',()=>{
 if(input.value==='\u200b'&&input.selectionStart===0&&input.selectionEnd===1){command('hide.edit.select-all');input.setSelectionRange(1,1);}
});
input.addEventListener('focus',()=>{if(!input.value){input.value='\u200b';input.setSelectionRange(1,1);}});
window.addEventListener('dragover',e=>{if(semanticReadingTarget(e.target)){e.preventDefault();e.dataTransfer.dropEffect='none';return;}if([...e.dataTransfer.types].includes('Files')){e.preventDefault();e.dataTransfer.dropEffect='copy';}});
window.addEventListener('drop',async e=>{
 e.preventDefault();if(semanticReadingTarget(e.target))return;const files=[...e.dataTransfer.files],connection=socket,epoch=attachmentEpoch;
 const current=()=>connection===socket&&epoch===attachmentEpoch&&ready&&!closed&&!detaching&&connection?.readyState===WebSocket.OPEN;
 if(!current()){if(!closed&&!detaching)status.textContent='Reconnect before dropping files.';return;}
 for(const file of files){
   if(file.size>16*1024*1024){status.textContent=`${file.name}: browser drops are limited to 16 MiB per file.`;continue;}
   try{
     const bytes=await file.arrayBuffer();
     if(!current()){if(!closed&&!detaching)status.textContent='Connection changed; drop the file again.';break;}
     send({type:'upload',name:file.name});connection.send(bytes);
   }catch(error){if(current())status.textContent=`Cannot open ${file.name}: ${error.message}`;}
 }
 if(current())input.focus({preventScroll:true});
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
canvas.addEventListener('webglcontextlost',e=>{e.preventDefault();contextLost=true;images.lost();status.textContent='WebGL context lost — restoring display…';});
canvas.addEventListener('webglcontextrestored',()=>{
 try{atlasSize=2048;atlasX=1;atlasY=atlasRow=0;atlasEntries.clear();gridCols=gridRows=0;initializeGPU();contextLost=false;gl.viewport(0,0,canvas.width,canvas.height);drawRows(rows.map((r,y)=>[y,r]));dirty=true;status.textContent=ready?'Connected':'Disconnected';}
 catch(error){status.textContent='Cannot restore display: '+error.message;}
});
input.focus({preventScroll:true});
