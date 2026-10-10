// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
// Build only the optional browser runtime, from explicit pinned dependency
// inputs into the supplied external output directory. Does not acquire or copy model weights.
import fs from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath,pathToFileURL} from 'node:url';
import {createHash} from 'node:crypto';
import {execFileSync} from 'node:child_process';

const [layaInput,dependencyInput,outputInput,...extra]=process.argv.slice(2);
if(!layaInput||!dependencyInput||!outputInput||extra.length)throw Error('Usage: node tools/build-system-one-web.mjs LAYA_CHECKOUT DEPENDENCIES_DIRECTORY OUTPUT_DIRECTORY');
const laya=await fs.realpath(layaInput),dependencies=await fs.realpath(dependencyInput),output=path.resolve(outputInput);
const source=path.resolve(path.dirname(fileURLToPath(import.meta.url)),'../assets/web/system-one-worker.js');
const revision='1adc59f7e371deb601fcfa18a14e25db238addcc';
const sha=data=>createHash('sha256').update(data).digest('hex');
const git=(...args)=>execFileSync('git',['-C',laya,...args],{encoding:'utf8'}).trim();
if(git('rev-parse','HEAD')!==revision)throw Error('Laya checkout does not match the supported pin '+revision);
const changes=git('diff','--name-only','HEAD','--','laya-ts/src').split('\n').filter(Boolean);
if(changes.some(name=>name!=='laya-ts/src/hooks.ts')||git('ls-files','--others','--exclude-standard','--','laya-ts/src'))throw Error('Pinned Laya runtime sources have unrelated changes');
const hooksPath=path.join(laya,'laya-ts/src/hooks.ts'),hooks=await fs.readFile(hooksPath,'utf8');
const originalHooks='fdc00f70cf7029ec1ea0d126951e28e9bcbcd32bddff6dd8e5fb3cc3cf487220',patchedHooks='d547ccce40514feb92b8b3a22ad00dfc98691be032425c77cf9936044682f25a';
let compiledHooks=hooks;
if(sha(hooks)===originalHooks){
 const lines=hooks.split('\n');
 // The pinned source has a quote/parenthesis typo at this exact line. The
 // source-loader repair is explicit and hashed; the input checkout is untouched.
 lines[350]='        reject(new Error(`hook ${operationName} timed out after ${timeoutMs}ms`));';
 compiledHooks=lines.join('\n');
}
if(sha(compiledHooks)!==patchedHooks)throw Error('Pinned Laya hooks syntax correction does not match its recorded hash');
const versions={'esbuild':'0.28.2','onnxruntime-web':'1.30.0','onnxruntime-common':'1.30.0','hash-wasm':'4.12.0'};
for(const [name,version] of Object.entries(versions)){
 const installed=JSON.parse(await fs.readFile(path.join(dependencies,'node_modules',name,'package.json'),'utf8'));
 if(installed.version!==version)throw Error(`Expected ${name}@${version}`);
}
const runtimeAssets={
 'ort-wasm-simd-threaded.asyncify.mjs':'3d1c85995364bb643302fc6fd877a0c3ba5ae72401815e0f24828a53d9191e28',
 'ort-wasm-simd-threaded.asyncify.wasm':'39f9f0894d478800487ed9f7dbe92618498db320cf55c8e3d89adff8dce658da'
};
const runtime=path.join(dependencies,'node_modules/onnxruntime-web/dist');
for(const [name,expected] of Object.entries(runtimeAssets))if(sha(await fs.readFile(path.join(runtime,name)))!==expected)throw Error('Runtime artifact mismatch: '+name);
const attribution=[];
for(const [name,directory] of [['Laya',laya],['hash-wasm',path.join(dependencies,'node_modules/hash-wasm')]]){
 let license;
 for(const name of ['LICENSE','LICENSE.txt','LICENSE.md']){try{license=await fs.readFile(path.join(directory,name),'utf8');break;}catch(error){if(error.code!=='ENOENT')throw error;}}
 if(!license)throw Error('Missing runtime license: '+name);
 attribution.push(name+'\n\n'+license);
}
// The published ORT npm packages omit these notices. Fetch only the two small,
// immutable release documents and verify their exact bytes before packaging.
const ortNotices={
 'LICENSE':'2f07c72751aed99790b8a4869cf2311df85a860b22ded05fa22803587a48922c',
 'ThirdPartyNotices.txt':'143764b952fdb1a7c69ce653bfba74a7744d6a8a573bfb73e235fba356c83de3'
};
for(const [name,expected] of Object.entries(ortNotices)){
 const response=await fetch('https://raw.githubusercontent.com/microsoft/onnxruntime/v1.30.0/'+name,{signal:AbortSignal.timeout(30000)});
 if(!response.ok)throw Error('Cannot acquire ONNX Runtime notice: '+name);
 const data=Buffer.from(await response.arrayBuffer());
 if(sha(data)!==expected)throw Error('ONNX Runtime notice identity: '+name);
 attribution.push('ONNX Runtime 1.30.0 '+name+'\n\n'+data.toString('utf8'));
}
const {build}=await import(pathToFileURL(path.join(dependencies,'node_modules/esbuild/lib/main.js')).href);
const worker=path.join(output,'system-one-runtime.js');
const compiled=await build({entryPoints:[source],outfile:worker,write:false,bundle:true,format:'esm',platform:'browser',target:'chrome155',
 conditions:['onnxruntime-web-use-extern-wasm'],nodePaths:[path.join(dependencies,'node_modules')],
 alias:{'pinned-laya':path.join(laya,'laya-ts/src')},external:['onnxruntime-node','node:*','fs','path'],
 plugins:[{name:'pinned-laya-hooks-syntax',setup(builder){builder.onLoad({filter:/[/\\]hooks\.ts$/},args=>args.path===hooksPath?{contents:compiledHooks,loader:'ts'}:undefined);}}]});
// Resolve, verify and compile every input before changing any generated output.
// The caller owns this directory; rebuilding its named artifacts needs no cleanup.
await fs.mkdir(output,{recursive:true});
for(const file of compiled.outputFiles)await fs.writeFile(file.path,file.contents);
for(const name of Object.keys(runtimeAssets))await fs.copyFile(path.join(runtime,name),path.join(output,name));
await fs.writeFile(path.join(output,'THIRD_PARTY_LICENSES.txt'),attribution.join('\n\n'));
const files=[];
for(const name of ['system-one-runtime.js',...Object.keys(runtimeAssets),'THIRD_PARTY_LICENSES.txt']){
 const data=await fs.readFile(path.join(output,name));files.push({path:name,bytes:data.length,sha256:sha(data)});
}
const provenance={format:'hide-system-one-web-runtime',version:1,layaRevision:revision,layaHooksSHA256:patchedHooks,
 sourceSHA256:sha(await fs.readFile(source)),dependencies:versions,ortNotices,files};
await fs.writeFile(path.join(output,'runtime-manifest.json'),JSON.stringify(provenance,null,2)+'\n');
console.log(JSON.stringify({runtimeDirectory:output,manifestSHA256:sha(await fs.readFile(path.join(output,'runtime-manifest.json'))),files},null,2));
