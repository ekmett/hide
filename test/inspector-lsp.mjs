#!/usr/bin/env node
// SPDX-FileCopyrightText: 2026 Edward Kmett
// SPDX-License-Identifier: BSD-2-Clause OR Apache-2.0
// A deterministic FAKE language server for regression tests of symbol inspection.
// It is NOT HLS and its answers never count as a real-HLS result. It speaks just
// enough LSP over stdio (started as `fake-lsp.mjs --lsp` in the project root) to
// answer hover, definition and references by scanning the project's .hs files
// for whole-word matches. A definition is the first top-level line starting with
// the word; references are every whole-word occurrence.
//
// Fault injection by symbol name, for the failure paths:
//   boom      -> JSON-RPC error for every request on it
//   slow      -> never answers (the editor must time out or cancel)
//   empty     -> null/[] answers
//   elsewhere -> also reports the --outside path (outside the workspace)
import {readdirSync, readFileSync, statSync} from 'node:fs';
import {join} from 'node:path';
import {pathToFileURL, fileURLToPath} from 'node:url';

const root = process.cwd();
const documents = new Map();
let buffer = Buffer.alloc(0);

function send(value) {
  const body = Buffer.from(JSON.stringify({jsonrpc: '2.0', ...value}));
  process.stdout.write(`Content-Length: ${body.length}\r\n\r\n`);
  process.stdout.write(body);
}

function sources(directory = root, found = []) {
  for (const name of readdirSync(directory).sort()) {
    if (name.startsWith('.') || name === 'dist-newstyle') continue;
    const path = join(directory, name);
    const stat = statSync(path);
    if (stat.isDirectory()) sources(path, found);
    else if (name.endsWith('.hs')) found.push(path);
  }
  return found;
}

function textOf(path) {
  const uri = pathToFileURL(path).href;
  return documents.has(uri) ? documents.get(uri) : readFileSync(path, 'utf8');
}

const wordPattern = /[A-Za-z0-9_']+/g;
function wordAt(text, position) {
  const line = text.split('\n')[position.line] ?? '';
  for (const match of line.matchAll(wordPattern)) {
    if (position.character >= match.index && position.character <= match.index + match[0].length) return match[0];
  }
  return undefined;
}

function occurrences(word) {
  const found = [];
  const whole = new RegExp(`(?<![A-Za-z0-9_'])${word.replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}(?![A-Za-z0-9_'])`, 'g');
  for (const path of sources()) {
    textOf(path).split('\n').forEach((line, row) => {
      if (line.trimStart().startsWith('--')) return;
      for (const match of line.matchAll(whole)) found.push({path, row, column: match.index, line});
    });
  }
  return found;
}

const location = (path, row, column, length) => ({
  uri: pathToFileURL(path).href,
  range: {start: {line: row, character: column}, end: {line: row, character: column + length}}});

function answer(id, method, params) {
  const path = fileURLToPath(params.textDocument.uri);
  const word = wordAt(textOf(path), params.position);
  if (word === 'slow') return;
  if (word === 'boom') return send({id, error: {code: -32603, message: 'fake: boom exploded'}});
  if (!word || word === 'empty') return send({id, result: method === 'textDocument/hover' ? null : []});
  const all = occurrences(word);
  const definition = all.find(site => site.line.startsWith(word + ' '));
  const outside = word === 'elsewhere' && process.argv[process.argv.indexOf("--outside")+1]
    ? [location(process.argv[process.argv.indexOf("--outside")+1], 0, 0, 1)] : [];
  if (method === 'textDocument/hover') {
    const signature = all.find(site => new RegExp(`^${word}\\s*::`).test(site.line));
    return send({id, result: signature
      ? {contents: {kind: 'markdown', value: '```haskell\n' + signature.line.trim() + '\n```\nFake hover.'}}
      : null});
  }
  if (method === 'textDocument/definition') {
    return send({id, result: [...(definition ? [location(definition.path, definition.row, definition.column, word.length)] : []), ...outside]});
  }
  if (method === 'textDocument/references') {
    const include = params.context?.includeDeclaration !== false;
    return send({id, result: [...all.filter(site => include || site !== definition)
      .map(site => location(site.path, site.row, site.column, word.length)), ...outside]});
  }
  send({id, result: null});
}

function handle(message) {
  const {id, method, params} = message;
  switch (method) {
    case 'initialize':
      return send({id, result: {capabilities: {textDocumentSync: 1, hoverProvider: true, definitionProvider: true,
        referencesProvider: true}, serverInfo: {name: 'fake-lsp', version: '0'}}});
    case 'shutdown': return send({id, result: null});
    case 'exit': return process.exit(0);
    case 'textDocument/didOpen': return documents.set(params.textDocument.uri, params.textDocument.text);
    case 'textDocument/didChange': return documents.set(params.textDocument.uri, params.contentChanges.at(-1).text);
    case 'textDocument/didClose': return documents.delete(params.textDocument.uri);
    case 'textDocument/hover':
    case 'textDocument/definition':
    case 'textDocument/references': return answer(id, method, params);
    default:
      if (id !== undefined && method) send({id, result: null});
  }
}

if (!process.argv.includes('--lsp')) { console.error('usage: fake-lsp.mjs --lsp'); process.exit(2); }
process.stdin.on('data', chunk => {
  buffer = Buffer.concat([buffer, chunk]);
  for (;;) {
    const end = buffer.indexOf('\r\n\r\n');
    if (end < 0) return;
    const length = Number(/Content-Length: (\d+)/i.exec(buffer.subarray(0, end).toString())?.[1]);
    if (!Number.isFinite(length) || buffer.length < end + 4 + length) return;
    const body = buffer.subarray(end + 4, end + 4 + length).toString();
    buffer = buffer.subarray(end + 4 + length);
    try { handle(JSON.parse(body)); } catch (error) { console.error(String(error)); }
  }
});
process.stdin.on('end', () => process.exit(0));
