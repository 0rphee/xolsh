// import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.2.21/dist/index.js";
// import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

import {
  WASI,
  OpenFile,
  File,
  ConsoleStdout,
  PreopenDirectory,
} from "@bjorn3/browser_wasi_shim";
import { createEditorView } from "./editorconf.js";

function encode(str) {
  return new TextEncoder().encode(str);
}

const loxFilename = "hello.lox";
const originalLoxFileString = `print "hello";\n`;
const loxFile = new File(
  new TextEncoder("utf-8").encode(originalLoxFileString),
);

const buttonElement = document.getElementById("run-button");
const editorContainer = document.getElementById("content");
const outputBox = document.getElementById("output-box");
outputBox.innerHTML = "";

const editorView = createEditorView(editorContainer, originalLoxFileString);
const wasiArgs = ["xolsh-exe.wasm", "hello.lox"];
const wasiEnv = [];

const wasiStdout = ConsoleStdout.lineBuffered((msg) => {
  outputBox.innerHTML += `${msg}\n`;
  // outputBox.innerText += `[WASI stdout] ${msg}\n`;
  // console.log(`[WASI stdout] ${msg}`);
});
const wasiStderr = ConsoleStdout.lineBuffered((msg) => {
  outputBox.innerHTML += `<span class="stderr">${msg}</span>\n`;
  // outputBox.innerText += `[WASI stderr] ${msg}\n`;
  // console.warn(`[WASI stderr] ${msg}`);
});

const fds = [
  new OpenFile(new File([])), // stdin
  wasiStdout,
  wasiStderr,
  new PreopenDirectory("/", [[loxFilename, loxFile]]),
];
const wasiOptions = { debug: false };
const instance_exports = {};
const wasmSource = await fetch("xolsh-exe.wasm").then((resp) =>
  resp.arrayBuffer(),
);

async function runWasi() {
  const wasi = new WASI(wasiArgs, wasiEnv, fds, wasiOptions);
  const { instance } = await WebAssembly.instantiate(wasmSource, {
    wasi_snapshot_preview1: wasi.wasiImport,
    // ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
  });
  Object.assign(instance_exports, instance.exports);
  wasi.start(instance);
}

await runWasi();
// Event listener for the button
buttonElement.addEventListener("click", async () => {
  loxFile.data = encode(editorView.state.doc);
  outputBox.innerHTML = "";
  await runWasi();
});

// await instance.exports.main2();
// console.log(await instance.exports.plus3(3));
