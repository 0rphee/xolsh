// import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.2.21/dist/index.js";
// import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

import {
  WASI,
  OpenFile,
  File,
  ConsoleStdout,
  PreopenDirectory,
} from "@bjorn3/browser_wasi_shim";
import { basicSetup } from "codemirror";
import { EditorView } from "@codemirror/view";

const loxFilename = "hello.lox";
const originalLoxFileStr = `print "hello";`;
const loxFile = new File(new TextEncoder("utf-8").encode(originalLoxFileStr));

const buttonElement = document.getElementById("run-button");
const editorContainer = document.getElementById("content");
const rightBox = document.getElementById("right-box");
rightBox.value = "";

const editorView = new EditorView({
  doc: originalLoxFileStr,
  extensions: [basicSetup],
  parent: editorContainer,
});

const args = ["xolsh-exe.wasm", "hello.lox"];
const env = [];

const stdout = ConsoleStdout.lineBuffered((msg) => {
  rightBox.value += `[WASI stdout] ${msg}\n`;
  // console.log(`[WASI stdout] ${msg}`);
});
const stderr = ConsoleStdout.lineBuffered((msg) => {
  rightBox.value += `[WASI stderr] ${msg}\n`;
  // console.warn(`[WASI stderr] ${msg}`);
});

const fds = [
  new OpenFile(new File([])), // stdin
  stdout,
  stderr,
  new PreopenDirectory("/", [[loxFilename, loxFile]]),
];
const options = { debug: false };
const instance_exports = {};
const wasmSource = await fetch("xolsh-exe.wasm").then((resp) =>
  resp.arrayBuffer(),
);

const runWasi = async () => {
  const wasi = new WASI(args, env, fds, options);
  const instance = await (async () => {
    const { instance } = await WebAssembly.instantiate(wasmSource, {
      wasi_snapshot_preview1: wasi.wasiImport,
      // ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
    });
    // instance.exports = new Object();
    Object.assign(instance_exports, instance.exports);
    return instance;
  })();
  wasi.start(instance);
};

const encode = (str) => new TextEncoder().encode(str);
const decode = (str) => new TextDecoder().decode(str);

await runWasi();
// Event listener for the button
buttonElement.addEventListener("click", async () => {
  loxFile.data = encode(editorView.state.doc);
  rightBox.value = "";
  await runWasi();
});

// await instance.exports.main2();
// console.log(await instance.exports.plus3(3));
