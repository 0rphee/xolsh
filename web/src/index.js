// import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.2.21/dist/index.js";

import {
  WASI,
  OpenFile,
  File,
  ConsoleStdout,
  PreopenDirectory,
} from "@bjorn3/browser_wasi_shim";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const args = [];
const env = [];

const loxFilename = "hello.lox";
const loxFile = new File(new TextEncoder("utf-8").encode(`print "hello";`));

const stdout = ConsoleStdout.lineBuffered((msg) =>
  console.log(`[WASI stdout] ${msg}`),
);

const stderr = ConsoleStdout.lineBuffered((msg) =>
  console.warn(`[WASI stderr] ${msg}`),
);

const fds = [
  new OpenFile(new File([])), // stdin
  stdout,
  stderr,
  new PreopenDirectory("/", [[loxFilename, loxFile]]),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(
  fetch("xolsh-web.wasm"),
  {
    wasi_snapshot_preview1: wasi.wasiImport,
    ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
  },
);

Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);

const buttonElement = document.getElementById("write-button");
const leftBox = document.getElementById("left-box");
const rightBox = document.getElementById("right-box");

rightBox.value = new TextDecoder().decode(
  new OpenFile(loxFile).fd_read(loxFile.size).data,
);

const encode = (str) => new TextEncoder().encode(str);
const decode = (str) => new TextDecoder().decode(str);

// Event listener for the button
buttonElement.addEventListener("click", async () => {
  const openFile = new OpenFile(loxFile);

  const leftBoxContent = leftBox.value;
  console.log("leftbox", leftBoxContent);
  const encoded = encode(leftBoxContent);

  console.log("fd_write", openFile.fd_write(encoded));
  // openFile.fd_close();

  const buffer = openFile.fd_read(loxFile.size).data;
  console.log("buffer after write", buffer);

  const content = decode(buffer);
  console.log("content after write", content);

  rightBox.value = content;

  console.log(`[WASI] Content written to ${loxFilename}:`, content);
  console.log("file", loxFile.data, decode(loxFile.data));
});

await instance.exports.main2();
console.log(await instance.exports.plus3(3));
