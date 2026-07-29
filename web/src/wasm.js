// Bridge to the Scheme/WASM expander.  reflect.js (loaded as a classic script)
// provides the global `Scheme`; the main module resolves to the `run-model`
// procedure, which we call repeatedly.

let runModelProc = null;

export async function init() {
  const loaded = await Scheme.load_main("app.wasm", { reflect_wasm_dir: "." });
  runModelProc = loaded[0];
}

// Expand INPUT-SRC under MODEL ("core" | "phases" | "local" | "defs"); return the parsed trace.
export function runModel(model, inputSrc) {
  const [result] = runModelProc.call(model, inputSrc);
  return JSON.parse(result.reflector.string_value(result));
}
