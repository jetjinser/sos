// Bridge to the Scheme/WASM expander.  reflect.js (loaded as a classic script)
// provides the global `Scheme`; the main module resolves to two procedures:
// run-model (trace JSON) and format-src (pretty-printer).

let runModelProc = null;
let formatProc = null;

export async function init() {
  const loaded = await Scheme.load_main("app.wasm", { reflect_wasm_dir: "." });
  runModelProc = loaded[0];
  formatProc = loaded[1];
}

// Scheme exceptions surface as SchemeTrapError: an empty .message but a
// reflective .data payload. Render it readably so the UI can show it.
function rethrow(err) {
  if (err && err.data !== undefined && typeof repr === "function") {
    throw new Error(repr(err.data));
  }
  throw err;
}

// Expand INPUT-SRC under MODEL ("core" | "phases" | "local" | "defs"); return the parsed trace.
export function runModel(model, inputSrc) {
  try {
    const [result] = runModelProc.call(model, inputSrc);
    return JSON.parse(result.reflector.string_value(result));
  } catch (err) {
    rethrow(err);
  }
}

// Pretty-print INPUT-SRC; return the formatted string.
export function formatSource(inputSrc) {
  try {
    const [result] = formatProc.call(inputSrc);
    return result.reflector.string_value(result);
  } catch (err) {
    rethrow(err);
  }
}
