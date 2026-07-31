// Feature probes for the WebAssembly proposals the app requires.  Each entry
// is a minimal module that only validates when the engine supports the
// feature, so WebAssembly.validate doubles as a capability check.  Byte
// sequences are generated with wabt (wat2wasm) from one-construct WAT files.

const PROBES = [
  { name: "Wasm GC", short: "GC",
    bytes: [0,97,115,109,1,0,0,0,1,5,1,95,1,127,0] },
  { name: "Tail call", short: "tail-call",
    bytes: [0,97,115,109,1,0,0,0,1,5,1,96,0,1,127,3,3,2,0,0,10,11,2,4,0,18,1,11,4,0,65,42,11] },
  { name: "Exception handling", short: "exceptions",
    bytes: [0,97,115,109,1,0,0,0,1,5,1,96,1,127,0,13,3,1,0,0] },
  { name: "Extended const", short: "ext-const",
    bytes: [0,97,115,109,1,0,0,0,6,9,1,127,0,65,1,65,2,106,11] },
  { name: "Reference types", short: "ref-types",
    bytes: [0,97,115,109,1,0,0,0,1,5,1,96,0,1,112,3,2,1,0,10,6,1,4,0,208,112,11] },
];

// [{ name, short, supported }] for every required feature.
export function detectFeatures() {
  return PROBES.map(({ name, short, bytes }) => ({
    name,
    short,
    supported: WebAssembly.validate(new Uint8Array(bytes)),
  }));
}

// Full names of the features this engine lacks.
export function missingFeatures(features) {
  return features.filter((f) => !f.supported).map((f) => f.name);
}
