// Structural diff of two syntax objects (stx trees or raw AST lists).
// Nodes are aligned positionally (child i against child i), which is exact
// for structure-preserving rewrites (lambda, app, quote...) and degrades to
// "everything changed" for whole-term rewrites (macro-invoke) — both read
// correctly.  Returns the set of root-to-node paths that are new/changed in
// `after` (added) and gone from `before` (removed); the renderer highlights
// those paths.  A path is "" for the root, "0", "1"... then "0.2" etc.

const formOf = (x) =>
  x && typeof x === "object" && !Array.isArray(x) && "form" in x ? x.form : x;

const isCompound = (x) => Array.isArray(formOf(x));

// Raw ctx (scope set or phase map) compared structurally.
const ctxKey = (x) =>
  JSON.stringify(
    x && typeof x === "object" && !Array.isArray(x) && "ctx" in x ? x.ctx : [],
  );

const atomEqual = (a, b) => formOf(a) === formOf(b) && ctxKey(a) === ctxKey(b);

const childPath = (p, i) => (p === "" ? `${i}` : `${p}.${i}`);

export function diffStx(before, after) {
  const added = new Set();
  const removed = new Set();
  const walk = (b, a, path) => {
    if (isCompound(b) && isCompound(a)) {
      if (ctxKey(b) !== ctxKey(a)) {
        added.add(path);
        removed.add(path);
      }
      const bf = formOf(b);
      const af = formOf(a);
      const n = Math.max(bf.length, af.length);
      for (let i = 0; i < n; i++) {
        const cp = childPath(path, i);
        if (i >= bf.length) added.add(cp);
        else if (i >= af.length) removed.add(cp);
        else walk(bf[i], af[i], cp);
      }
    } else if (!atomEqual(a, b)) {
      added.add(path);
      removed.add(path);
    }
  };
  if (before != null && after != null) walk(before, after, "");
  return { added, removed };
}
