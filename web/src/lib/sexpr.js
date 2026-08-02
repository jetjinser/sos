export function ctxScopes(ctx) {
  if (!ctx || !ctx.length) return [];
  if (typeof ctx[0] === "string") return ctx;
  return ctx.flatMap(([, scps]) => scps);
}

export function stxLabel(stx) {
  if (!stx || typeof stx !== "object") return String(stx ?? "");
  const form = stx.form;
  if (Array.isArray(form)) return "(" + form.length + ")";
  return String(form);
}

const isWs = (c) => c === " " || c === "\t" || c === "\n" || c === "\r";
const isDelim = (c) => c === "(" || c === ")";

// Split source text into renderable tokens, each carrying its [start, end)
// offsets so decorations can be aligned back onto the original string.
export function tokenizeSource(src) {
  const tokens = [];
  const n = src.length;
  let i = 0;
  while (i < n) {
    const c = src[i];
    if (isWs(c)) {
      let j = i;
      while (j < n && isWs(src[j])) j++;
      tokens.push({ start: i, end: j, text: src.slice(i, j), kind: "ws" });
      i = j;
    } else if (c === ";") {
      let j = i;
      while (j < n && src[j] !== "\n") j++;
      tokens.push({ start: i, end: j, text: src.slice(i, j), kind: "comment" });
      i = j;
    } else if (c === '"') {
      let j = i + 1;
      while (j < n && src[j] !== '"') {
        if (src[j] === "\\") j++;
        j++;
      }
      j = Math.min(j + 1, n);
      tokens.push({ start: i, end: j, text: src.slice(i, j), kind: "string" });
      i = j;
    } else if (isDelim(c)) {
      tokens.push({ start: i, end: i + 1, text: c, kind: "paren" });
      i++;
    } else {
      let j = i;
      while (j < n && !isWs(src[j]) && !isDelim(src[j]) && src[j] !== '"' && src[j] !== ";") j++;
      tokens.push({ start: i, end: j, text: src.slice(i, j), kind: "atom" });
      i = j;
    }
  }
  return tokens;
}

const KEYWORDS = new Set(["let-syntax", "lambda", "quote", "syntax"]);
const PRIMS = new Set([
  "syntax->datum", "datum->syntax", "+", "-", "CONS", "CAR", "CDR", "LIST",
  "LOCAL-VALUE", "LOCAL-EXPAND", "LOCAL-BINDER",
  "BOX", "UNBOX", "SET-BOX!", "NEW-DEFS", "DEF-BIND",
]);

export function atomKind(text) {
  if (KEYWORDS.has(text)) return "keyword";
  if (PRIMS.has(text)) return "prim";
  if (/^[+-]?\d+(\.\d+)?$/.test(text)) return "number";
  return "symbol";
}
