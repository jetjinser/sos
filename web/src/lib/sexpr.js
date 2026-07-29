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
