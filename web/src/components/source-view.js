import { LitElement, html, css, nothing } from "lit";
import { scopeColor, scopeHue } from "../lib/scope-colors.js";
import { ctxScopes, tokenizeSource, atomKind } from "../lib/sexpr.js";

// Foreground palette for token kinds.
const FG = {
  keyword: "hsl(215 65% 42%)",
  prim: "hsl(168 60% 30%)",
  number: "hsl(28 75% 40%)",
  symbol: "hsl(222 22% 26%)",
  paren: "hsl(222 15% 58%)",
  comment: "hsl(140 25% 45%)",
  string: "hsl(340 60% 42%)",
  ws: "inherit",
};

const sameSet = (a, b) =>
  a.length === b.length && a.every((x) => b.includes(x));

export class SsvSourceView extends LitElement {
  static properties = {
    src: { type: String },
    snapshot: { type: Array },
    prevSnapshot: { type: Array },
    resolve: { type: Array },
  };

  static styles = css`
    :host { display: block; font-family: inherit; }

    /* ---- scope legend ---- */
    .legend {
      display: flex; flex-wrap: wrap; align-items: center;
      gap: 4px 8px;
      margin-bottom: 0.45rem; padding-bottom: 0.35rem;
      border-bottom: 1px dashed hsl(40 25% 80%);
    }
    .legend-title {
      font-size: 0.6rem; letter-spacing: 0.1em; text-transform: uppercase;
      color: hsl(38 22% 55%);
    }
    .lgd {
      display: inline-flex; align-items: center; gap: 4px;
      font-size: 0.66rem; color: hsl(222 20% 36%);
      padding: 1px 7px 1px 5px; border-radius: 8px;
      background: hsl(40 35% 96%);
      border: 1px solid hsl(40 25% 84%);
      cursor: default;
      transition: transform 120ms ease, box-shadow 120ms ease;
    }
    .lgd:hover, .lgd.on {
      box-shadow: 0 0 0 2px var(--scp);
      transform: translateY(-1px);
    }
    .lgd .sw { width: 9px; height: 9px; border-radius: 2px; background: var(--scp); }

    /* ---- code ---- */
    .code {
      white-space: pre-wrap; word-break: break-word;
      font-size: 0.86rem; line-height: 1.8;
      color: hsl(222 22% 26%);
      padding: 0.2rem 0.1rem;
    }
    .seg { border-radius: 2px; }
    .kw { font-weight: 700; }

    .seg.tinted {
      background: var(--scp-bg);
      box-shadow: inset 0 -2px 0 var(--scp);
      transition: background-color 220ms ease, filter 160ms ease;
    }
    .seg.tinted:hover { filter: saturate(1.35) brightness(0.97); }

    /* changed at the current step */
    .seg.fresh {
      outline: 2px solid var(--fresh-c, hsl(220 20% 60%));
      outline-offset: -1px;
      animation: fresh-in 550ms ease-out;
    }
    @keyframes fresh-in { from { opacity: 0.15; } to { opacity: 1; } }

    /* hovered from the legend */
    .seg.focus {
      outline: 2px solid var(--scp);
      outline-offset: -1px;
      position: relative; z-index: 1;
    }

    /* resolve chip (final binding) */
    .chip {
      display: inline-block; vertical-align: middle;
      margin: 0 4px 0 1px; padding: 0 5px; border-radius: 7px;
      font-size: 0.62rem; line-height: 1.5; color: #fff; white-space: nowrap;
      transform: translateY(-1px);
      animation: pop-in 260ms cubic-bezier(0.34, 1.56, 0.64, 1);
      transition: transform 120ms ease;
    }
    .chip:hover { transform: translateY(-1px) scale(1.18); }

    /* step-delta tag (+added / -removed scope) */
    .dtag {
      display: inline-block; vertical-align: middle;
      margin: 0 3px; padding: 0 4px; border-radius: 6px;
      font-size: 0.6rem; line-height: 1.5; white-space: nowrap;
      transform: translateY(-2px);
      animation: pop-in 260ms cubic-bezier(0.34, 1.56, 0.64, 1);
    }
    .dtag.add {
      color: var(--scp); border: 1px solid var(--scp);
      background: hsl(0 0% 100% / 0.9); font-weight: 700;
    }
    .dtag.rem {
      color: hsl(220 10% 55%); border: 1px solid hsl(220 12% 76%);
      background: hsl(0 0% 100% / 0.6); text-decoration: line-through;
    }
    @keyframes pop-in {
      0% { opacity: 0; transform: translateY(-2px) scale(0.5); }
      100% { opacity: 1; transform: translateY(-2px) scale(1); }
    }

    .empty { color: #99a; font-style: italic; }
  `;

  constructor() {
    super();
    this.src = "";
    this.snapshot = null;
    this.prevSnapshot = null;
    this.resolve = null;
  }

  render() {
    if (!this.src) return html`<span class="empty">—</span>`;

    const tokens = tokenizeSource(this.src);
    const spans = this.snapshot ?? [];
    const prevSpans = this.prevSnapshot ?? [];
    const chips = this._chips();
    const tags = this._deltaTags(spans, prevSpans);

    const points = this._boundaries(tokens, spans, prevSpans, chips, tags);
    const segs = [];
    for (let k = 0; k < points.length - 1; k++) {
      const a = points[k];
      const b = points[k + 1];
      if (a >= b) continue;
      if (tags.has(a)) segs.push(...tags.get(a));
      segs.push(this._segment(this.src.slice(a, b), tokens, spans, prevSpans, a, b));
      if (chips.has(b)) segs.push(...chips.get(b));
    }

    return html`${this._legend(spans)}<div class="code">${segs}</div>`;
  }

  // ---- legend ----------------------------------------------------------

  _legendScopes(spans) {
    const set = new Set();
    for (const [, , ctx] of spans) for (const s of ctxScopes(ctx)) set.add(s);
    return [...set].sort();
  }

  _legend(spans) {
    const scopes = this._legendScopes(spans);
    if (!scopes.length) return nothing;
    return html`<div class="legend">
      <span class="legend-title">scopes</span>
      ${scopes.map(
        (s) => html`<span
          class="lgd"
          data-scope=${s}
          style="--scp:${scopeColor(s)}"
          @mouseenter=${() => this._setFocus(s)}
          @mouseleave=${() => this._setFocus(null)}>
          <i class="sw"></i>${s}</span>`,
      )}
    </div>`;
  }

  // Toggle the "focus" highlight directly on the DOM so hovering the legend
  // does not re-render (which would replay the step-change animation).
  _setFocus(scope) {
    const root = this.renderRoot;
    for (const el of root.querySelectorAll(".seg.tinted")) {
      const scs = (el.dataset.scopes || "").split(" ").filter(Boolean);
      el.classList.toggle("focus", scope != null && scs.includes(scope));
    }
    for (const el of root.querySelectorAll(".lgd"))
      el.classList.toggle("on", scope != null && el.dataset.scope === scope);
  }

  // ---- resolve chips (final binding) -----------------------------------

  _chips() {
    const map = new Map();
    const seen = new Set();
    for (const [start, end, name] of this.resolve ?? []) {
      const key = end + ":" + name;
      if (seen.has(key)) continue;
      seen.add(key);
      if (!map.has(end)) map.set(end, []);
      map.get(end).push(
        html`<span class="chip" style="background:${scopeColor(name)}"
                   title="resolves to ${name}">→ ${name}</span>`,
      );
    }
    return map;
  }

  // ---- step delta (+added / -removed scope tags) -----------------------

  _deltaTags(spans, prevSpans) {
    const key = (s, e) => s + "," + e;
    const prevMap = new Map();
    for (const [s, e, ctx] of prevSpans) prevMap.set(key(s, e), ctxScopes(ctx));
    const curMap = new Map();
    for (const [s, e, ctx] of spans) curMap.set(key(s, e), ctxScopes(ctx));

    const changed = [];
    for (const [s, e, ctx] of spans) {
      const now = ctxScopes(ctx);
      const before = prevMap.get(key(s, e)) ?? [];
      const added = now.filter((x) => !before.includes(x));
      const removed = before.filter((x) => !now.includes(x));
      if (added.length || removed.length) changed.push({ s, e, added, removed });
    }
    for (const [s, e, ctx] of prevSpans) {
      if (!curMap.has(key(s, e)) && ctxScopes(ctx).length)
        changed.push({ s, e, added: [], removed: ctxScopes(ctx) });
    }

    // Tag only the topmost changed span of each region, not every nested node.
    const topmost = changed.filter(
      (c) =>
        !changed.some(
          (o) =>
            o !== c &&
            o.s <= c.s && c.e <= o.e &&
            (o.s < c.s || c.e < o.e),
        ),
    );

    const map = new Map();
    for (const c of topmost) {
      const tags = [
        ...c.added.map(
          (sc) => html`<span class="dtag add" style="--scp:${scopeColor(sc)}">+${sc}</span>`,
        ),
        ...c.removed.map(
          (sc) => html`<span class="dtag rem" style="--scp:${scopeColor(sc)}">−${sc}</span>`,
        ),
      ];
      if (!tags.length) continue;
      if (!map.has(c.s)) map.set(c.s, []);
      map.get(c.s).push(...tags);
    }
    return map;
  }

  // ---- segment rendering -----------------------------------------------

  _boundaries(tokens, spans, prevSpans, chips, tags) {
    const set = new Set([0, this.src.length]);
    for (const t of tokens) { set.add(t.start); set.add(t.end); }
    for (const [s, e] of spans) { set.add(s); set.add(e); }
    for (const [s, e] of prevSpans) { set.add(s); set.add(e); }
    for (const pos of chips.keys()) set.add(pos);
    for (const pos of tags.keys()) set.add(pos);
    return [...set].sort((x, y) => x - y);
  }

  // The most specific span covering [a, b) supplies its scope set.
  _ctxAt(spans, a, b) {
    let best = null;
    let bestLen = Infinity;
    for (const [s, e, ctx] of spans) {
      if (s <= a && b <= e && e - s < bestLen) {
        best = ctx;
        bestLen = e - s;
      }
    }
    return best ? ctxScopes(best) : [];
  }

  _tokenAt(tokens, pos) {
    for (const t of tokens) if (t.start <= pos && pos < t.end) return t;
    return null;
  }

  _segment(text, tokens, spans, prevSpans, a, b) {
    const tok = this._tokenAt(tokens, a);
    const kind = tok ? (tok.kind === "atom" ? atomKind(tok.text) : tok.kind) : "ws";
    const scopesNow = this._ctxAt(spans, a, b);
    const scopesPrev = this._ctxAt(prevSpans, a, b);

    let cls = "seg";
    if (kind === "keyword" || kind === "prim") cls += " kw";

    const style = [`color:${FG[kind] ?? FG.ws}`];
    let title = nothing;
    if (scopesNow.length) {
      cls += " tinted";
      const hue = scopeHue(scopesNow[0]);
      style.push(`--scp:${scopeColor(scopesNow[0])}`);
      style.push(`--scp-bg:hsl(${hue} 70% 88%)`);
      title = `scopes: ${scopesNow.join(" ")}`;
    }

    if (!sameSet(scopesNow, scopesPrev)) {
      cls += " fresh";
      const delta =
        scopesNow.find((x) => !scopesPrev.includes(x)) ??
        scopesPrev.find((x) => !scopesNow.includes(x));
      if (delta) style.push(`--fresh-c:${scopeColor(delta)}`);
    }

    return html`<span class=${cls} style=${style.join(";")} title=${title}
                      data-scopes=${scopesNow.length ? scopesNow.join(" ") : nothing}>${text}</span>`;
  }
}

customElements.define("ssv-source-view", SsvSourceView);
