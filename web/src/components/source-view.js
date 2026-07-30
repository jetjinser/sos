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

export class SsvSourceView extends LitElement {
  static properties = {
    src: { type: String },
    snapshot: { type: Array },
    resolve: { type: Array },
  };

  static styles = css`
    :host { display: block; font-family: inherit; }
    .code {
      white-space: pre-wrap;
      word-break: break-word;
      font-size: 0.86rem;
      line-height: 1.75;
      color: hsl(222 22% 26%);
      padding: 0.2rem 0.1rem;
    }
    .seg { border-radius: 2px; }
    .kw { font-weight: 700; }

    /* Scope-tinted segments carry their colors as custom properties so the
       stylesheet can react on hover. */
    .seg.tinted {
      background: var(--scp-bg);
      box-shadow: inset 0 -2px 0 var(--scp);
      transition: background-color 220ms ease, filter 160ms ease;
    }
    .seg.tinted:hover { filter: saturate(1.35) brightness(0.97); }

    .chip {
      display: inline-block;
      vertical-align: middle;
      margin: 0 4px 0 1px;
      padding: 0 5px;
      border-radius: 7px;
      font-size: 0.62rem;
      line-height: 1.5;
      color: #fff;
      white-space: nowrap;
      transform: translateY(-1px);
      animation: chip-in 260ms cubic-bezier(0.34, 1.56, 0.64, 1);
      transition: transform 120ms ease;
    }
    .chip:hover { transform: translateY(-1px) scale(1.18); }
    @keyframes chip-in {
      0% { opacity: 0; transform: translateY(-1px) scale(0.5); }
      100% { opacity: 1; transform: translateY(-1px) scale(1); }
    }
    .empty { color: #99a; font-style: italic; }
  `;

  constructor() {
    super();
    this.src = "";
    this.snapshot = null;
    this.resolve = null;
  }

  render() {
    if (!this.src) return html`<span class="empty">—</span>`;

    const tokens = tokenizeSource(this.src);
    const spans = this.snapshot ?? [];
    const chips = this._chips();

    const points = this._boundaries(tokens, spans, chips);
    const segs = [];
    for (let k = 0; k < points.length - 1; k++) {
      const a = points[k];
      const b = points[k + 1];
      if (a >= b) continue;
      segs.push(this._segment(this.src.slice(a, b), tokens, spans, a, b));
      if (chips.has(b)) segs.push(...chips.get(b));
    }
    return html`<div class="code">${segs}</div>`;
  }

  // De-duplicated resolve chips keyed by their insertion offset.
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

  _boundaries(tokens, spans, chips) {
    const set = new Set([0, this.src.length]);
    for (const t of tokens) { set.add(t.start); set.add(t.end); }
    for (const [s, e] of spans) { set.add(s); set.add(e); }
    for (const pos of chips.keys()) set.add(pos);
    return [...set].sort((x, y) => x - y);
  }

  // The most specific snapshot span covering [a, b) supplies its scope set.
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
    for (const t of tokens) {
      if (t.start <= pos && pos < t.end) return t;
    }
    return null;
  }

  _segment(text, tokens, spans, a, b) {
    const tok = this._tokenAt(tokens, a);
    const kind = tok ? (tok.kind === "atom" ? atomKind(tok.text) : tok.kind) : "ws";
    const scopes = this._ctxAt(spans, a, b);

    let cls = "seg";
    if (kind === "keyword" || kind === "prim") cls += " kw";

    const style = [`color:${FG[kind] ?? FG.ws}`];
    let title = nothing;
    if (scopes.length) {
      cls += " tinted";
      const hue = scopeHue(scopes[0]);
      style.push(`--scp:${scopeColor(scopes[0])}`);
      style.push(`--scp-bg:hsl(${hue} 70% 88%)`);
      title = `scopes: ${scopes.join(" ")}`;
    }

    return html`<span class=${cls} style=${style.join(";")} title=${title}>${text}</span>`;
  }
}

customElements.define("ssv-source-view", SsvSourceView);
