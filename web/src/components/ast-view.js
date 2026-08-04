import { LitElement, html, css, nothing } from "lit";
import { scopeColor } from "../lib/scope-colors.js";
import { ctxScopes } from "../lib/sexpr.js";

const KEYWORDS = new Set([
  "fun", "var", "app", "list-val",
  "lambda", "quote", "syntax", "let-syntax", "syntax-rules", "syntax-case",
  "syntax->datum", "datum->syntax",
  "bound-identifier=?", "free-identifier=?", "generate-temporaries",
  "if", "stx-len", "=",
  "+", "-", "CONS", "CAR", "CDR", "LIST",
  "LOCAL-VALUE", "LOCAL-EXPAND", "LOCAL-BINDER",
  "BOX", "UNBOX", "SET-BOX!", "NEW-DEFS", "DEF-BIND",
]);

const FRESH = /^[A-Za-z_][\w!?*-]*:\d+$/;

export class SsvAstView extends LitElement {
  static properties = {
    ast: { type: Object },
  };

  static styles = css`
    :host { display: block; font-family: inherit; }
    .sexpr {
      white-space: pre;
      font-size: 0.85rem;
      line-height: 1.55;
      color: hsl(220 20% 25%);
    }
    .punc { color: hsl(220 15% 62%); }
    .kw {
      color: hsl(221 55% 42%);
      font-weight: 700;
      letter-spacing: 0.01em;
    }
    .fresh { font-weight: 700; }
    .num { color: hsl(160 55% 32%); }
    .bool { color: hsl(280 45% 45%); }
    .sym { color: hsl(220 15% 35%); }
    .nil { color: #aab; }
    .stx {
      display: inline-block;
      background: hsl(260 45% 95%);
      border: 1px solid hsl(260 35% 80%);
      border-radius: 4px;
      padding: 0 4px;
      color: hsl(260 40% 38%);
      transition: box-shadow 120ms ease, transform 120ms ease;
    }
    .stx:hover {
      box-shadow: 0 1px 5px hsl(260 40% 40% / 0.25);
      transform: translateY(-1px);
    }
    .stx-tag { color: hsl(260 30% 60%); font-size: 0.75em; }
    .empty { color: #99a; font-style: italic; }
  `;

  constructor() {
    super();
    this.ast = null;
  }

  render() {
    if (this.ast == null) return html`<span class="empty">—</span>`;
    return html`<div class="sexpr">${this._node(this.ast, 0)}</div>`;
  }

  _node(n, d) {
    if (n == null) return html`<span class="nil">·</span>`;
    if (typeof n === "number") return html`<span class="num">${n}</span>`;
    if (typeof n === "boolean") return html`<span class="bool">${String(n)}</span>`;
    if (typeof n === "string") return this._sym(n);
    if (Array.isArray(n)) return this._list(n, d);
    if (typeof n === "object" && "form" in n && "ctx" in n) return this._stx(n);
    return html`<span>${String(n)}</span>`;
  }

  _sym(s) {
    if (FRESH.test(s)) {
      return html`<span class="fresh" style="color:${scopeColor(s)}">${s}</span>`;
    }
    if (KEYWORDS.has(s)) return html`<span class="kw">${s}</span>`;
    return html`<span class="sym">${s}</span>`;
  }

  _isSimple(n) {
    return n == null || typeof n !== "object";
  }

  _list(arr, d) {
    if (arr.length === 0) return html`<span class="punc">()</span>`;
    if (arr.every((x) => this._isSimple(x))) {
      return html`<span class="punc">(</span>${arr.map((x, i) =>
        html`${i > 0 ? " " : nothing}${this._node(x, d + 1)}`)}<span class="punc">)</span>`;
    }
    const [head, ...rest] = arr;
    const indent = "\n" + "  ".repeat(d + 1);
    return html`<span class="punc">(</span>${this._node(head, d + 1)}${rest.map((x) =>
      html`${indent}${this._node(x, d + 1)}`)}<span class="punc">)</span>`;
  }

  _stx(s) {
    const scopes = ctxScopes(s.ctx);
    const title = scopes.length ? `ctx: ${scopes.join(" ")}` : "ctx: ∅";
    return html`<span class="stx" title=${title}><span class="stx-tag">⟨</span>${this._node(s.form, 0)}<span class="stx-tag">⟩</span></span>`;
  }
}

customElements.define("ssv-ast-view", SsvAstView);
