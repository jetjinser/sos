import { LitElement, html, css, nothing } from "lit";
import { keyed } from "lit/directives/keyed.js";
import { scopeColor, scopeBg } from "../lib/scope-colors.js";
import "./ast-view.js";

const RULE_HUES = {
  lambda: 212, "let-syntax": 248,
  "macro-invoke": 282,
  app: 198, "fun-app": 198, "prim-app": 188,
  id: 168,
  quote: 222, syntax: 262, literal: 222, stops: 222, value: 222,
  "LOCAL-VALUE": 158, "LOCAL-EXPAND": 302, "LOCAL-BINDER": 318,
  "NEW-DEFS": 338, "DEF-BIND": 352,
  BOX: 18, UNBOX: 32, "SET-BOX!": 4,
};

const OP_HUES = {
  "stx-add": 145, "stx-flip": 25, "stx-prune": 335,
  bind: 262, "alloc-name": 185, "alloc-scope": 85,
};

// Current-step inspector: the rule/op badge with its info entries, sitting
// directly above the rewrite it describes (rule before -> after, or the op's
// scope/binding payload).  One pane so step identity and step content are
// never separated.
export class SsvStepDetail extends LitElement {
  static properties = {
    step: { type: Object },
    index: { type: Number },
  };

  static styles = css`
    :host { display: block; font-family: inherit; font-size: 0.82rem; }

    /* ---- head: badge, with info chips always on their own row below ---- */
    .head {
      display: flex; flex-direction: column; align-items: flex-start;
      gap: 0.4rem;
      margin-bottom: 0.55rem;
    }
    .chips { display: flex; flex-wrap: wrap; gap: 0.4rem; }
    /* Category anchored by --cat (rule cool / op warm); per-step kind hue
       injected as --h; all three colors derived via color-mix */
    .badge {
      --h: 222;
      display: inline-flex; align-items: stretch;
      border-radius: 3px; overflow: hidden;
      font-weight: bold;
      animation: badge-in 180ms ease-out;
    }
    .badge.rule { --cat: #246; }
    .badge.op   { --cat: #642; }
    .badge .kind {
      display: inline-flex; align-items: center;
      font-size: 0.62rem; font-weight: 700;
      letter-spacing: 0.07em; text-transform: uppercase;
      padding: 0 0.4rem;
      color: #fff;
      background: color-mix(in oklab, var(--cat) 80%, hsl(var(--h) 75% 42%));
    }
    .badge .name {
      padding: 0.14rem 0.5rem;
      background: color-mix(in oklab, hsl(var(--h) 90% 95%) 80%, var(--cat));
      color: color-mix(in oklab, var(--cat) 42%, hsl(var(--h) 65% 28%));
    }
    .kv {
      display: inline-flex; align-items: baseline; gap: 0.4em;
      font-size: 0.72rem;
      padding: 0.16rem 0.5rem;
      border: 1px solid hsl(220 15% 88%);
      border-radius: 3px;
      background: hsl(220 25% 97% / 0.8);
      animation: chip-in 200ms ease-out backwards;
    }
    .kv:nth-child(2) { animation-delay: 30ms; }
    .kv:nth-child(3) { animation-delay: 60ms; }
    .kv:nth-child(n+4) { animation-delay: 90ms; }
    .kv .k {
      font-size: 0.58rem; font-weight: 700;
      letter-spacing: 0.08em; text-transform: uppercase;
      color: hsl(220 12% 55%);
    }
    .kv .v { font-weight: 600; color: hsl(220 30% 30%); }

    /* ---- rewrite panes ---- */
    .cap {
      display: block;
      font-size: 0.58rem; font-weight: 700;
      letter-spacing: 0.1em; text-transform: uppercase;
      color: hsl(220 12% 58%);
      margin-bottom: 0.25rem;
    }
    .pane {
      padding: 0.35rem 0.45rem;
      background: hsl(220 30% 98% / 0.7);
      border: 1px solid hsl(220 18% 88%);
      border-radius: 6px;
      overflow-x: auto;
      animation: pane-in 240ms ease-out backwards;
    }
    .pane.before { animation-delay: 80ms; }
    .pane.after  { animation-delay: 180ms; }
    .arrow {
      text-align: center;
      color: hsl(220 15% 62%);
      font-size: 0.9rem; line-height: 1.3;
      animation: arrow-in 240ms ease-out backwards;
      animation-delay: 130ms;
    }

    /* ---- op payload ---- */
    .row {
      display: flex; align-items: center; gap: 0.35rem;
      flex-wrap: wrap;
      animation: pane-in 240ms ease-out backwards;
      animation-delay: 80ms;
    }
    .scp {
      display: inline-block;
      font-size: 0.68rem; padding: 0 4px;
      border-radius: 3px; color: #fff; white-space: nowrap;
    }
    .fresh {
      display: inline-block;
      font-weight: 700;
      padding: 0.05rem 0.4rem; border-radius: 4px;
      animation: pop 260ms cubic-bezier(0.34, 1.56, 0.64, 1);
    }
    .opname { color: hsl(220 15% 45%); }

    @keyframes badge-in {
      from { opacity: 0.2; transform: translateX(-4px); }
      to   { opacity: 1;    transform: none; }
    }
    @keyframes chip-in {
      from { opacity: 0; transform: translateY(3px); }
      to   { opacity: 1; transform: none; }
    }
    @keyframes pane-in {
      from { opacity: 0; transform: translateY(4px); }
      to   { opacity: 1; transform: none; }
    }
    @keyframes arrow-in {
      from { opacity: 0; transform: translateY(-3px); }
      to   { opacity: 1; transform: none; }
    }
    @keyframes pop {
      from { opacity: 0; transform: scale(0.6); }
      to   { opacity: 1; transform: scale(1); }
    }
    .empty { color: #99a; font-style: italic; }
  `;

  constructor() {
    super();
    this.step = null;
    this.index = 0;
  }

  render() {
    const step = this.step;
    if (!step) return html`<span class="empty">—</span>`;
    return keyed(this.index, html`
      <div class="head">
        ${this._badge(step)}
        ${step.type === "rule" && Object.keys(step.info ?? {}).length
          ? html`<div class="chips">${this._info(step)}</div>`
          : nothing}
      </div>
      ${this._detail(step)}
    `);
  }

  _badge(step) {
    if (step.type === "rule") {
      return html`
        <span class="badge rule"
              style="--h: ${RULE_HUES[step.rule] ?? 222}">
          <span class="kind">rule</span><span class="name">${step.rule}</span>
        </span>`;
    }
    if (step.type === "op") {
      return html`
        <span class="badge op" style="--h: ${OP_HUES[step.op] ?? 222}">
          <span class="kind">op</span><span class="name">${step.op}</span>
        </span>`;
    }
    return nothing;
  }

  _info(step) {
    return Object.entries(step.info ?? {}).map(([k, v]) => html`
      <span class="kv">
        <span class="k">${k}</span>
        <span class="v">${typeof v === "object" ? JSON.stringify(v) : v}</span>
      </span>`);
  }

  _detail(step) {
    if (step.type === "rule") return this._rewrite(step.before, step.after);
    if (step.type === "op") return this._op(step);
    return nothing;
  }

  _rewrite(before, after) {
    return html`
      <div class="pane before">
        <span class="cap">before</span>
        <ssv-ast-view .ast=${before}></ssv-ast-view>
      </div>
      <div class="arrow">↓</div>
      <div class="pane after">
        <span class="cap">after</span>
        <ssv-ast-view .ast=${after}></ssv-ast-view>
      </div>`;
  }

  _scpChips(scopes) {
    return (Array.isArray(scopes) ? scopes : [scopes]).map(
      (s) => html`<span class="scp" style="background:${scopeColor(s)}">${s}</span>`,
    );
  }

  _fresh(name) {
    return html`<span class="fresh"
      style="color:${scopeColor(name)};background:${scopeBg(name)}">${name}</span>`;
  }

  _op(step) {
    const d = step.data ?? [];
    switch (step.op) {
      case "stx-add": case "stx-flip": case "stx-prune":
        return html`
          <div class="row">${this._scpChips(d[0])}</div>
          ${this._rewrite(d[1], d[2])}`;
      case "bind":
        return html`
          <div class="row">
            <span class="opname">${d[0]}</span>
            ${this._scpChips(d[1])}
            <span class="opname">→</span>
            ${this._fresh(d[2])}
          </div>`;
      case "alloc-name": case "alloc-scope":
        return html`<div class="row">${this._fresh(d[0])}</div>`;
      default:
        return html`<div class="row"><code>${JSON.stringify(d)}</code></div>`;
    }
  }
}

customElements.define("ssv-step-detail", SsvStepDetail);
