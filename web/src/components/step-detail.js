import { LitElement, html, css, nothing } from "lit";
import { keyed } from "lit/directives/keyed.js";
import { scopeColor, scopeBg } from "../lib/scope-colors.js";
import "./ast-view.js";

// Per-step inspector pane: rule steps show the before -> after rewrite,
// op steps show their scope/binding payload.  Everything renders through
// ssv-ast-view, which handles both stx objects and raw AST values.
export class SsvStepDetail extends LitElement {
  static properties = {
    step: { type: Object },
    index: { type: Number },
  };

  static styles = css`
    :host { display: block; font-family: inherit; font-size: 0.78rem; }
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
    .pane.after { animation-delay: 90ms; }
    .arrow {
      text-align: center;
      color: hsl(220 15% 62%);
      font-size: 0.9rem; line-height: 1.3;
      animation: arrow-in 240ms ease-out backwards;
      animation-delay: 45ms;
    }
    @keyframes pane-in {
      from { opacity: 0; transform: translateY(4px); }
      to   { opacity: 1; transform: none; }
    }
    @keyframes arrow-in {
      from { opacity: 0; transform: translateY(-3px); }
      to   { opacity: 1; transform: none; }
    }
    .row {
      display: flex; align-items: center; gap: 0.35rem;
      flex-wrap: wrap;
      margin-bottom: 0.3rem;
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
    @keyframes pop {
      from { opacity: 0; transform: scale(0.6); }
      to   { opacity: 1; transform: scale(1); }
    }
    .opname { color: hsl(220 15% 45%); }
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
    return keyed(this.index, this._detail(step));
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
