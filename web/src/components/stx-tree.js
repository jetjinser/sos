import { LitElement, html, css } from "lit";
import { scopeColor } from "../lib/scope-colors.js";
import { ctxScopes } from "../lib/sexpr.js";

export class SsvStxTree extends LitElement {
  static properties = {
    stx: { type: Object },
  };

  static styles = css`
    :host { display: block; font-family: inherit; font-size: 0.82rem; }
    .node {
      border: 1px solid #ccd; border-radius: 4px;
      padding: 2px 4px; margin: 2px 0;
      display: inline-block; vertical-align: top;
      background: hsl(0 0% 100% / 0.6);
      transition: border-color 120ms ease, box-shadow 120ms ease,
                  transform 120ms ease;
    }
    .node:hover {
      border-color: hsl(220 40% 60%);
      box-shadow: 0 1px 5px hsl(220 40% 30% / 0.18);
      transform: translateY(-1px);
    }
    .compound { border-color: #99a; }
    .header { display: flex; align-items: center; gap: 3px; flex-wrap: wrap; }
    .form-atom { font-weight: bold; }
    .form-tag { color: #889; font-size: 0.75rem; }
    .children {
      display: flex; flex-wrap: wrap; gap: 2px;
      padding-left: 10px; margin-top: 2px;
      border-left: 2px solid hsl(220 20% 88%);
    }
    .scp {
      font-size: 0.68rem; padding: 0 3px; border-radius: 3px;
      color: #fff; white-space: nowrap;
      transition: transform 120ms ease;
    }
    .scp:hover { transform: scale(1.15); }
    .empty { color: #99a; font-style: italic; }
  `;

  constructor() {
    super();
    this.stx = null;
  }

  render() {
    if (!this.stx) return html`<span class="empty">—</span>`;
    return this._node(this.stx);
  }

  _node(stx) {
    if (!stx || typeof stx !== "object") {
      return html`<span class="node">${String(stx ?? "·")}</span>`;
    }
    const form = stx.form;
    const scopes = ctxScopes(stx.ctx);
    const badges = scopes.map(
      (s) => html`<span class="scp" style="background:${scopeColor(s)}"
                        title=${s}>${s}</span>`,
    );

    if (Array.isArray(form)) {
      return html`
        <div class="node compound">
          <div class="header">
            <span class="form-tag">(${form.length})</span>
            ${badges}
          </div>
          <div class="children">
            ${form.map((child) => this._node(child))}
          </div>
        </div>`;
    }

    return html`
      <span class="node" style="border-color:${scopes.length ? scopeColor(scopes[0]) : "#ccc"}">
        <span class="form-atom">${String(form)}</span>
        ${badges}
      </span>`;
  }
}

customElements.define("ssv-stx-tree", SsvStxTree);
