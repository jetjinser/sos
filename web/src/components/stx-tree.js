import { LitElement, html, css } from "lit";
import { scopeColor } from "../lib/scope-colors.js";
import { ctxScopes } from "../lib/sexpr.js";

export class SsvStxTree extends LitElement {
  static properties = {
    stx: { type: Object },
    // { paths: Set<string>, kind: "added" | "removed" } — nodes whose
    // positional path is in the set are highlighted as part of a diff.
    diff: { type: Object },
  };

  static styles = css`
    :host { display: block; font-family: inherit; font-size: 0.82rem; }
    .node {
      border: 1px solid #ccd; border-radius: 4px;
      padding: 2px 4px; margin: 2px 0;
      display: inline-block; vertical-align: top;
      background: hsl(0 0% 100% / 0.6);
      transition: border-color 120ms ease, box-shadow 120ms ease,
                  transform 120ms ease, background-color 120ms ease;
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
    /* Diff highlight: inset ring (not border, so a leaf's inline scope colour
       can't override it) plus a tinted background */
    .node.diff-added {
      background: hsl(145 60% 93%);
      box-shadow: inset 0 0 0 1.5px hsl(145 50% 44%);
    }
    .node.diff-removed {
      background: hsl(4 72% 95%);
      box-shadow: inset 0 0 0 1.5px hsl(4 58% 58%);
    }
    /* While a diff is shown, unchanged nodes lose their box entirely and
       recede to grey text on indentation rails — boxes are reserved for the
       changed nodes, so nesting reads as rails, not boxes-in-boxes, and the
       rewrite stands out as figure against ground.  Direct-child selectors
       keep the muting from leaking into changed subtrees nested inside. */
    .node.ctx-node {
      background: transparent;
      border-color: transparent;
      box-shadow: none;
    }
    .node.ctx-node > .form-atom { color: hsl(220 10% 52%); font-weight: 500; }
    .node.ctx-node > .scp,
    .node.ctx-node > .header > .scp { opacity: 0.3; }
    .node.ctx-node > .header > .form-tag { color: hsl(220 12% 68%); }
    .node.ctx-node > .children { border-left-color: hsl(220 20% 80%); }
    .empty { color: #99a; font-style: italic; }
  `;

  constructor() {
    super();
    this.stx = null;
    this.diff = null;
  }

  render() {
    if (!this.stx) return html`<span class="empty">—</span>`;
    return this._node(this.stx, "");
  }

  _diffCls(path) {
    if (!this.diff) return "";
    return this.diff.paths.has(path) ? ` diff-${this.diff.kind}` : " ctx-node";
  }

  _childPath(path, i) {
    return path === "" ? `${i}` : `${path}.${i}`;
  }

  _node(x, path) {
    const dc = this._diffCls(path);
    if (x == null || typeof x !== "object") {
      return html`<span class="node leaf${dc}">${String(x ?? "·")}</span>`;
    }
    // Raw AST list (plain array, no ctx) — local/defs steps carry these
    if (Array.isArray(x)) {
      return html`
        <div class="node compound${dc}">
          <div class="header"><span class="form-tag">(${x.length})</span></div>
          <div class="children">
            ${x.map((c, i) => this._node(c, this._childPath(path, i)))}
          </div>
        </div>`;
    }
    const form = x.form;
    const scopes = ctxScopes(x.ctx);
    const badges = scopes.map(
      (s) => html`<span class="scp" style="background:${scopeColor(s)}"
                        title=${s}>${s}</span>`,
    );

    if (Array.isArray(form)) {
      return html`
        <div class="node compound${dc}">
          <div class="header">
            <span class="form-tag">(${form.length})</span>
            ${badges}
          </div>
          <div class="children">
            ${form.map((child, i) => this._node(child, this._childPath(path, i)))}
          </div>
        </div>`;
    }

    return html`
      <span class="node leaf${dc}" style="border-color:${scopes.length ? scopeColor(scopes[0]) : "#ccc"}">
        <span class="form-atom">${String(form)}</span>
        ${badges}
      </span>`;
  }
}

customElements.define("ssv-stx-tree", SsvStxTree);
