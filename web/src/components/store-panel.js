import { LitElement, html, css, nothing } from "lit";
import { scopeColor } from "../lib/scope-colors.js";

export class SsvStorePanel extends LitElement {
  static properties = {
    store: { type: Object },
  };

  static styles = css`
    :host { display: block; font-family: monospace; font-size: 0.78rem; }
    h3 { font-size: 0.8rem; margin: 0.5rem 0 0.2rem; color: #444; }
    h3:first-child { margin-top: 0; }
    table { border-collapse: collapse; width: 100%; margin-bottom: 0.4rem; }
    td, th {
      border: 1px solid #ddd; padding: 2px 5px; text-align: left;
      vertical-align: top;
    }
    th { background: #f5f5f5; font-weight: normal; color: #666; }
    .scp {
      font-size: 0.68rem; padding: 0 3px; border-radius: 3px;
      color: #fff; white-space: nowrap; display: inline-block; margin: 1px;
    }
    .counter { color: #888; }
    .bname { font-weight: 700; }
    .empty { color: #aaa; font-style: italic; }
    .val { max-width: 16em; overflow: hidden; text-overflow: ellipsis; }
  `;

  constructor() {
    super();
    this.store = null;
  }

  render() {
    const s = this.store;
    if (!s) return html`<span class="empty">—</span>`;
    return html`
      <h3>counter: <span class="counter">${s.counter}</span></h3>
      ${this._binds(s.binds)}
      ${this._boxes(s.boxes)}
      ${this._defEnvs(s["def-envs"])}
    `;
  }

  _scope(scopes) {
    if (!scopes || !scopes.length) return html`<span class="empty">∅</span>`;
    return scopes.map(
      (sc) => html`<span class="scp" style="background:${scopeColor(sc)}">${sc}</span>`,
    );
  }

  _binds(binds) {
    const entries = Object.entries(binds ?? {});
    if (!entries.length) return nothing;
    return html`
      <h3>binds</h3>
      <table>
        <tr><th>sym</th><th>scopes</th><th>name</th></tr>
        ${entries.flatMap(([sym, pairs]) =>
          pairs.map(
            ([scopes, name], i) => html`
              <tr>
                <td>${i === 0 ? sym : ""}</td>
                <td>${this._scope(scopes)}</td>
                <td><span class="bname" style="color:${scopeColor(name)}">${name}</span></td>
              </tr>`,
          ),
        )}
      </table>`;
  }

  _boxes(boxes) {
    if (!boxes || !boxes.length) return nothing;
    return html`
      <h3>boxes</h3>
      <table>
        <tr><th>addr</th><th>value</th></tr>
        ${boxes.map(
          ([addr, val]) => html`
            <tr><td>${addr}</td><td class="val">${JSON.stringify(val)}</td></tr>`,
        )}
      </table>`;
  }

  _defEnvs(defEnvs) {
    if (!defEnvs || !defEnvs.length) return nothing;
    return html`
      <h3>def-envs</h3>
      ${defEnvs.map(
        ([addr, env]) => html`
          <div style="margin-bottom:0.3rem">
            <strong>${addr}</strong>
            <table>
              <tr><th>name</th><th>value</th></tr>
              ${env.map(
                ([name, val]) => html`
                  <tr><td>${name}</td><td class="val">${JSON.stringify(val)}</td></tr>`,
              )}
            </table>
          </div>`,
      )}`;
  }
}

customElements.define("ssv-store-panel", SsvStorePanel);
