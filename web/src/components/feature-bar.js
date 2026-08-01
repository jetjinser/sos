import { LitElement, html, css } from "lit";

// Compact status chips for the WebAssembly features the app needs.  Green
// when the engine supports a feature, red when it does not — so a load
// failure is self-explanatory at a glance.
export class SsvFeatureBar extends LitElement {
  static properties = {
    features: { type: Array },
  };

  static styles = css`
    :host {
      display: inline-flex; align-items: center;
      margin-left: auto; flex-shrink: 0;
    }
    .bar { display: inline-flex; align-items: center; gap: 5px; flex-wrap: wrap; }
    .label {
      font-size: 0.6rem; letter-spacing: 0.12em; text-transform: uppercase;
      color: hsl(220 12% 55%);
      margin-right: 2px;
    }
    .feat {
      display: inline-flex; align-items: center; gap: 3px;
      font-size: 0.64rem; line-height: 1.5;
      padding: 1px 7px; border-radius: 8px;
      border: 1px solid;
      cursor: default; white-space: nowrap;
      transition: transform 120ms ease, box-shadow 120ms ease;
    }
    .feat:hover { transform: translateY(-1px); }
    .feat .mark { font-weight: 700; }
    .feat.ok {
      color: hsl(145 45% 32%);
      border-color: hsl(145 35% 74%);
      background: hsl(145 45% 95%);
    }
    .feat.ok:hover { box-shadow: 0 1px 4px hsl(145 45% 40% / 0.25); }
    .feat.no {
      color: hsl(4 60% 40%);
      border-color: hsl(4 55% 74%);
      background: hsl(4 70% 96%);
      animation: attention 1.6s ease-in-out infinite;
    }
    .feat.no:hover { box-shadow: 0 1px 4px hsl(4 60% 45% / 0.3); }
    @keyframes attention {
      0%, 100% { box-shadow: 0 0 0 0 hsl(4 60% 50% / 0); }
      50%      { box-shadow: 0 0 0 3px hsl(4 60% 50% / 0.18); }
    }
  `;

  constructor() {
    super();
    this.features = [];
  }

  render() {
    if (!this.features.length) return "";
    return html`<div class="bar">
      <span class="label">wasm</span>
      ${this.features.map(
        (f) => html`<span
          class="feat ${f.supported ? "ok" : "no"}"
          title="${f.name}${f.supported ? " — supported" : " — MISSING"}">
          <span class="mark">${f.supported ? "✓" : "✗"}</span>${f.short}
        </span>`,
      )}
    </div>`;
  }
}

customElements.define("ssv-feature-bar", SsvFeatureBar);
