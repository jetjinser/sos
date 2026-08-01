import { LitElement, html, css } from "lit";

export class SsvCodeInput extends LitElement {
  static properties = {
    model: { type: String },
    loading: { type: Boolean },
  };

  static styles = css`
    :host {
      display: inline-flex; align-items: center; gap: 0.6rem;
      flex-shrink: 0;
    }
    label {
      display: inline-flex; align-items: center; gap: 0.4rem;
      font-size: 0.85rem; color: hsl(220 20% 35%);
    }
    select, button {
      font-family: inherit; font-size: 0.88rem;
      padding: 0.34rem 0.8rem;
      border: 1px solid hsl(220 15% 78%);
      border-radius: 6px;
      background: hsl(0 0% 100%);
      color: hsl(220 25% 32%);
      cursor: pointer;
      transition: background 120ms ease, transform 100ms ease, box-shadow 120ms ease;
    }
    select:disabled, button:disabled { opacity: 0.5; cursor: default; }
    button.fmt:hover:not(:disabled) {
      background: hsl(40 60% 94%);
      border-color: hsl(40 45% 70%);
      transform: translateY(-1px);
      box-shadow: 0 1px 3px hsl(40 50% 40% / 0.18);
    }
    button.fmt:active:not(:disabled) { transform: translateY(0); }
  `;

  constructor() {
    super();
    this.model = "core";
    this.loading = false;
  }

  render() {
    return html`
      <label>Model:
        <select .value=${this.model} ?disabled=${this.loading} @change=${this._onModel}>
          <option value="core">core</option>
          <option value="phases">phases</option>
          <option value="local">local</option>
          <option value="defs">defs</option>
        </select>
      </label>
      <button class="fmt" ?disabled=${this.loading} @click=${this._onFormat}>Format</button>`;
  }

  _onModel(e) {
    this.dispatchEvent(new CustomEvent("model-change", {
      detail: { model: e.target.value }, bubbles: true, composed: true,
    }));
  }

  _onFormat() {
    this.dispatchEvent(new CustomEvent("format", { bubbles: true, composed: true }));
  }
}

customElements.define("ssv-code-input", SsvCodeInput);
