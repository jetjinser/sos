import { LitElement, html, css } from "lit";

export class SsvCodeInput extends LitElement {
  static properties = {
    model: { type: String },
    loading: { type: Boolean },
  };

  static styles = css`
    :host { display: inline-flex; align-items: center; }
    label {
      display: inline-flex; align-items: center; gap: 0.4rem;
      font-size: 0.85rem; color: hsl(220 20% 35%);
    }
    select {
      font-family: inherit; font-size: 0.85rem;
      padding: 0.22rem 0.5rem;
      border: 1px solid hsl(220 15% 78%);
      border-radius: 5px;
      background: hsl(0 0% 100%);
      color: hsl(220 25% 32%);
      cursor: pointer;
    }
    select:disabled { opacity: 0.5; cursor: default; }
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
      </label>`;
  }

  _onModel(e) {
    this.dispatchEvent(new CustomEvent("model-change", {
      detail: { model: e.target.value }, bubbles: true, composed: true,
    }));
  }
}

customElements.define("ssv-code-input", SsvCodeInput);
