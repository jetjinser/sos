import { LitElement, html, css } from "lit";

const EXAMPLES = {
  core: "(let-syntax x (lambda z (syntax (quote 2))) (x 1))",
  phases:
    "(lambda z (let-syntax x (lambda s (MKS (LIST (syntax lambda) (syntax z) (CAR (CDR (SE s)))) (syntax here))) (x z)))",
  local:
    "(let-syntax q (lambda s (syntax (CAR 8))) (let-syntax x (lambda s (CAR (CDR (SE (LOCAL-EXPAND (CAR (CDR (SE s))) (LIST)))))) (x (q))))",
  defs:
    "(let-syntax call (lambda s (MKS (LIST (CAR (CDR (SE s)))) (syntax here))) (let-syntax p (lambda s (syntax 0)) (let-syntax q (lambda s ((lambda defs ((lambda ignored (MKS (LIST (syntax lambda) (LOCAL-BINDER (CAR (CDR (SE (LOCAL-EXPAND (MKS (LIST (syntax quote) (CAR (CDR (SE s)))) (syntax here)) (LIST) defs))))) (LOCAL-EXPAND (CAR (CDR (CDR (SE s)))) (LIST (syntax call)) defs)) (syntax here))) (DEF-BIND defs (CAR (CDR (SE s)))))) (NEW-DEFS))) (q p (call p)))))",
};

export class SsvCodeInput extends LitElement {
  static properties = {
    model: { type: String },
    input: { type: String },
    loading: { type: Boolean },
  };

  static styles = css`
    :host { display: block; }
    .bar {
      display: flex; gap: 0.5rem; align-items: center;
      flex-wrap: wrap;
      margin-bottom: 0.4rem;
    }
    .divider {
      width: 1px; height: 1.5em; background: hsl(220 15% 80%);
      margin: 0 0.15rem;
    }
    select, button {
      font-family: inherit; font-size: 0.85rem;
      padding: 0.2rem 0.5rem;
    }
    button { cursor: pointer; }
    button:disabled { opacity: 0.5; cursor: default; }
    textarea {
      width: 100%; height: 6em; box-sizing: border-box;
      font-family: inherit; font-size: 0.82rem;
      padding: 0.4rem; border: 1px solid #bbb; border-radius: 3px;
      resize: vertical;
    }
  `;

  constructor() {
    super();
    this.model = "core";
    this.input = EXAMPLES.core;
    this.loading = false;
  }

  render() {
    return html`
      <div class="bar">
        <label>Model:
          <select .value=${this.model} @change=${this._onModel}>
            <option value="core">core</option>
            <option value="phases">phases</option>
            <option value="local">local</option>
            <option value="defs">defs</option>
          </select>
        </label>
        <button @click=${this._onRun} ?disabled=${this.loading}>
          ${this.loading ? "Running…" : "Run"}
        </button>
        <span class="divider"></span>
        <slot></slot>
      </div>
      <textarea spellcheck="false" .value=${this.input}
                @input=${this._onInput}></textarea>
    `;
  }

  _onModel(e) {
    this.model = e.target.value;
    this.input = EXAMPLES[this.model] ?? "";
    this.dispatchEvent(new CustomEvent("model-change", {
      detail: { model: this.model }, bubbles: true, composed: true,
    }));
  }

  _onInput(e) { this.input = e.target.value; }

  _onRun() {
    this.dispatchEvent(new CustomEvent("run", {
      detail: { model: this.model, input: this.input },
      bubbles: true, composed: true,
    }));
  }
}

customElements.define("ssv-code-input", SsvCodeInput);
