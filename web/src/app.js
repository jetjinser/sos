import { LitElement, html, css, nothing } from "lit";
import { init, runModel } from "./wasm.js";
import "./components/code-input.js";
import "./components/store-panel.js";
import "./components/step-controls.js";
import "./components/source-view.js";

const EXAMPLES = {
  core: "(let-syntax x (lambda z (syntax (quote 2))) (x 1))",
  phases:
    "(lambda z (let-syntax x (lambda s (MKS (LIST (syntax lambda) (syntax z) (CAR (CDR (SE s)))) (syntax here))) (x z)))",
  local:
    "(let-syntax q (lambda s (syntax (CAR 8))) (let-syntax x (lambda s (CAR (CDR (SE (LOCAL-EXPAND (CAR (CDR (SE s))) (LIST)))))) (x (q))))",
  defs:
    "(let-syntax call (lambda s (MKS (LIST (CAR (CDR (SE s)))) (syntax here))) (let-syntax p (lambda s (syntax 0)) (let-syntax q (lambda s ((lambda defs ((lambda ignored (MKS (LIST (syntax lambda) (LOCAL-BINDER (CAR (CDR (SE (LOCAL-EXPAND (MKS (LIST (syntax quote) (CAR (CDR (SE s)))) (syntax here)) (LIST) defs))))) (LOCAL-EXPAND (CAR (CDR (CDR (SE s)))) (LIST (syntax call)) defs)) (syntax here))) (DEF-BIND defs (CAR (CDR (SE s)))))) (NEW-DEFS))) (q p (call p)))))",
};

export class SsvApp extends LitElement {
  static properties = {
    _trace: { state: true },
    _index: { state: true },
    _playing: { state: true },
    _speed: { state: true },
    _loading: { state: true },
    _loadError: { state: true },
    _runError: { state: true },
    _src: { state: true },
    _model: { state: true },
  };

  static styles = css`
    :host {
      display: grid;
      grid-template-rows: auto 1fr;
      height: 100vh; box-sizing: border-box;
      padding: 0.7rem;
      gap: 0.55rem;
      font-family: "Iosevka", "JetBrains Mono", "Fira Code", ui-monospace, monospace;
      background:
        radial-gradient(1100px 500px at 85% -10%, hsl(210 60% 96%), transparent 60%),
        radial-gradient(900px 500px at -10% 110%, hsl(150 40% 96%), transparent 55%),
        radial-gradient(hsl(220 15% 88%) 1px, transparent 1px) 0 0 / 22px 22px,
        hsl(220 20% 98%);
    }
    .topbar {
      display: flex; align-items: center; gap: 0.8rem; flex-wrap: wrap;
    }
    .main {
      display: grid;
      grid-template-columns: 1fr minmax(15em, 18em);
      gap: 0.55rem;
      min-height: 0; overflow: hidden;
    }
    .editor-panel {
      display: flex; flex-direction: column;
      min-height: 0; overflow: hidden;
      background: hsl(48 45% 98% / 0.92);
      border: 1px solid hsl(40 30% 82%);
      border-left: 4px solid hsl(40 70% 52%);
      border-radius: 6px;
      padding: 0.55rem 0.75rem;
      box-shadow: 0 1px 3px hsl(220 30% 20% / 0.06);
    }
    .side {
      overflow: auto; min-height: 0;
      background: hsl(0 0% 100% / 0.82);
      border: 1px solid hsl(220 15% 84%);
      border-radius: 6px;
      padding: 0.5rem 0.6rem;
      box-shadow: 0 1px 3px hsl(220 30% 20% / 0.06);
    }
    .panel-label {
      font-size: 0.68rem; font-weight: 700; letter-spacing: 0.12em;
      text-transform: uppercase; color: hsl(220 12% 55%);
      margin-bottom: 0.4rem;
    }
    .error { color: #a00; padding: 0.5rem; }
    .run-error {
      font-size: 0.72rem;
      color: hsl(4 60% 38%);
      background: hsl(4 70% 96%);
      border: 1px solid hsl(4 50% 82%);
      border-left: 3px solid hsl(4 60% 50%);
      border-radius: 4px;
      padding: 0.3rem 0.6rem;
      margin-bottom: 0.45rem;
      flex-shrink: 0;
      animation: run-error-in 200ms ease-out;
    }
    @keyframes run-error-in {
      from { opacity: 0; transform: translateY(-3px); }
      to   { opacity: 1; transform: none; }
    }
  `;

  constructor() {
    super();
    this._trace = null;
    this._index = 0;
    this._playing = false;
    this._speed = 500;
    this._loading = true;
    this._loadError = null;
    this._runError = null;
    this._timer = null;
    this._debounce = null;
    this._src = "";
    this._model = "core";
  }

  async connectedCallback() {
    super.connectedCallback();
    try {
      await init();
      this._loading = false;
      this._src = EXAMPLES[this._model];
      this._run();
    } catch (e) {
      this._loading = false;
      this._loadError = e instanceof WebAssembly.CompileError
        ? "Wasm GC + tail call required (Firefox / Chrome latest)"
        : String(e);
    }
    this.addEventListener("keydown", this._onKey);
  }

  disconnectedCallback() {
    super.disconnectedCallback();
    this._stopTimer();
    clearTimeout(this._debounce);
    this.removeEventListener("keydown", this._onKey);
  }

  get _step() {
    return this._trace?.steps?.[this._index] ?? null;
  }

  get _currentStore() {
    return this._trace?.stores?.[this._index] ?? null;
  }

  render() {
    if (this._loadError) return html`<div class="error">${this._loadError}</div>`;

    return html`
      <div class="topbar">
        <ssv-code-input .model=${this._model} .loading=${this._loading}
                        @model-change=${this._onModel}></ssv-code-input>
        <ssv-step-controls
          .steps=${this._trace?.steps ?? []}
          .index=${this._index}
          .playing=${this._playing}
          .speed=${this._speed}
          @step-to=${this._onStepTo}
          @play-toggle=${this._onPlayToggle}
          @speed-change=${this._onSpeed}></ssv-step-controls>
      </div>

      <div class="main">
        <div class="editor-panel">
          ${this._runError
            ? html`<div class="run-error">${this._runError}</div>`
            : nothing}
          <ssv-source-view editable
            .src=${this._src}
            .snapshot=${this._trace?.snapshots?.[this._index] ?? null}
            .prevSnapshot=${this._index > 0 ? this._trace?.snapshots?.[this._index - 1] ?? null : null}
            .resolve=${this._trace?.resolve ?? null}
            @code-input=${this._onCodeInput}></ssv-source-view>
        </div>
        <div class="side">
          <div class="panel-label">store</div>
          ${this._trace
            ? html`<ssv-store-panel .store=${this._currentStore}></ssv-store-panel>`
            : nothing}
        </div>
      </div>
    `;
  }

  _onModel(e) {
    this._model = e.detail.model;
    this._src = EXAMPLES[this._model] ?? "";
    this._run();
  }

  _onCodeInput(e) {
    this._src = e.detail.value;
    this._playing = false;
    this._stopTimer();
    clearTimeout(this._debounce);
    this._debounce = setTimeout(() => this._run(), 250);
  }

  _run() {
    // Empty input is "nothing to expand", not an error: keep the editor clean
    // and typeable instead of surfacing a parse failure.
    if (!this._src.trim()) {
      this._trace = null;
      this._runError = null;
      return;
    }
    try {
      this._trace = runModel(this._model, this._src);
      const n = this._trace?.steps?.length ?? 0;
      this._index = n > 0 ? n - 1 : 0;
      this._runError = null;
    } catch (err) {
      this._trace = null;
      this._runError = String(err?.message ?? err);
    }
  }

  _onStepTo(e) {
    this._index = e.detail.index;
  }

  _onPlayToggle() {
    this._playing = !this._playing;
    if (this._playing) this._startTimer();
    else this._stopTimer();
  }

  _onSpeed(e) {
    this._speed = e.detail.speed;
    if (this._playing) {
      this._stopTimer();
      this._startTimer();
    }
  }

  _startTimer() {
    this._stopTimer();
    this._timer = setInterval(() => {
      const n = this._trace?.steps?.length ?? 0;
      if (this._index >= n - 1) {
        this._playing = false;
        this._stopTimer();
        return;
      }
      this._index++;
    }, this._speed);
  }

  _stopTimer() {
    if (this._timer !== null) {
      clearInterval(this._timer);
      this._timer = null;
    }
  }

  _onKey(e) {
    if (e.target.tagName === "TEXTAREA" || e.target.tagName === "SELECT") return;
    const n = this._trace?.steps?.length ?? 0;
    if (e.key === "ArrowRight" && this._index < n - 1) this._index++;
    if (e.key === "ArrowLeft" && this._index > 0) this._index--;
  }
}

customElements.define("ssv-app", SsvApp);
