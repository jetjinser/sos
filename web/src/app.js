import { LitElement, html, css, nothing } from "lit";
import { init, runModel, formatSource } from "./wasm.js";
import { detectFeatures, missingFeatures } from "./lib/wasm-features.js";
import "./components/code-input.js";
import "./components/store-panel.js";
import "./components/step-controls.js";
import "./components/source-view.js";
import "./components/feature-bar.js";
import "./components/step-detail.js";
import "./components/step-scrubber.js";
import "./components/step-compare.js";
import "./components/ast-view.js";

const EXAMPLES = {
  core: "(let-syntax x (lambda z (syntax (quote 2))) (x 1))",
  phases:
    "(lambda z (let-syntax x (lambda s (datum->syntax (syntax here) (LIST (syntax lambda) (syntax z) (CAR (CDR (syntax->datum s)))))) (x z)))",
  local:
    "(let-syntax q (lambda s (syntax (CAR 8))) (let-syntax x (lambda s (CAR (CDR (syntax->datum (LOCAL-EXPAND (CAR (CDR (syntax->datum s))) (LIST)))))) (x (q))))",
  defs:
    "(let-syntax call (lambda s (datum->syntax (syntax here) (LIST (CAR (CDR (syntax->datum s)))))) (let-syntax p (lambda s (syntax 0)) (let-syntax q (lambda s ((lambda defs ((lambda ignored (datum->syntax (syntax here) (LIST (syntax lambda) (LOCAL-BINDER (CAR (CDR (syntax->datum (LOCAL-EXPAND (datum->syntax (syntax here) (LIST (syntax quote) (CAR (CDR (syntax->datum s))))) (LIST) defs))))) (LOCAL-EXPAND (CAR (CDR (CDR (syntax->datum s)))) (LIST (syntax call)) defs)))) (DEF-BIND defs (CAR (CDR (syntax->datum s)))))) (NEW-DEFS))) (q p (call p)))))",
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
    _features: { state: true },
    _view: { state: true },
  };

  static styles = css`
    :host {
      display: grid;
      grid-template-rows: auto auto 1fr;
      height: 100vh; box-sizing: border-box;
      padding: clamp(1.2rem, 3vw, 3rem);
      gap: clamp(0.9rem, 1.6vw, 1.4rem);
      font-family: "Iosevka", "JetBrains Mono", "Fira Code", ui-monospace, monospace;
      background:
        radial-gradient(1100px 500px at 85% -10%, hsl(210 60% 96%), transparent 60%),
        radial-gradient(900px 500px at -10% 110%, hsl(150 40% 96%), transparent 55%),
        radial-gradient(hsl(220 15% 88%) 1px, transparent 1px) 0 0 / 22px 22px,
        hsl(220 20% 98%);
    }
    /* Controls only: the current-step badge, info and rewrite live in the
       sidebar's step pane (ssv-step-detail), so nothing here competes for width */
    .topbar {
      display: flex; align-items: center;
      gap: clamp(0.8rem, 1.6vw, 1.2rem);
    }
    /* Inspector rail left, code right: the step's rewrite sits adjacent to
       the flush-left source it rewrites, and causality reads left to right */
    .main {
      display: grid;
      grid-template-columns: minmax(16em, 19em) 1fr;
      gap: clamp(0.9rem, 1.6vw, 1.4rem);
      min-height: 0; overflow: hidden;
    }
    .editor-panel {
      display: flex; flex-direction: column;
      min-height: 0; overflow: hidden;
      background: hsl(48 45% 98% / 0.92);
      border: 1px solid hsl(40 30% 82%);
      border-left: 4px solid hsl(40 70% 52%);
      border-radius: 10px;
      padding: clamp(0.8rem, 1.6vw, 1.2rem) clamp(1rem, 2vw, 1.4rem);
      box-shadow: 0 1px 2px hsl(220 30% 20% / 0.05),
                  0 8px 24px hsl(220 30% 30% / 0.07);
    }
    /* IDE-style tabs switching between the source and the macro-stepper
       expansion comparison; both stay mounted so neither loses state */
    .view-tabs {
      display: flex; gap: 4px;
      margin-bottom: 0.6rem;
      border-bottom: 1px solid hsl(40 25% 85%);
      flex-shrink: 0;
    }
    .vtab {
      font-family: inherit; font-size: 0.72rem; font-weight: 600;
      letter-spacing: 0.04em;
      padding: 0.3rem 0.85rem 0.4rem;
      border: none; border-bottom: 2px solid transparent;
      background: transparent;
      color: hsl(38 18% 52%);
      cursor: pointer;
      transition: color 140ms ease, border-color 140ms ease,
                  background-color 140ms ease;
    }
    .vtab:hover { color: hsl(38 40% 38%); background: hsl(40 40% 94% / 0.6); }
    .vtab.on {
      color: hsl(32 65% 38%);
      border-bottom-color: hsl(40 70% 52%);
    }
    .view { display: none; flex: 1; min-height: 0; }
    .view.show { display: flex; flex-direction: column; }
    .side {
      overflow: auto; min-height: 0;
      background: hsl(0 0% 100% / 0.85);
      border: 1px solid hsl(220 15% 84%);
      border-radius: 10px;
      padding: clamp(0.8rem, 1.5vw, 1.1rem) clamp(0.9rem, 1.7vw, 1.2rem);
      box-shadow: 0 1px 2px hsl(220 30% 20% / 0.05),
                  0 8px 24px hsl(220 30% 30% / 0.07);
    }
    .panel-label {
      font-size: 0.68rem; font-weight: 700; letter-spacing: 0.14em;
      text-transform: uppercase; color: hsl(220 12% 52%);
      margin-bottom: 0.75rem;
    }
    .panel-label:not(:first-child) { margin-top: 1.15rem; }
    .error {
      grid-row: 1 / -1;
      place-self: center;
      max-width: 36em;
      background: hsl(4 70% 97%);
      border: 1px solid hsl(4 50% 84%);
      border-left: 4px solid hsl(4 60% 50%);
      border-radius: 10px;
      padding: 1.2rem 1.5rem;
      color: hsl(4 55% 34%);
      font-size: 0.85rem; line-height: 1.65;
      box-shadow: 0 8px 28px hsl(4 50% 40% / 0.14);
    }
    .run-error {
      font-size: 0.72rem;
      color: hsl(4 60% 38%);
      background: hsl(4 70% 96%);
      border: 1px solid hsl(4 50% 82%);
      border-left: 3px solid hsl(4 60% 50%);
      border-radius: 6px;
      padding: 0.4rem 0.7rem;
      margin-bottom: 0.7rem;
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
    this._features = [];
    this._view = "source";
  }

  async connectedCallback() {
    super.connectedCallback();
    this._features = detectFeatures();
    try {
      await init();
      this._loading = false;
      this._loadExample(this._model);
    } catch (e) {
      this._loading = false;
      this._loadError = this._diagnoseLoadError(e);
    }
    this.addEventListener("keydown", this._onKey);
  }

  // Distinguish "the browser lacks a required feature" from "the build is
  // broken" so the error points at the real cause instead of a generic one.
  _diagnoseLoadError(e) {
    const missing = missingFeatures(this._features);
    if (missing.length) {
      return `This browser is missing required WebAssembly features: ${missing.join(", ")}. Use the latest Firefox or Chrome.`;
    }
    if (e instanceof WebAssembly.CompileError) {
      return "Failed to compile the app module — the build may be corrupt. Re-run `blue build` (and `blue release` if serving the release).";
    }
    return String(e);
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
                        @model-change=${this._onModel}
                        @format=${this._onFormat}></ssv-code-input>
        <ssv-step-controls
          .steps=${this._trace?.steps ?? []}
          .index=${this._index}
          .playing=${this._playing}
          .speed=${this._speed}
          @step-to=${this._onStepTo}
          @play-toggle=${this._onPlayToggle}
          @speed-change=${this._onSpeed}></ssv-step-controls>
        <ssv-feature-bar .features=${this._features}></ssv-feature-bar>
      </div>

      <ssv-step-scrubber .steps=${this._trace?.steps ?? []}
                         .index=${this._index}
                         .playing=${this._playing}
                         @step-to=${this._onStepTo}></ssv-step-scrubber>

      <div class="main">
        <div class="side">
          <div class="panel-label">step</div>
          <ssv-step-detail .step=${this._step}
                           .index=${this._index}></ssv-step-detail>
          <div class="panel-label">store</div>
          ${this._trace
            ? html`<ssv-store-panel .store=${this._currentStore}></ssv-store-panel>`
            : nothing}
          <div class="panel-label">result</div>
          <ssv-ast-view .ast=${this._trace?.["final-ast"] ?? null}></ssv-ast-view>
        </div>
        <div class="editor-panel">
          <div class="view-tabs">
            <button class="vtab ${this._view === "source" ? "on" : ""}"
                    @click=${() => (this._view = "source")}>source</button>
            <button class="vtab ${this._view === "expansion" ? "on" : ""}"
                    @click=${() => (this._view = "expansion")}>expansion</button>
          </div>
          ${this._runError
            ? html`<div class="run-error">${this._runError}</div>`
            : nothing}
          <div class="view ${this._view === "source" ? "show" : ""}">
            <ssv-source-view editable
              .src=${this._src}
              .snapshot=${this._trace?.snapshots?.[this._index] ?? null}
              .prevSnapshot=${this._index > 0 ? this._trace?.snapshots?.[this._index - 1] ?? null : null}
              .resolve=${this._trace?.resolve ?? null}
              .binders=${this._trace?.binders ?? null}
              .uses=${this._trace?.uses ?? null}
              @code-input=${this._onCodeInput}></ssv-source-view>
          </div>
          <div class="view ${this._view === "expansion" ? "show" : ""}">
            <ssv-step-compare .step=${this._step}
                              .index=${this._index}></ssv-step-compare>
          </div>
        </div>
      </div>
    `;
  }

  _onModel(e) {
    this._model = e.detail.model;
    this._loadExample(this._model);
  }

  // Load a model's example already pretty-printed (fall back to raw if the
  // formatter is unavailable for any reason).
  _loadExample(model) {
    const raw = EXAMPLES[model] ?? "";
    try {
      this._src = formatSource(raw);
    } catch {
      this._src = raw;
    }
    this._run();
  }

  _onFormat() {
    if (!this._src.trim()) return;
    try {
      this._src = formatSource(this._src);
      this._runError = null;
      this._run();
    } catch (err) {
      this._runError = (err && (err.message || String(err))) || "format failed";
    }
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
      this._runError = (err && (err.message || String(err))) || "expansion failed";
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
