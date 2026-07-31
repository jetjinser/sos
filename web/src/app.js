import { LitElement, html, css, nothing } from "lit";
import { keyed } from "lit/directives/keyed.js";
import { init, runModel } from "./wasm.js";
import { scopeColor } from "./lib/scope-colors.js";
import "./components/code-input.js";
import "./components/stx-tree.js";
import "./components/store-panel.js";
import "./components/step-controls.js";
import "./components/source-view.js";

const STX_OPS = new Set(["stx-add", "stx-flip", "stx-prune"]);

export class SsvApp extends LitElement {
  static properties = {
    _trace: { state: true },
    _index: { state: true },
    _playing: { state: true },
    _speed: { state: true },
    _loading: { state: true },
    _error: { state: true },
    _src: { state: true },
  };

  static styles = css`
    :host {
      display: grid;
      grid-template-rows: auto 1fr auto;
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
    .main {
      display: grid;
      grid-template-columns: 1fr 1fr minmax(15em, 0.72fr);
      gap: 0.55rem;
      overflow: hidden; min-height: 0;
    }
    .panel {
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
    .panel-label b { color: hsl(220 45% 38%); }
    .placeholder { color: #99a; font-style: italic; padding: 1.2rem 0.4rem; }
    .error { color: #a00; padding: 0.5rem; }

    .source-panel {
      background: hsl(48 45% 98% / 0.92);
      border: 1px solid hsl(40 30% 82%);
      border-left: 4px solid hsl(40 70% 52%);
      border-radius: 6px;
      padding: 0.45rem 0.8rem;
      min-height: 9em; max-height: 38vh; overflow: auto;
      box-shadow: 0 1px 3px hsl(220 30% 20% / 0.06);
    }
    .source-panel .panel-label { color: hsl(38 30% 45%); }
    .step-tag {
      margin-left: 0.5rem;
      font-size: 0.66rem; letter-spacing: 0.05em;
      color: hsl(38 45% 38%);
      background: hsl(40 60% 90%);
      border: 1px solid hsl(40 40% 78%);
      border-radius: 8px;
      padding: 0 7px;
    }

    ssv-stx-tree { animation: step-in 200ms ease-out; display: block; }
    @keyframes step-in {
      from { opacity: 0.15; transform: translateY(5px); }
      to   { opacity: 1;    transform: none; }
    }

    .op-card { animation: step-in 200ms ease-out; padding: 0.3rem 0.1rem; }
    .op-card .kind {
      display: inline-block; font-weight: 700; font-size: 0.85rem;
      color: hsl(16 60% 40%); background: hsl(16 70% 94%);
      border: 1px solid hsl(16 50% 80%);
      padding: 0.1rem 0.5rem; border-radius: 4px; margin-bottom: 0.5rem;
    }
    .op-card .row { margin: 0.25rem 0; font-size: 0.8rem; color: #445; }
    .op-card .fresh {
      display: inline-block; font-weight: 700;
      padding: 0.05rem 0.45rem; border-radius: 4px;
      color: #fff; animation: pop 350ms ease-out;
    }
    @keyframes pop {
      0%   { transform: scale(0.6); }
      60%  { transform: scale(1.15); }
      100% { transform: scale(1); }
    }
    .scp {
      font-size: 0.7rem; padding: 0 4px; border-radius: 3px;
      color: #fff; display: inline-block; margin: 1px;
      transition: transform 120ms ease;
    }
    .scp:hover { transform: scale(1.12); }
  `;

  constructor() {
    super();
    this._trace = null;
    this._index = 0;
    this._playing = false;
    this._speed = 500;
    this._loading = true;
    this._error = null;
    this._timer = null;
    this._src = "";
  }

  async connectedCallback() {
    super.connectedCallback();
    try {
      await init();
      this._loading = false;
    } catch (e) {
      this._loading = false;
      this._error = e instanceof WebAssembly.CompileError
        ? "Wasm GC + tail call required (Firefox / Chrome latest)"
        : String(e);
    }
    this.addEventListener("keydown", this._onKey);
  }

  disconnectedCallback() {
    super.disconnectedCallback();
    this._stopTimer();
    this.removeEventListener("keydown", this._onKey);
  }

  get _step() {
    return this._trace?.steps?.[this._index] ?? null;
  }

  get _beforeStx() {
    const s = this._step;
    if (!s) return null;
    if (s.type === "rule") return s.before ?? null;
    if (s.type === "op" && STX_OPS.has(s.op)) return s.data?.[1] ?? null;
    return null;
  }

  get _afterStx() {
    const s = this._step;
    if (!s) return null;
    if (s.type === "rule") return s.after ?? null;
    if (s.type === "op" && STX_OPS.has(s.op)) return s.data?.[2] ?? null;
    return null;
  }

  get _currentStore() {
    return this._trace?.stores?.[this._index] ?? null;
  }

  render() {
    if (this._error) return html`<div class="error">${this._error}</div>`;

    const step = this._step;
    const isStxView = step && (step.type === "rule" || STX_OPS.has(step.op));

    return html`
      <ssv-code-input .loading=${this._loading}
                      @run=${this._onRun}>
        <ssv-step-controls
          .steps=${this._trace?.steps ?? []}
          .index=${this._index}
          .playing=${this._playing}
          .speed=${this._speed}
          @step-to=${this._onStepTo}
          @play-toggle=${this._onPlayToggle}
          @speed-change=${this._onSpeed}></ssv-step-controls>
      </ssv-code-input>

      <div class="main">
        <div class="panel">
          <div class="panel-label">before expand</div>
          ${this._trace
            ? (isStxView
                ? keyed(this._index, html`<ssv-stx-tree .stx=${this._beforeStx}></ssv-stx-tree>`)
                : this._opCard(step))
            : html`<span class="placeholder">Run to see the expansion trace</span>`}
        </div>
        <div class="panel">
          <div class="panel-label">after expand</div>
          ${this._trace && isStxView
            ? keyed(this._index, html`<ssv-stx-tree .stx=${this._afterStx}></ssv-stx-tree>`)
            : nothing}
        </div>
        <div class="panel">
          <div class="panel-label">store status</div>
          ${this._trace && step
            ? keyed(this._index, html`<ssv-store-panel .store=${this._currentStore}></ssv-store-panel>`)
            : nothing}
        </div>
      </div>

      <div class="source-panel">
        <div class="panel-label">source · scopes
          ${this._trace && step
            ? html`<span class="step-tag">${this._index + 1}/${this._trace.steps.length}</span>`
            : nothing}
        </div>
        ${this._trace
          ? html`<ssv-source-view
              .src=${this._src}
              .snapshot=${this._trace.snapshots?.[this._index] ?? null}
              .prevSnapshot=${this._index > 0 ? this._trace.snapshots?.[this._index - 1] ?? null : null}
              .resolve=${this._trace.resolve ?? null}></ssv-source-view>`
          : html`<span class="placeholder">Run to see scopes painted on your code</span>`}
      </div>
    `;
  }

  _opCard(step) {
    const data = step.data ?? [];
    if (step.op === "bind") {
      const [sym, scopes, name] = data;
      return html`
        <div class="op-card">
          <span class="kind">store-bind</span>
          <div class="row">sym: <strong>${sym}</strong></div>
          <div class="row">scopes:
            ${(scopes ?? []).map((sc) =>
              html`<span class="scp" style="background:${scopeColor(sc)}">${sc}</span>`)}
          </div>
          <div class="row">→ name:
            <span class="fresh" style="background:${scopeColor(name)}">${name}</span>
          </div>
        </div>`;
    }
    if (step.op === "alloc-name" || step.op === "alloc-scope") {
      const [name] = data;
      return html`
        <div class="op-card">
          <span class="kind">${step.op}</span>
          <div class="row">fresh:
            <span class="fresh" style="background:${scopeColor(name)}">${name}</span>
          </div>
        </div>`;
    }
    return html`<div class="op-card"><span class="kind">${step.op}</span>
      <div class="row">${JSON.stringify(data)}</div></div>`;
  }

  _onRun(e) {
    const { model, input } = e.detail;
    this._stopTimer();
    this._playing = false;
    try {
      this._trace = runModel(model, input);
      this._src = input;
      this._index = 0;
      this._error = null;
    } catch (err) {
      this._trace = null;
      this._error = String(err?.message ?? err);
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
