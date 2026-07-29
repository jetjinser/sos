import { LitElement, html, css, nothing } from "lit";
import { keyed } from "lit/directives/keyed.js";

export class SsvStepControls extends LitElement {
  static properties = {
    steps: { type: Array },
    index: { type: Number },
    playing: { type: Boolean },
    speed: { type: Number },
  };

  static styles = css`
    :host {
      display: flex; align-items: center; gap: 0.55rem;
      font-family: inherit; font-size: 0.82rem;
      flex-wrap: wrap;
    }
    .transport { display: flex; align-items: center; gap: 0.3rem; }
    button {
      font-family: inherit; cursor: pointer;
      border: 1px solid hsl(220 15% 78%);
      background: hsl(0 0% 100%);
      color: hsl(220 25% 32%);
      border-radius: 5px;
      transition: transform 100ms ease, background 120ms ease,
                  box-shadow 120ms ease;
    }
    button.nav { font-size: 1.05rem; line-height: 1; padding: 0.28rem 0.5rem; }
    button.nav .step-glyph { font-size: 1.25rem; font-weight: 700; line-height: 0.7; }
    button.nav:hover:not(:disabled) {
      background: hsl(220 45% 95%);
      transform: translateY(-1px);
      box-shadow: 0 1px 3px hsl(220 40% 30% / 0.18);
    }
    button.nav:active:not(:disabled) { transform: translateY(0); }
    button.play {
      width: 2.1em; height: 2.1em;
      display: inline-flex; align-items: center; justify-content: center;
      font-size: 0.9rem;
      border: none; border-radius: 50%;
      background: hsl(221 58% 46%);
      color: #fff;
      box-shadow: 0 1px 5px hsl(221 55% 30% / 0.4);
    }
    button.play:hover { transform: scale(1.1); background: hsl(221 58% 40%); }
    button.play.on {
      background: hsl(16 62% 48%);
      animation: pulse 1.1s ease-in-out infinite;
    }
    @keyframes pulse {
      0%, 100% { box-shadow: 0 1px 5px hsl(16 60% 35% / 0.45); }
      50%      { box-shadow: 0 1px 12px hsl(16 70% 45% / 0.75); }
    }
    button:disabled { opacity: 0.35; cursor: default; }
    .pos { color: #556; min-width: 5em; text-align: center; font-variant-numeric: tabular-nums; }
    .rule {
      font-weight: bold; color: #246;
      background: #eef; padding: 0.1rem 0.4rem; border-radius: 3px;
      animation: badge-in 180ms ease-out;
    }
    .op {
      font-weight: bold; color: #642;
      background: #fee; padding: 0.1rem 0.4rem; border-radius: 3px;
      animation: badge-in 180ms ease-out;
    }
    @keyframes badge-in {
      from { opacity: 0.2; transform: translateX(-4px); }
      to   { opacity: 1;    transform: none; }
    }
    .info { color: #888; font-size: 0.75rem; }
    select { font-family: inherit; font-size: 0.78rem; }
  `;

  constructor() {
    super();
    this.steps = [];
    this.index = 0;
    this.playing = false;
    this.speed = 500;
  }

  get _step() {
    return this.steps[this.index] ?? null;
  }

  render() {
    const n = this.steps.length;
    const step = this._step;
    return html`
      <div class="transport">
        <button class="nav" title="First" @click=${this._first}
                ?disabled=${this.index <= 0}>⏮</button>
        <button class="nav" title="Prev" @click=${this._prev}
                ?disabled=${this.index <= 0}><span class="step-glyph">‹</span></button>
        <button class="play ${this.playing ? "on" : ""}"
                title=${this.playing ? "Pause" : "Play"}
                @click=${this._togglePlay}>${this.playing ? "⏸" : "▶"}</button>
        <button class="nav" title="Next" @click=${this._next}
                ?disabled=${this.index >= n - 1}><span class="step-glyph">›</span></button>
        <button class="nav" title="Last" @click=${this._last}
                ?disabled=${this.index >= n - 1}>⏭</button>
      </div>
      <span class="pos">${n ? this.index + 1 : 0} / ${n}</span>
      <select .value=${String(this.speed)} @change=${this._speed}>
        <option value="1000">0.5×</option>
        <option value="500">1×</option>
        <option value="250">2×</option>
        <option value="100">5×</option>
      </select>
      ${step ? keyed(this.index, this._stepInfo(step)) : nothing}
    `;
  }

  _stepInfo(step) {
    if (step.type === "rule") {
      const info = Object.entries(step.info ?? {})
        .map(([k, v]) => `${k}:${typeof v === "object" ? JSON.stringify(v) : v}`)
        .join("  ");
      return html`
        <span class="rule">${step.rule}</span>
        ${info ? html`<span class="info">${info}</span>` : nothing}`;
    }
    if (step.type === "op") {
      return html`<span class="op">op:${step.op}</span>`;
    }
    return nothing;
  }

  _emit(index) {
    this.dispatchEvent(new CustomEvent("step-to", {
      detail: { index: Math.max(0, Math.min(index, this.steps.length - 1)) },
      bubbles: true, composed: true,
    }));
  }

  _first() { this._emit(0); }
  _prev() { this._emit(this.index - 1); }
  _next() { this._emit(this.index + 1); }
  _last() { this._emit(this.steps.length - 1); }

  _togglePlay() {
    this.dispatchEvent(new CustomEvent("play-toggle", {
      bubbles: true, composed: true,
    }));
  }

  _speed(e) {
    this.speed = Number(e.target.value);
    this.dispatchEvent(new CustomEvent("speed-change", {
      detail: { speed: this.speed }, bubbles: true, composed: true,
    }));
  }
}

customElements.define("ssv-step-controls", SsvStepControls);
