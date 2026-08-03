import { LitElement, html, css } from "lit";
import { isKeyStep } from "./step-scrubber.js";

export class SsvStepControls extends LitElement {
  static properties = {
    steps: { type: Array },
    index: { type: Number },
    playing: { type: Boolean },
    speed: { type: Number },
    focusKey: { type: Boolean },
  };

  static styles = css`
    :host {
      display: flex; align-items: center; gap: 0.55rem;
      font-family: inherit; font-size: 0.82rem;
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
    button.nav { font-size: 1.05rem; line-height: 1; padding: 0.4rem 0.65rem; }
    button.nav .step-glyph { font-size: 1.25rem; font-weight: 700; line-height: 0.7; }
    button.nav:hover:not(:disabled) {
      background: hsl(220 45% 95%);
      transform: translateY(-1px);
      box-shadow: 0 1px 3px hsl(220 40% 30% / 0.18);
    }
    button.nav:active:not(:disabled) { transform: translateY(0); }
    button.play {
      width: 2.35em; height: 2.35em;
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
    select { font-family: inherit; font-size: 0.78rem; }
  `;

  constructor() {
    super();
    this.steps = [];
    this.index = 0;
    this.playing = false;
    this.speed = 500;
    this.focusKey = false;
  }

  render() {
    const n = this.steps.length;
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
    `;
  }

  _emit(index) {
    this.dispatchEvent(new CustomEvent("step-to", {
      detail: { index: Math.max(0, Math.min(index, this.steps.length - 1)) },
      bubbles: true, composed: true,
    }));
  }

  _first() { this._emit(this.focusKey ? this._edgeKey(1) : 0); }
  _prev() { this._emit(this.focusKey ? this._nextKey(this.index, -1) : this.index - 1); }
  _next() { this._emit(this.focusKey ? this._nextKey(this.index, 1) : this.index + 1); }
  _last() { this._emit(this.focusKey ? this._edgeKey(-1) : this.steps.length - 1); }

  _nextKey(i, dir) {
    const n = this.steps.length;
    let j = i + dir;
    while (j >= 0 && j < n && !isKeyStep(this.steps[j])) j += dir;
    return j >= 0 && j < n ? j : i;
  }

  // First (dir=1) or last (dir=-1) key step.
  _edgeKey(dir) {
    const n = this.steps.length;
    let j = dir > 0 ? 0 : n - 1;
    while (j >= 0 && j < n && !isKeyStep(this.steps[j])) j += dir;
    return j >= 0 && j < n ? j : this.index;
  }

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
