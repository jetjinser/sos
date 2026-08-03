import { LitElement, html, css, nothing } from "lit";
import { keyed } from "lit/directives/keyed.js";

// Structural expansion bookkeeping (id/app and the trivial value rules) drowns
// out the meaningful steps in a syntax-rules trace.  A step is "key" if it is a
// hygiene op, a binder/macro rule, or an identifier resolving to a binder.
const NOISE_RULES = new Set([
  "app", "syntax", "quote", "literal", "stops", "value", "fun-app", "prim-app",
]);

export function isKeyStep(step) {
  if (step.type === "op") return true;
  if (step.rule === "id") return !!(step.info && "tvar" in step.info);
  return !NOISE_RULES.has(step.rule);
}

// A video-editor style timeline for the expansion trace.  Each step is a clip
// (rules tall + cool, ops short + warm) labelled with its name — full when the
// clip is wide, elided when dense.  A playhead glides across; click to seek,
// drag to scrub.  The wheel zooms in/out (anchored under the cursor) to a
// window of steps for precise navigation on dense traces; the window follows
// the playhead as it advances, and double-click (or the zoom badge) resets to
// the full overview.  Below the strip a fixed readout names the hovered (or
// current) step and lists its info.
export class SsvStepScrubber extends LitElement {
  static properties = {
    steps: { type: Array },
    index: { type: Number },
    playing: { type: Boolean },
    focusKey: { type: Boolean },
    _hover: { state: true },
    _dragging: { state: true },
    _zoom: { state: true },
    _panStart: { state: true },
  };

  static styles = css`
    :host { display: block; font-family: inherit; }
    .wrap { position: relative; padding-top: 12px; }

    /* ---- clip strip ---- */
    .track {
      position: relative;
      display: flex; align-items: center; gap: 2px;
      height: 32px;
      padding: 0 3px;
      border-radius: 6px;
      background: hsl(220 25% 95% / 0.85);
      border: 1px solid hsl(220 18% 88%);
      box-shadow: inset 0 1px 2px hsl(220 30% 40% / 0.06);
      cursor: pointer;
      touch-action: none;
    }
    .seg {
      flex: 1 1 0; min-width: 3px;
      container-type: inline-size;
      display: flex; align-items: center; justify-content: center;
      overflow: hidden;
      border-radius: 3px;
      background: var(--bg);
      transition: opacity 160ms ease, filter 160ms ease,
                  background-color 160ms ease, transform 160ms ease;
    }
    .seg.rule { --bg: hsl(215 62% 90%); --fg: hsl(215 55% 36%); height: 24px; }
    .seg.op   { --bg: hsl(22 62% 90%);  --fg: hsl(22 55% 36%);  height: 17px; }
    .seg .name {
      font-size: 0.58rem; font-weight: 600;
      color: var(--fg);
      white-space: nowrap; overflow: hidden; text-overflow: ellipsis;
      padding: 0 3px;
    }
    /* too narrow to say anything useful — let the clip speak by colour */
    @container (max-width: 22px) { .name { display: none; } }
    .seg.unplayed { opacity: 0.34; filter: saturate(0.4); }
    .seg:hover { filter: brightness(1.08); }
    .seg.cur {
      background: var(--fg);
      opacity: 1;
      transform: scaleY(1.12);
      box-shadow: 0 0 0 1px hsl(0 0% 100%), 0 1px 8px color-mix(in oklab, var(--fg) 55%, transparent);
    }
    .seg.cur .name { color: #fff; }
    /* key-step focus: structural id/app clips shrink back so the binder/macro
       rules and hygiene ops stand out */
    .seg.rule.minor { height: 11px; }
    .seg.rule.minor:not(.cur) { opacity: 0.3; filter: saturate(0.35); }
    .seg.rule.minor .name { display: none; }

    /* ---- playhead: a lone marker above the strip — the lit clip below
       already carries the position, so no line through the clips ---- */
    .playhead {
      position: absolute;
      top: -10px;
      width: 12px; height: 9px;
      background: hsl(221 58% 44%);
      clip-path: polygon(0 0, 100% 0, 50% 100%);
      transform: translateX(-50%);
      transition: left 130ms ease;
      pointer-events: none;
    }
    .dragging .playhead { transition: none; }
    .playing .playhead { animation: ph-glow 1s ease-in-out infinite; }
    @keyframes ph-glow {
      0%, 100% { filter: drop-shadow(0 0 2px hsl(221 58% 44% / 0.55)); }
      50%      { filter: drop-shadow(0 0 7px hsl(221 58% 44% / 0.95)); }
    }

    /* ---- readout ---- */
    .readout {
      display: flex; align-items: center;
      min-height: 24px;
      margin-top: 6px;
      padding: 0 2px;
      font-size: 0.7rem;
    }
    .r-left {
      display: flex; align-items: center; gap: 0.55rem;
      min-width: 0;
    }
    .r-idx {
      font-size: 0.62rem; color: hsl(220 10% 55%);
      font-variant-numeric: tabular-nums;
    }
    .r-badge {
      display: inline-flex; align-items: stretch;
      border-radius: 3px; overflow: hidden;
      font-weight: 700;
    }
    .r-badge .r-kind {
      display: inline-flex; align-items: center;
      font-size: 0.55rem; letter-spacing: 0.07em; text-transform: uppercase;
      padding: 0 5px; color: #fff; background: var(--cat);
    }
    .r-badge .r-name {
      display: inline-flex; align-items: center;
      padding: 1px 7px;
      color: var(--cat);
      background: color-mix(in oklab, var(--cat) 12%, white);
    }
    .r-badge.rule { --cat: #246; }
    .r-badge.op   { --cat: #642; }
    .r-info {
      display: inline-flex; align-items: center; gap: 0.5rem;
      color: hsl(220 12% 48%);
      font-size: 0.64rem;
      overflow: hidden; white-space: nowrap;
    }
    .r-info b { color: hsl(220 15% 38%); font-weight: 600; }
    .r-empty { color: hsl(220 10% 62%); font-style: italic; }
    .r-in { animation: r-in 160ms ease-out; }
    @keyframes r-in {
      from { opacity: 0; transform: translateY(3px); }
      to   { opacity: 1; transform: none; }
    }
    .r-right {
      margin-left: auto;
      display: flex; align-items: center; gap: 6px;
      flex-shrink: 0;
    }
    .focus-toggle {
      font-family: inherit; font-size: 0.6rem; font-weight: 700;
      letter-spacing: 0.05em; text-transform: uppercase;
      padding: 2px 9px;
      border: 1px solid hsl(220 20% 82%);
      border-radius: 10px;
      background: hsl(220 30% 97%);
      color: hsl(220 25% 42%);
      cursor: pointer;
      transition: background-color 120ms ease, border-color 120ms ease,
                  color 120ms ease;
    }
    .focus-toggle:hover { background: hsl(220 45% 93%); border-color: hsl(220 35% 68%); }
    .focus-toggle.on {
      background: hsl(221 58% 44%); border-color: hsl(221 58% 40%);
      color: #fff;
    }
    .zoom-badge {
      flex-shrink: 0;
      font-family: inherit; font-size: 0.6rem; font-weight: 600;
      font-variant-numeric: tabular-nums;
      padding: 2px 9px;
      border: 1px solid hsl(220 20% 82%);
      border-radius: 10px;
      background: hsl(220 30% 97%);
      color: hsl(220 25% 42%);
      cursor: pointer;
      transition: background-color 120ms ease, border-color 120ms ease;
    }
    .zoom-badge:hover {
      background: hsl(220 45% 93%);
      border-color: hsl(220 35% 68%);
    }
  `;

  constructor() {
    super();
    this.steps = [];
    this.index = 0;
    this.playing = false;
    this.focusKey = false;
    this._hover = null;
    this._dragging = false;
    this._zoom = 1;
    this._panStart = 0;
  }

  // ---- zoom window -------------------------------------------------------

  _maxZoom() {
    return Math.max(1, Math.ceil(this.steps.length / 5));
  }

  _windowSize() {
    return Math.max(1, Math.ceil(this.steps.length / this._zoom));
  }

  _clampedStart(ws) {
    const n = this.steps.length;
    return Math.min(Math.max(0, this._panStart), Math.max(0, n - ws));
  }

  // Keep the playhead inside the window as it moves (minimal slide).  Only
  // on index/steps changes — a wheel zoom keeps its cursor anchor instead.
  willUpdate(changed) {
    if (changed.has("steps")) {
      this._zoom = 1;
      this._panStart = 0;
    }
    if (changed.has("index") || changed.has("steps")) {
      const ws = this._windowSize();
      if (this.index < this._panStart) this._panStart = this.index;
      else if (this.index >= this._panStart + ws) this._panStart = this.index - ws + 1;
      this._panStart = this._clampedStart(ws);
    }
  }

  _resetZoom() {
    this._zoom = 1;
    this._panStart = 0;
  }

  // Wheel zoom, anchored so the step under the cursor stays put.
  _wheel(e) {
    e.preventDefault();
    const n = this.steps.length;
    if (!n) return;
    const track = this.renderRoot.querySelector(".track");
    const rect = track.getBoundingClientRect();
    const frac = Math.min(1, Math.max(0, (e.clientX - rect.left) / rect.width));
    const ws = this._windowSize();
    const start = this._clampedStart(ws);
    const visCount = Math.min(ws, n - start);
    const anchor = start + frac * visCount;
    const factor = e.deltaY < 0 ? 1.35 : 1 / 1.35;
    const zoom = Math.min(this._maxZoom(), Math.max(1, this._zoom * factor));
    const nws = Math.max(1, Math.ceil(n / zoom));
    this._zoom = zoom;
    this._panStart = Math.min(Math.max(0, Math.round(anchor - frac * nws)),
                              Math.max(0, n - nws));
  }

  // ---- render ------------------------------------------------------------

  render() {
    const n = this.steps.length;
    if (!n) return nothing;
    const ws = this._windowSize();
    const start = this._clampedStart(ws);
    const end = Math.min(n, start + ws);
    const visCount = end - start;
    const inView = this.index >= start && this.index < end;
    const pos = inView ? ((this.index - start + 0.5) / visCount) * 100 : null;
    const wrapCls = `wrap${this._dragging ? " dragging" : ""}${this.playing ? " playing" : ""}`;
    return html`
      <div class=${wrapCls}>
        <div class="track"
             @pointerdown=${this._down}
             @pointermove=${this._move}
             @pointerup=${this._up}
             @pointercancel=${this._up}
             @mouseleave=${this._leave}
             @wheel=${this._wheel}
             @dblclick=${this._resetZoom}>
          ${this.steps.slice(start, end).map((s, j) => this._seg(s, start + j))}
          ${pos != null ? html`<div class="playhead" style="left:${pos}%"></div>` : nothing}
        </div>
        ${this._readout(start, end, ws < n)}
      </div>`;
  }

  _seg(step, i) {
    const isRule = step.type === "rule";
    const minor = this.focusKey && !isKeyStep(step);
    const state = i === this.index ? "cur" : i > this.index ? "unplayed" : "";
    const name = isRule ? step.rule : step.op;
    return html`<div class="seg ${isRule ? "rule" : "op"}${minor ? " minor" : ""} ${state}"
                     @mouseenter=${() => { if (!this._dragging) this._hover = i; }}>
      <span class="name">${name}</span>
    </div>`;
  }

  // The step the eye is on: whichever clip is hovered, else the playhead.
  _readout(start, end, zoomed) {
    const n = this.steps.length;
    const i = this._hover ?? this.index;
    const step = this.steps[i];
    return html`<div class="readout">
      <div class="r-left">
        ${step
          ? keyed(i, this._stepInfo(step, i, n))
          : html`<span class="r-empty">—</span>`}
      </div>
      <div class="r-right">
        <button class="focus-toggle ${this.focusKey ? "on" : ""}"
                @click=${this._toggleFocus}
                title="step through key steps only (skip id/app bookkeeping)">key</button>
        ${zoomed
          ? html`<button class="zoom-badge" @click=${this._resetZoom}
                         title="double-click the track to reset">
              ${this._zoom.toFixed(1)}× · ${start + 1}–${end} / ${n}
            </button>`
          : nothing}
      </div>
    </div>`;
  }

  _toggleFocus() {
    this.dispatchEvent(new CustomEvent("focus-change", {
      detail: { focus: !this.focusKey }, bubbles: true, composed: true,
    }));
  }

  _stepInfo(step, i, n) {
    const isRule = step.type === "rule";
    const name = isRule ? step.rule : step.op;
    const info = Object.entries(step.info ?? {});
    return html`<span class="r-in" style="display:contents">
      <span class="r-idx">${i + 1} / ${n}</span>
      <span class="r-badge ${isRule ? "rule" : "op"}">
        <span class="r-kind">${isRule ? "rule" : "op"}</span>
        <span class="r-name">${name}</span>
      </span>
      ${info.length
        ? html`<span class="r-info">${info.map(([k, v]) =>
            html`<span><b>${k}</b>=${typeof v === "object" ? JSON.stringify(v) : v}</span>`)}
          </span>`
        : nothing}
    </span>`;
  }

  _indexFromEvent(e) {
    const track = this.renderRoot.querySelector(".track");
    const rect = track.getBoundingClientRect();
    const frac = Math.min(1, Math.max(0, (e.clientX - rect.left) / rect.width));
    const n = this.steps.length;
    const ws = this._windowSize();
    const start = this._clampedStart(ws);
    const visCount = Math.min(ws, n - start);
    return Math.min(start + visCount - 1, start + Math.floor(frac * visCount));
  }

  _seek(i) {
    this._hover = i;
    if (i !== this.index) {
      this.dispatchEvent(new CustomEvent("step-to", {
        detail: { index: i }, bubbles: true, composed: true,
      }));
    }
  }

  _down(e) {
    this._dragging = true;
    e.currentTarget.setPointerCapture(e.pointerId);
    this._seek(this._indexFromEvent(e));
  }

  _move(e) {
    if (this._dragging) this._seek(this._indexFromEvent(e));
  }

  _up(e) {
    this._dragging = false;
    if (e.currentTarget.hasPointerCapture?.(e.pointerId))
      e.currentTarget.releasePointerCapture(e.pointerId);
  }

  _leave() {
    if (!this._dragging) this._hover = null;
  }
}

customElements.define("ssv-step-scrubber", SsvStepScrubber);
