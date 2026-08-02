import { LitElement, html, css, nothing } from "lit";
import { keyed } from "lit/directives/keyed.js";

// A video-editor style timeline for the expansion trace.  Each step is a clip
// (rules tall + cool, ops short + warm) labelled with its name — full when the
// clip is wide, elided when dense.  A playhead glides across; click to seek,
// drag to scrub.  Below the strip a fixed readout names the hovered (or
// current) step and lists its info, so details have a steady home instead of
// a floating tooltip.
export class SsvStepScrubber extends LitElement {
  static properties = {
    steps: { type: Array },
    index: { type: Number },
    playing: { type: Boolean },
    _hover: { state: true },
    _dragging: { state: true },
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
      display: flex; align-items: center; gap: 0.55rem;
      min-height: 24px;
      margin-top: 6px;
      padding: 0 2px;
      font-size: 0.7rem;
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
  `;

  constructor() {
    super();
    this.steps = [];
    this.index = 0;
    this.playing = false;
    this._hover = null;
    this._dragging = false;
  }

  render() {
    const n = this.steps.length;
    if (!n) return nothing;
    const pos = ((this.index + 0.5) / n) * 100;
    const wrapCls = `wrap${this._dragging ? " dragging" : ""}${this.playing ? " playing" : ""}`;
    return html`
      <div class=${wrapCls}>
        <div class="track"
             @pointerdown=${this._down}
             @pointermove=${this._move}
             @pointerup=${this._up}
             @pointercancel=${this._up}
             @mouseleave=${this._leave}>
          ${this.steps.map((s, i) => this._seg(s, i))}
          <div class="playhead" style="left:${pos}%"></div>
        </div>
        ${this._readout()}
      </div>`;
  }

  _seg(step, i) {
    const isRule = step.type === "rule";
    const state = i === this.index ? "cur" : i > this.index ? "unplayed" : "";
    const name = isRule ? step.rule : step.op;
    return html`<div class="seg ${isRule ? "rule" : "op"} ${state}"
                     @mouseenter=${() => { if (!this._dragging) this._hover = i; }}>
      <span class="name">${name}</span>
    </div>`;
  }

  // The step the eye is on: whichever clip is hovered, else the playhead.
  _readout() {
    const n = this.steps.length;
    const i = this._hover ?? this.index;
    const step = this.steps[i];
    if (!step) return html`<div class="readout"><span class="r-empty">—</span></div>`;
    const isRule = step.type === "rule";
    const name = isRule ? step.rule : step.op;
    const info = Object.entries(step.info ?? {});
    return html`<div class="readout">
      ${keyed(i, html`<span class="r-in" style="display:contents">
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
      </span>`)}
    </div>`;
  }

  _indexFromEvent(e) {
    const track = this.renderRoot.querySelector(".track");
    const rect = track.getBoundingClientRect();
    const frac = Math.min(1, Math.max(0, (e.clientX - rect.left) / rect.width));
    return Math.min(this.steps.length - 1, Math.floor(frac * this.steps.length));
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
