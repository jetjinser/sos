import { LitElement, html, css, nothing } from "lit";
import { keyed } from "lit/directives/keyed.js";

const RULE_HUES = {
  lambda: 212, "let-syntax": 248,
  "macro-invoke": 282,
  app: 198, "fun-app": 198, "prim-app": 188,
  id: 168,
  quote: 222, syntax: 262, literal: 222, stops: 222, value: 222,
  "LOCAL-VALUE": 158, "LOCAL-EXPAND": 302, "LOCAL-BINDER": 318,
  "NEW-DEFS": 338, "DEF-BIND": 352,
  BOX: 18, UNBOX: 32, "SET-BOX!": 4,
};

const OP_HUES = {
  "stx-add": 145, "stx-flip": 25, "stx-prune": 335,
  bind: 262, "alloc-name": 185, "alloc-scope": 85,
};

// Current-step readout: the rule/op badge plus the rule's info entries as
// key/value chips.  Lives on its own full-width row below the topbar, so
// long info never competes with the controls for space.
export class SsvStepStatus extends LitElement {
  static properties = {
    step: { type: Object },
    index: { type: Number },
  };

  static styles = css`
    :host {
      display: flex; align-items: center; gap: 0.6rem;
      flex-wrap: wrap;
      min-height: 2.1em;
      font-family: inherit; font-size: 0.82rem;
    }
    /* Category anchored by --cat (rule cool / op warm); per-step kind hue
       injected as --h; all three colors derived via color-mix */
    .badge {
      --h: 222;
      display: inline-flex; align-items: stretch;
      border-radius: 3px; overflow: hidden;
      font-weight: bold;
      animation: badge-in 180ms ease-out;
    }
    .badge.rule { --cat: #246; }
    .badge.op   { --cat: #642; }
    .badge .kind {
      display: inline-flex; align-items: center;
      font-size: 0.62rem; font-weight: 700;
      letter-spacing: 0.07em; text-transform: uppercase;
      padding: 0 0.4rem;
      color: #fff;
      background: color-mix(in oklab, var(--cat) 80%, hsl(var(--h) 75% 42%));
    }
    .badge .name {
      padding: 0.14rem 0.5rem;
      background: color-mix(in oklab, hsl(var(--h) 90% 95%) 80%, var(--cat));
      color: color-mix(in oklab, var(--cat) 42%, hsl(var(--h) 65% 28%));
    }
    .kv {
      display: inline-flex; align-items: baseline; gap: 0.4em;
      font-size: 0.72rem;
      padding: 0.16rem 0.5rem;
      border: 1px solid hsl(220 15% 88%);
      border-radius: 3px;
      background: hsl(220 25% 97% / 0.8);
      animation: chip-in 200ms ease-out backwards;
    }
    .kv:nth-child(3) { animation-delay: 30ms; }
    .kv:nth-child(4) { animation-delay: 60ms; }
    .kv:nth-child(5) { animation-delay: 90ms; }
    .kv:nth-child(n+6) { animation-delay: 120ms; }
    .kv .k {
      font-size: 0.58rem; font-weight: 700;
      letter-spacing: 0.08em; text-transform: uppercase;
      color: hsl(220 12% 55%);
    }
    .kv .v { font-weight: 600; color: hsl(220 30% 30%); }
    @keyframes badge-in {
      from { opacity: 0.2; transform: translateX(-4px); }
      to   { opacity: 1;    transform: none; }
    }
    @keyframes chip-in {
      from { opacity: 0; transform: translateY(3px); }
      to   { opacity: 1; transform: none; }
    }
  `;

  constructor() {
    super();
    this.step = null;
    this.index = 0;
  }

  render() {
    const step = this.step;
    if (!step) return nothing;
    return keyed(this.index, html`
      ${this._badge(step)}
      ${step.type === "rule" ? this._info(step) : nothing}
    `);
  }

  _badge(step) {
    if (step.type === "rule") {
      return html`
        <span class="badge rule"
              style="--h: ${RULE_HUES[step.rule] ?? 222}">
          <span class="kind">rule</span><span class="name">${step.rule}</span>
        </span>`;
    }
    if (step.type === "op") {
      return html`
        <span class="badge op" style="--h: ${OP_HUES[step.op] ?? 222}">
          <span class="kind">op</span><span class="name">${step.op}</span>
        </span>`;
    }
    return nothing;
  }

  _info(step) {
    return Object.entries(step.info ?? {}).map(([k, v]) => html`
      <span class="kv">
        <span class="k">${k}</span>
        <span class="v">${typeof v === "object" ? JSON.stringify(v) : v}</span>
      </span>`);
  }
}

customElements.define("ssv-step-status", SsvStepStatus);
