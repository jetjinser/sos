import { LitElement, html, css, nothing } from "lit";
import { keyed } from "lit/directives/keyed.js";
import "./stx-tree.js";

// Macro-stepper style comparison: the syntax object before and after the
// current rewrite, side by side, with every node's scope set on display.
// Rule steps carry before/after directly; the stx-mutating ops carry them in
// data; binding/alloc ops have no syntax rewrite and show their payload.
export class SsvStepCompare extends LitElement {
  static properties = {
    step: { type: Object },
    index: { type: Number },
  };

  static styles = css`
    :host { display: block; font-family: inherit; height: 100%; min-height: 0; }
    .compare {
      display: flex; align-items: stretch; gap: 0.7rem;
      height: 100%; min-height: 0;
    }
    .pane {
      flex: 1 1 0; min-width: 0;
      display: flex; flex-direction: column;
      border: 1px solid hsl(220 18% 86%);
      border-radius: 8px;
      background: hsl(220 30% 99% / 0.7);
      overflow: hidden;
      animation: pane-in 220ms ease-out backwards;
    }
    .pane.after { animation-delay: 70ms; }
    @keyframes pane-in {
      from { opacity: 0; transform: translateY(5px); }
      to   { opacity: 1; transform: none; }
    }
    .pane-head {
      display: flex; align-items: center; gap: 0.45rem;
      padding: 0.35rem 0.7rem;
      border-bottom: 1px solid hsl(220 18% 90%);
      background: hsl(220 25% 97% / 0.8);
    }
    .tag {
      font-size: 0.58rem; font-weight: 700;
      letter-spacing: 0.12em; text-transform: uppercase;
    }
    .tag.before { color: hsl(220 12% 55%); }
    .tag.after  { color: hsl(145 45% 36%); }
    .pane-head .rule-name {
      font-size: 0.68rem; font-weight: 600; color: hsl(220 20% 40%);
      overflow: hidden; text-overflow: ellipsis; white-space: nowrap;
    }
    .pane-body {
      flex: 1; min-height: 0;
      overflow: auto;
      padding: 0.6rem 0.7rem;
    }
    .arrow {
      align-self: center;
      font-size: 1.5rem; font-weight: 700;
      color: hsl(220 18% 70%);
      flex-shrink: 0;
      animation: arrow-in 220ms ease-out backwards;
      animation-delay: 40ms;
    }
    @keyframes arrow-in {
      from { opacity: 0; transform: translateX(-5px); }
      to   { opacity: 1; transform: none; }
    }
    .payload {
      display: flex; align-items: center; justify-content: center;
      height: 100%;
      font-size: 0.8rem; color: hsl(220 15% 42%);
      text-align: center; padding: 1rem;
    }
    .payload b { color: hsl(220 25% 30%); }
    .empty { color: #99a; font-style: italic; }
  `;

  constructor() {
    super();
    this.step = null;
    this.index = 0;
  }

  render() {
    const step = this.step;
    if (!step) return html`<span class="empty">—</span>`;
    return keyed(this.index, this._body(step));
  }

  _body(step) {
    const rw = this._rewrite(step);
    if (!rw) {
      return html`<div class="compare">
        <div class="pane" style="flex:1">
          <div class="pane-head"><span class="tag before">op</span>
            <span class="rule-name">${step.op}</span></div>
          <div class="pane-body"><div class="payload">${this._payload(step)}</div></div>
        </div>
      </div>`;
    }
    const name = step.type === "rule" ? step.rule : step.op;
    return html`<div class="compare">
      <div class="pane before">
        <div class="pane-head"><span class="tag before">before</span>
          <span class="rule-name">${name}</span></div>
        <div class="pane-body"><ssv-stx-tree .stx=${rw.before}></ssv-stx-tree></div>
      </div>
      <div class="arrow">⟶</div>
      <div class="pane after">
        <div class="pane-head"><span class="tag after">after</span>
          <span class="rule-name">${name}</span></div>
        <div class="pane-body"><ssv-stx-tree .stx=${rw.after}></ssv-stx-tree></div>
      </div>
    </div>`;
  }

  _rewrite(step) {
    if (step.type === "rule") return { before: step.before, after: step.after };
    const d = step.data ?? [];
    if (["stx-add", "stx-flip", "stx-prune"].includes(step.op))
      return { before: d[1], after: d[2] };
    return null;
  }

  _payload(step) {
    const d = step.data ?? [];
    if (step.op === "bind") {
      return html`<span>bind <b>${d[0]}</b> at { ${(d[1] ?? []).join(" ")} } ⟶ <b>${d[2]}</b></span>`;
    }
    if (step.op === "alloc-name" || step.op === "alloc-scope") {
      return html`<span>${step.op} ⟶ <b>${d[0]}</b></span>`;
    }
    return html`<span>${JSON.stringify(d)}</span>`;
  }
}

customElements.define("ssv-step-compare", SsvStepCompare);
