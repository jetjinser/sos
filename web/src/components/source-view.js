import { LitElement, html, css, nothing } from "lit";
import { scopeColor, scopeHue, scopeBg } from "../lib/scope-colors.js";
import { ctxScopes, tokenizeSource, atomKind } from "../lib/sexpr.js";

// Foreground palette for token kinds.
const FG = {
  keyword: "hsl(215 65% 42%)",
  prim: "hsl(168 60% 30%)",
  number: "hsl(28 75% 40%)",
  symbol: "hsl(222 22% 26%)",
  paren: "hsl(222 15% 58%)",
  comment: "hsl(140 25% 45%)",
  string: "hsl(340 60% 42%)",
  ws: "inherit",
};

const sameSet = (a, b) =>
  a.length === b.length && a.every((x) => b.includes(x));

export class SsvSourceView extends LitElement {
  static properties = {
    src: { type: String },
    snapshot: { type: Array },
    prevSnapshot: { type: Array },
    resolve: { type: Array },
    editable: { type: Boolean },
  };

  // Text metrics shared by the highlight layer and the editing textarea so
  // they stay perfectly aligned.
  static styles = css`
    :host { display: flex; flex-direction: column; font-family: inherit; height: 100%; min-height: 0; }

    /* ---- scope legend ---- */
    .legend {
      display: flex; flex-wrap: wrap; align-items: center;
      gap: 4px 8px;
      margin-bottom: 0.4rem; padding-bottom: 0.3rem;
      border-bottom: 1px dashed hsl(40 25% 80%);
    }
    .legend-title {
      font-size: 0.6rem; letter-spacing: 0.1em; text-transform: uppercase;
      color: hsl(38 22% 55%);
    }
    .lgd {
      display: inline-flex; align-items: center; gap: 4px;
      font-size: 0.66rem; color: hsl(222 20% 36%);
      padding: 1px 7px 1px 5px; border-radius: 8px;
      background: hsl(40 35% 96%); border: 1px solid hsl(40 25% 84%);
      cursor: default;
      transition: transform 120ms ease, box-shadow 120ms ease;
    }
    .lgd:hover, .lgd.on { box-shadow: 0 0 0 2px var(--scp); transform: translateY(-1px); }
    .lgd .sw { width: 9px; height: 9px; border-radius: 2px; background: var(--scp); }

    /* ---- editor shell ---- */
    .editor { position: relative; flex: 1; min-height: 0; }
    .highlight { position: absolute; inset: 0; overflow: hidden; }
    .editor.editing .highlight { pointer-events: none; }

    .input, .code {
      font-family: inherit;
      font-size: 0.86rem;
      line-height: 2.1;
      white-space: pre-wrap;
      overflow-wrap: break-word;
      /* generous top padding clears the floating chips above line 1 */
      padding: 1.1rem 0.5rem 0.4rem;
      margin: 0;
      border: none;
      box-sizing: border-box;
    }
    .input {
      position: absolute; inset: 0; width: 100%; height: 100%;
      resize: none; outline: none;
      background: transparent;
      color: transparent;
      caret-color: hsl(222 40% 30%);
      overflow: auto;
    }
    .input::selection { background: hsl(215 70% 80% / 0.45); }

    .code { color: hsl(222 22% 26%); min-height: 100%; }

    /* ---- segments ---- */
    .seg { border-radius: 2px; position: relative; }
    .kw { font-weight: 700; }
    .seg.tinted {
      background: var(--scp-bg);
      transition: background-color 220ms ease, filter 160ms ease;
    }
    .seg.fresh {
      outline: 2px solid var(--fresh-c, hsl(220 20% 60%));
      outline-offset: -1px;
      animation: fresh-in 550ms ease-out;
    }
    @keyframes fresh-in { from { opacity: 0.15; } to { opacity: 1; } }

    /* ---- floating badges (out of flow so the textarea stays aligned) ---- */
    .chip, .dtag {
      position: absolute;
      z-index: 3;
      font-size: 0.58rem; line-height: 1.4;
      padding: 0 4px; border-radius: 6px;
      white-space: nowrap;
      animation: pop-in 260ms cubic-bezier(0.34, 1.56, 0.64, 1);
    }
    .chip {
      left: 0; bottom: 100%; margin-bottom: 1px;
      color: #fff; background: var(--scp);
    }
    .dtag { left: 0; top: 100%; margin-top: 1px; }
    .dtag.add {
      color: var(--scp); border: 1px solid var(--scp);
      background: hsl(0 0% 100% / 0.92); font-weight: 700;
    }
    .dtag.rem {
      color: hsl(220 10% 55%); border: 1px solid hsl(220 12% 76%);
      background: hsl(0 0% 100% / 0.7); text-decoration: line-through;
    }

    /* Scope-set virtual text at end of line (eol): one chip per scope in a
       right-margin row, oldest first so fresh scopes grow in on the right.
       Absolute + out of flow so the textarea stays aligned */
    .shint-row {
      position: absolute;
      left: 100%; margin-left: 9px;
      top: 50%; transform: translateY(-50%);
      z-index: 3;
      display: inline-flex; align-items: center; gap: 3px;
      white-space: nowrap;
      pointer-events: none;
    }
    /* Interactive in read-only mode; while editing the textarea sits on top
       and chip hover is hit-tested geometrically instead */
    .shint {
      display: inline-flex; align-items: center;
      font-size: 0.6rem; line-height: 1.35; font-weight: 600;
      padding: 1px 6px;
      border-radius: 7px;
      color: var(--scp);
      background: var(--scp-bg);
      border: 1px solid color-mix(in oklab, var(--scp) 35%, var(--scp-bg));
      pointer-events: auto; cursor: pointer;
      transition: opacity 180ms ease, background-color 180ms ease,
                  color 180ms ease, box-shadow 180ms ease, transform 120ms ease;
      animation: shint-in 220ms ease-out;
    }
    .shint:hover { transform: translateY(-1px); }
    .shint.dim { opacity: 0.18; }
    .shint.on {
      color: #fff;
      background: var(--scp);
      box-shadow: 0 0 0 2px color-mix(in oklab, var(--scp) 25%, transparent),
                  0 2px 8px color-mix(in oklab, var(--scp) 35%, transparent);
    }
    @keyframes shint-in { from { opacity: 0; } to { opacity: 1; } }
    @keyframes pop-in {
      0% { opacity: 0; transform: scale(0.5); }
      100% { opacity: 1; transform: scale(1); }
    }

    .empty { color: #99a; font-style: italic; }
  `;

  constructor() {
    super();
    this.src = "";
    this.snapshot = null;
    this.prevSnapshot = null;
    this.resolve = null;
    this.editable = false;
    this._chipBoxes = null;
    this._focusScope = null;
    this._focusTimer = null;
  }

  disconnectedCallback() {
    super.disconnectedCallback();
    clearTimeout(this._focusTimer);
  }

  render() {
    // In editable mode the textarea must always exist so the user can (re)type
    // after clearing everything; only the read-only view shows a placeholder.
    if (!this.src && !this.editable) return html`<span class="empty">—</span>`;

    this._chipBoxes = null;
    const tokens = tokenizeSource(this.src);
    const spans = this.snapshot ?? [];
    const prevSpans = this.prevSnapshot ?? [];
    const chips = this._chips();
    const tags = this._deltaTags(spans, prevSpans);

    const points = this._boundaries(tokens, spans, prevSpans, chips, tags);
    const lineEnds = this._lineEndChips(tokens, spans);
    const segs = [];
    for (let k = 0; k < points.length - 1; k++) {
      const a = points[k];
      const b = points[k + 1];
      if (a >= b) continue;
      const scopes = this._ctxAt(spans, a, b);
      segs.push(
        this._segment(
          this.src.slice(a, b), tokens, spans, prevSpans, a, b,
          tags.get(a) ?? [], chips.get(b) ?? [], scopes,
          lineEnds.get(b) ?? null,
        ),
      );
    }
    const code = html`<div class="code">${segs}</div>`;
    const legend = this._legend(spans);

    if (!this.editable) return html`${legend}${code}`;

    return html`${legend}
      <div class="editor editing">
        <div class="highlight">${code}</div>
        <textarea class="input" spellcheck="false" wrap="soft"
                  @input=${this._onInput}
                  @scroll=${this._onScroll}
                  @mousemove=${this._hoverMove}
                  @mouseleave=${this._hoverLeave}></textarea>
      </div>`;
  }

  // Keep the textarea in sync without clobbering the caret while typing: only
  // assign when the value actually differs (e.g. an example was loaded).
  updated(changed) {
    if (!this.editable || !changed.has("src")) return;
    const ta = this.renderRoot.querySelector(".input");
    if (ta && ta.value !== this.src) ta.value = this.src;
  }

  firstUpdated() {
    const ta = this.renderRoot.querySelector(".input");
    if (ta && ta.value !== this.src) ta.value = this.src;
  }

  _onInput(e) {
    this.dispatchEvent(new CustomEvent("code-input", {
      detail: { value: e.target.value },
      bubbles: true, composed: true,
    }));
  }

  _onScroll(e) {
    const hl = this.renderRoot.querySelector(".highlight");
    if (hl) { hl.scrollTop = e.target.scrollTop; hl.scrollLeft = e.target.scrollLeft; }
  }

  // ---- legend ----------------------------------------------------------

  _legendScopes(spans) {
    const set = new Set();
    for (const [, , ctx] of spans) for (const s of ctxScopes(ctx)) set.add(s);
    return [...set].sort();
  }

  _legend(spans) {
    const scopes = this._legendScopes(spans);
    if (!scopes.length) return nothing;
    return html`<div class="legend">
      <span class="legend-title">scopes</span>
      ${scopes.map(
        (s) => html`<span
          class="lgd"
          data-scope=${s}
          style="--scp:${scopeColor(s)}"
          @mouseenter=${() => this._setFocus(s)}
          @mouseleave=${() => this._setFocus(null)}>
          <i class="sw"></i>${s}</span>`,
      )}
    </div>`;
  }

  // Bidirectional focus: legend chips call this on hover, and so do code
  // segments (directly in read-only mode, via caret hit-testing in editing
  // mode).  Clearing is deferred so crossing segment boundaries — where the
  // next mouseenter fires before the previous mouseleave — never flickers.
  _setFocus(scope) {
    clearTimeout(this._focusTimer);
    if (scope == null) {
      this._focusTimer = setTimeout(() => this._applyFocus(null), 40);
      return;
    }
    this._applyFocus(scope);
  }

  // Focus lives on the chips now: the focused scope's chips flip to solid
  // and the legend chip lights up, while every other chip recedes.  The
  // code keeps only its ambient tint — no per-hover deepening.
  _applyFocus(scope) {
    if (this._focusScope === scope) return;
    this._focusScope = scope;
    const root = this.renderRoot;
    for (const el of root.querySelectorAll(".lgd"))
      el.classList.toggle("on", scope != null && el.dataset.scope === scope);
    for (const el of root.querySelectorAll(".shint")) {
      const has = scope != null && el.dataset.scope === scope;
      el.classList.toggle("on", has);
      el.classList.toggle("dim", scope != null && !has);
    }
  }

  // ---- chip -> legend direction ------------------------------------------

  // Direct chip hover (read-only mode, where the highlight layer receives
  // pointer events).
  _chipEnter(e) {
    const scope = e.currentTarget.dataset.scope;
    if (scope) this._setFocus(scope);
  }

  _chipLeave() { this._setFocus(null); }

  // While editing, the textarea sits on top of the pointer-transparent
  // highlight layer, so chip hover is hit-tested geometrically: chip boxes
  // are cached in .code content coordinates (scroll-invariant) and the
  // cursor point is mapped into the same space.  Only chips trigger focus —
  // the code itself stays inert.
  _hoverMove(e) {
    const scope = this._chipAt(e.clientX, e.clientY);
    e.currentTarget.style.cursor = scope ? "pointer" : "";
    this._setFocus(scope);
  }

  _hoverLeave() { this._setFocus(null); }

  _chipAt(x, y) {
    const code = this.renderRoot.querySelector(".code");
    if (!code) return null;
    const base = code.getBoundingClientRect();
    if (!this._chipBoxes) {
      this._chipBoxes = [...this.renderRoot.querySelectorAll(".shint")].map((el) => {
        const r = el.getBoundingClientRect();
        return {
          scope: el.dataset.scope,
          left: r.left - base.left, top: r.top - base.top,
          right: r.right - base.left, bottom: r.bottom - base.top,
        };
      });
    }
    const px = x - base.left;
    const py = y - base.top;
    for (const b of this._chipBoxes) {
      if (px >= b.left && px < b.right && py >= b.top && py < b.bottom)
        return b.scope;
    }
    return null;
  }

  // ---- resolve chips (final binding) -----------------------------------

  _chips() {
    const map = new Map();
    const seen = new Set();
    for (const [start, end, name] of this.resolve ?? []) {
      const key = end + ":" + name;
      if (seen.has(key)) continue;
      seen.add(key);
      if (!map.has(end)) map.set(end, []);
      map.get(end).push(
        html`<span class="chip" style="--scp:${scopeColor(name)}"
                   title="resolves to ${name}">→ ${name}</span>`,
      );
    }
    return map;
  }

  // ---- step delta tags -------------------------------------------------

  _deltaTags(spans, prevSpans) {
    const key = (s, e) => s + "," + e;
    const prevMap = new Map();
    for (const [s, e, ctx] of prevSpans) prevMap.set(key(s, e), ctxScopes(ctx));
    const curMap = new Map();
    for (const [s, e, ctx] of spans) curMap.set(key(s, e), ctxScopes(ctx));

    const changed = [];
    for (const [s, e, ctx] of spans) {
      const now = ctxScopes(ctx);
      const before = prevMap.get(key(s, e)) ?? [];
      const added = now.filter((x) => !before.includes(x));
      const removed = before.filter((x) => !now.includes(x));
      if (added.length || removed.length) changed.push({ s, e, added, removed });
    }
    for (const [s, e, ctx] of prevSpans) {
      if (!curMap.has(key(s, e)) && ctxScopes(ctx).length)
        changed.push({ s, e, added: [], removed: ctxScopes(ctx) });
    }

    const topmost = changed.filter(
      (c) =>
        !changed.some(
          (o) => o !== c && o.s <= c.s && c.e <= o.e && (o.s < c.s || c.e < o.e),
        ),
    );

    const map = new Map();
    for (const c of topmost) {
      const tags = [
        ...c.added.map(
          (sc) => html`<span class="dtag add" style="--scp:${scopeColor(sc)}">+${sc}</span>`,
        ),
        ...c.removed.map(
          (sc) => html`<span class="dtag rem" style="--scp:${scopeColor(sc)}">−${sc}</span>`,
        ),
      ];
      if (!tags.length) continue;
      if (!map.has(c.s)) map.set(c.s, []);
      map.get(c.s).push(...tags);
    }
    return map;
  }

  // ---- segment rendering -----------------------------------------------

  _boundaries(tokens, spans, prevSpans, chips, tags) {
    const set = new Set([0, this.src.length]);
    for (const t of tokens) { set.add(t.start); set.add(t.end); }
    for (const [s, e] of spans) { set.add(s); set.add(e); }
    for (const [s, e] of prevSpans) { set.add(s); set.add(e); }
    for (const pos of chips.keys()) set.add(pos);
    for (const pos of tags.keys()) set.add(pos);
    return [...set].sort((x, y) => x - y);
  }

  _ctxAt(spans, a, b) {
    let best = null;
    let bestLen = Infinity;
    for (const [s, e, ctx] of spans) {
      if (s <= a && b <= e && e - s < bestLen) {
        best = ctx;
        bestLen = e - s;
      }
    }
    return best ? ctxScopes(best) : [];
  }

  // End-of-line "virtual text": the only place inline annotations can sit
  // without reflowing the textarea-aligned highlight (cf. Neovim's
  // virt_text_pos=eol, git-blame style).  A line's trailing characters are
  // often wrapper parens with empty ctx, so walk back from the last content
  // character to the nearest position carrying a scope set — that is the set
  // in effect at end of line.
  _lineEndChips(tokens, spans) {
    const map = new Map();
    const src = this.src;
    const isWs = (c) => c === " " || c === "\t" || c === "\n" || c === "\r";
    let lineStart = 0;
    for (let i = 0; i <= src.length; i++) {
      if (i !== src.length && src[i] !== "\n") continue;
      let last = i - 1;
      while (last >= lineStart && isWs(src[last])) last--;
      if (last >= lineStart) {
        let p = last;
        let scopes = [];
        while (p >= lineStart && !scopes.length) {
          scopes = this._ctxAt(spans, p, p + 1);
          p--;
        }
        if (scopes.length) map.set(last + 1, scopes);
      }
      lineStart = i + 1;
    }
    return map;
  }

  _tokenAt(tokens, pos) {
    for (const t of tokens) if (t.start <= pos && pos < t.end) return t;
    return null;
  }

  _segment(text, tokens, spans, prevSpans, a, b, startTags, endChips,
           scopesNow, setChip) {
    const tok = this._tokenAt(tokens, a);
    const kind = tok ? (tok.kind === "atom" ? atomKind(tok.text) : tok.kind) : "ws";
    const scopesPrev = this._ctxAt(prevSpans, a, b);

    let cls = "seg";
    if (kind === "keyword" || kind === "prim") cls += " kw";

    const style = [`color:${FG[kind] ?? FG.ws}`];
    let title = nothing;
    if (scopesNow.length) {
      cls += " tinted";
      const hue = scopeHue(scopesNow[0]);
      style.push(`--scp:${scopeColor(scopesNow[0])}`);
      style.push(`--scp-bg:hsl(${hue} 70% 88%)`);
      title = `scopes: ${scopesNow.join(" ")}`;
    }

    if (!sameSet(scopesNow, scopesPrev)) {
      cls += " fresh";
      const delta =
        scopesNow.find((x) => !scopesPrev.includes(x)) ??
        scopesPrev.find((x) => !scopesNow.includes(x));
      if (delta) style.push(`--fresh-c:${scopeColor(delta)}`);
    }

    const hint = setChip
      ? html`<span class="shint-row">
          ${[...setChip].reverse().map(
            (s) => html`<span class="shint" data-scope=${s}
                              title="scope: ${s}"
                              style="--scp:${scopeColor(s)};--scp-bg:${scopeBg(s)}"
                              @mouseenter=${this._chipEnter}
                              @mouseleave=${this._chipLeave}
                        >${s}</span>`,
          )}
        </span>`
      : nothing;

    return html`<span class=${cls} style=${style.join(";")} title=${title}
                      data-scopes=${scopesNow.length ? scopesNow.join(" ") : nothing}
      >${text}${startTags}${endChips}${hint}</span>`;
  }
}

customElements.define("ssv-source-view", SsvSourceView);
