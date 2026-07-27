import { init, runModel } from "./wasm.js";

const EXAMPLES = {
  core: "(let-syntax x (lambda z (syntax (quote 2))) (x 1))",
  phases:
    "(lambda z (let-syntax x (lambda s (MKS (LIST (syntax lambda) (syntax z) (CAR (CDR (SE s)))) (syntax here))) (x z)))",
};

const app = document.getElementById("app");

async function main() {
  try {
    await init();
  } catch (e) {
    if (e instanceof WebAssembly.CompileError) {
      document.getElementById("wasm-error").hidden = false;
      app.textContent = "";
      return;
    }
    throw e;
  }

  app.innerHTML = `
    <div class="row">
      <label>Model:
        <select id="model">
          <option value="core">core</option>
          <option value="phases">phases</option>
        </select>
      </label>
      <button id="run">Run</button>
    </div>
    <div class="row"><textarea id="input" spellcheck="false"></textarea></div>
    <div class="row"><pre id="output"></pre></div>
  `;

  const modelSel = document.getElementById("model");
  const input = document.getElementById("input");
  const output = document.getElementById("output");
  const runBtn = document.getElementById("run");

  input.value = EXAMPLES[modelSel.value];
  modelSel.addEventListener("change", () => {
    input.value = EXAMPLES[modelSel.value];
  });

  runBtn.addEventListener("click", () => {
    try {
      const trace = runModel(modelSel.value, input.value);
      output.textContent =
        `model: ${trace.model}\n` +
        `steps: ${trace.steps.length}\n` +
        `final-ast: ${JSON.stringify(trace["final-ast"])}\n\n` +
        JSON.stringify(trace, null, 2);
    } catch (e) {
      output.textContent = "Error: " + (e && e.message ? e.message : e);
    }
  });

  runBtn.click();
}

main();
