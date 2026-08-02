(define-module (blueprint buildables)
  #:use-module (blue build)
  #:use-module (blue stencils copy-file)
  #:use-module (blue stencils guile)
  #:use-module (blueprint stencils hoot)
  #:use-module (blueprint stencils esbuild)
  #:export (ssv-modules model-wasm-tests app-wasm web-bundle web-static))

(define +ssv-sources+
  '())

(define ssv-modules
  (map
   (λ (source)
     (guile-module
       (inputs source)
       (outputs (->.go source))
       (load-path #%~(list #%?srcdir))
       (optimizations
         '(#:seal-private-bindings? #t))))
   +ssv-sources+))

;;; Model sources the Wasm test runner depends on (for rebuild tracking).
(define +model-sources+
  '("model/core-model.scm"
    "model/phases-model.scm"
    "model/local-model.scm"
    "model/defs-model.scm"
    "ssv/emit.scm"
    "ssv/source.scm"
    "tests/unit/model/harness.scm"))

;;; Compile the model test suites to a WebAssembly module via Guile Hoot.
;;; Execute it with the `check-wasm' command (guild compile-wasm --run).
(define model-wasm-tests
  (hoot-wasm
    (inputs "tests/wasm/model.scm")
    (requirements +model-sources+)
    (outputs "model-tests.wasm")
    (load-paths #%~(list #%?srcdir (string-append #%?srcdir "/model")))
    (bundle? #f)))

;;; Dependencies of the web application's Wasm module (for rebuild tracking).
(define +app-deps+
  '("ssv/trace.scm"
    "ssv/serialize.scm"
    "ssv/source.scm"
    "ssv/emit.scm"
    "ssv/annotate.scm"
    "ssv/format.scm"
    "model/core-model.scm"
    "model/phases-model.scm"
    "model/local-model.scm"
    "model/defs-model.scm"))

;;; The web application: main.scm compiled to Wasm, with the Hoot web runtime
;;; bundled alongside (web/app.wasm + reflect.js/wasm + wtf8.wasm).
(define app-wasm
  (hoot-wasm
    (inputs "ssv/main.scm")
    (requirements +app-deps+)
    (outputs "web/app.wasm")
    (load-paths #%~(list #%?srcdir (string-append #%?srcdir "/model")))
    (bundle? #t)))

;;; Bundle the front-end application (Lit components + wasm bridge) via esbuild.
(define +web-sources+
  '("web/src/wasm.js"
    "web/src/components/code-input.js"
    "web/src/components/stx-tree.js"
    "web/src/components/store-panel.js"
    "web/src/components/step-controls.js"
    "web/src/components/source-view.js"
    "web/src/components/feature-bar.js"
    "web/src/components/step-detail.js"
    "web/src/components/step-scrubber.js"
    "web/src/components/step-compare.js"
    "web/src/components/ast-view.js"
    "web/src/lib/scope-colors.js"
    "web/src/lib/sexpr.js"
    "web/src/lib/wasm-features.js"
    "package-lock.json"))

(define web-bundle
  (esbuild-bundle
    (inputs "web/src/app.js")
    (requirements +web-sources+)
    (outputs "web/app.js")))

;;; Static front-end files copied next to the bundle into the served directory.
(define +web-static+
  '("web/index.html"))

(define web-static
  (map (λ (file)
         (copy-file
           (inputs file)
           (outputs file)))
       +web-static+))
