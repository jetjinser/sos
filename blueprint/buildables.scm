(define-module (blueprint buildables)
  #:use-module (blue build)
  #:use-module (blue stencils guile)
  #:use-module (blueprint stencils hoot)
  #:export (ssv-modules model-wasm-tests))

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
