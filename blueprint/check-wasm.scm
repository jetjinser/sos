;;; blueprint/check-wasm.scm
;;; A blue command that runs the model test suites inside the Hoot Wasm VM.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (blueprint check-wasm)
  #:use-module (blue computation)
  #:use-module (blue types command)
  #:export (check-wasm-command))

(define-command (check-wasm-command _)
  ((invoke "check-wasm")
   (category 'test)
   (synopsis "Run the model test suites in the Hoot Wasm VM")
   (help "Compile tests/wasm/model.scm to WebAssembly with Guile Hoot and run it inside the Hoot virtual machine (headless)."))
  (let* ((srcdir (run-computation #%~#%?srcdir))
         (status (system* "guild" "compile-wasm"
                          "-L" srcdir
                          "-L" (string-append srcdir "/model")
                          "--run"
                          (string-append srcdir "/tests/wasm/model.scm"))))
    (unless (zero? (status:exit-val status))
      (error "model Wasm tests failed"))))
