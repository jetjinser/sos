;;; blueprint/release.scm
;;; A blue command that assembles minified, self-contained release artifacts.
;;; SPDX-License-Identifier: LGPL-3.0-or-later

(define-module (blueprint release)
  #:use-module (blue computation)
  #:use-module (blue types command)
  #:export (release-command))

;;; Static assets shipped alongside the bundle (taken from builddir/web).
(define release-assets
  '("reflect.js" "reflect.wasm" "wtf8.wasm" "index.html"))

;;; Wasm features Hoot actually emits.  Deliberately NOT --all-features: that
;;; pulls in experimental proposals (stack-switching etc.) browsers reject.
(define wasm-features
  '("--enable-gc" "--enable-tail-call" "--enable-reference-types"
    "--enable-exception-handling" "--enable-multivalue"
    "--enable-mutable-globals" "--enable-sign-ext"
    "--enable-bulk-memory" "--enable-extended-const"))

;;; Shrink the Hoot wasm with binaryen: -Oz for size, --strip-dwarf drops the
;;; ~120kb of debug sections (release builds don't need Scheme stack traces).
;;; Falls back to a plain copy when wasm-opt is unavailable or fails.
(define (optimize-wasm src dst)
  (let ((status (false-if-exception
                 (apply system* "wasm-opt" "-Oz" "--strip-dwarf"
                        (append wasm-features (list src "-o" dst))))))
    (if (and status (zero? (status:exit-val status)))
        (format #t "optimized wasm -> ~a~%" dst)
        (begin
          (false-if-exception (delete-file dst))
          (copy-file src dst)
          (format #t "wasm-opt unavailable/failed; copied unoptimized wasm~%")))))

(define-command (release-command _)
  ((invoke "release")
   (category 'build)
   (synopsis "Assemble minified release artifacts")
   (help "Bundle the front-end with esbuild --minify, shrink the Wasm with wasm-opt -Oz, and copy the static assets into a self-contained release directory (builddir/release). Requires `blue build` to have produced builddir/web first."))
  (let* ((srcdir   (run-computation #%~#%?srcdir))
         (builddir (run-computation #%~#%?builddir))
         (web-dir  (string-append builddir "/web"))
         (release  (string-append builddir "/release")))
    (unless (file-exists? (string-append web-dir "/app.wasm"))
      (error "builddir/web/app.wasm not found — run `blue build` first"))
    (system* "rm" "-rf" release)
    (mkdir release)
    (let ((status (system* "esbuild"
                           (string-append srcdir "/web/src/app.js")
                           "--bundle" "--format=esm" "--minify"
                           (string-append "--outfile=" release "/app.js"))))
      (unless (zero? (status:exit-val status))
        (error "esbuild --minify failed")))
    (optimize-wasm (string-append web-dir "/app.wasm")
                   (string-append release "/app.wasm"))
    (for-each (lambda (f)
                (copy-file (string-append web-dir "/" f)
                           (string-append release "/" f)))
              release-assets)
    (format #t "release artifacts written to ~a~%" release)))
