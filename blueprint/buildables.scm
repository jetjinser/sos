(define-module (blueprint buildables)
  #:use-module (blue build)
  #:use-module (blue stencils guile)
  #:export (ssv-modules))

(define +ssv-sources+
  '("ssv/core/L.scm"))

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
