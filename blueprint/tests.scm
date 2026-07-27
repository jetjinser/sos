(define-module (blueprint tests)
  #:use-module (blue build)
  #:use-module (blue stencils guile)
  #:use-module (blue types testable)
  #:export (ssv-tests))

(define test-options
  #%~(list
       #:load-path (list #%?srcdir)
       #:load-compiled-path (list #%?builddir)
       #:cov-filter-globs '("ssv/*")
       #:enable-coverage? (not (getenv "SSV_TESTS_DISABLE_COVERAGE"))
       #:time-limit (or (and=> (getenv "SSV_TESTS_TIME_LIMIT") string->number)
                        60)))

(define +test-sources+
  '("tests/unit/ssv/core.scm"))

(define ssv-tests
  (map
   (λ (script-name)
     (guile-test-suite
       (inputs script-name)
       (options test-options)))
   +test-sources+))
