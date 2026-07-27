(use-modules
  (blue build)
  (blue types blueprint)
  (blue types command)
  (blue types configuration)
  (blue types variable)
  (blueprint install)
  (blueprint buildables)
  (blueprint tests))

(define ssv-variables
  (append
   (make-package-variables
     #:name "ssv"
     #:version "0.1.0")
   (list
    (variable
      (name "builddir")
      (value (const "build"))))))

(define ssv-configuration
  (configuration (variables ssv-variables)))

(blueprint
  (configuration ssv-configuration)
  (buildables ssv-modules)
  (testables ssv-tests)
  (commands
    (list install-command)))
