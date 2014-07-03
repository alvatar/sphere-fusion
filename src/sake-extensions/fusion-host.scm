;;; Copyright (c) 2012-2014, Álvaro Castro-Castilla
;;; Extensions for Sake (Host platform: Linux / OSX)

;;------------------------------------------------------------------------------
;;!! Host platform

(define (fusion#host-run-interpreted main-module #!key
                                     (version '())
                                     (cond-expand-features '()))
  (let* ((features (cons 'host cond-expand-features))
         (code `((define-syntax syntax-rules-error
                   (syntax-rules ()
                     ((_) (0))))
                 (define-syntax cond-expand
                   (syntax-rules (and or not else ,@features)
                     ((cond-expand) (syntax-rules-error "Unfulfilled cond-expand"))
                     ((cond-expand (else body ...))
                      (begin body ...))
                     ((cond-expand ((and) body ...) more-clauses ...)
                      (begin body ...))
                     ((cond-expand ((and req1 req2 ...) body ...) more-clauses ...)
                      (cond-expand
                       (req1
                        (cond-expand
                         ((and req2 ...) body ...)
                         more-clauses ...))
                       more-clauses ...))
                     ((cond-expand ((or) body ...) more-clauses ...)
                      (cond-expand more-clauses ...))
                     ((cond-expand ((or req1 req2 ...) body ...) more-clauses ...)
                      (cond-expand
                       (req1
                        (begin body ...))
                       (else
                        (cond-expand
                         ((or req2 ...) body ...)
                         more-clauses ...))))
                     ((cond-expand ((not req) body ...) more-clauses ...)
                      (cond-expand
                       (req
                        (cond-expand more-clauses ...))
                       (else body ...)))
                     ,@(map
                        (lambda (cef)
                          `((cond-expand (,cef body ...) more-clauses ...)
                            (begin body ...)))
                        features)
                     ((cond-expand (feature-id body ...) more-clauses ...)
                      (cond-expand more-clauses ...))))
                 (##spheres-load ,main-module))))
    (gambit-eval-here code flags-string: "-:dar,h10000")))

(define (fusion#host-compile-exe exe-name main-module #!key
                                 (version '())
                                 (cond-expand-features '())
                                 (verbose #f))
  (sake#compile-to-exe exe-name (list main-module)
                       version: version
                       cond-expand-features: (cons 'host cond-expand-features)
                       verbose: verbose))
