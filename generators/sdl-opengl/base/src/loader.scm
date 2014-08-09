(cond-expand
 (ios
  ;; SchemeSpheres initialization
  (##spheres-init)
  ;; Preload modules
  (##spheres-include-eval '((core: base-macros)
                            (core: assert-macros)))
  (eval '(add-cond-expand-feature! ios))
  (##spheres-load-remote "localhost:8000"
                         '((= globals)
                           (= engine-types)))
  ;; Make procedure that runs the app
  (define (go)
    (##spheres-load-remote "localhost:8000"
                           '((= gl-utils)
                             (= engine)
                             (= app)))))
 (host
  (define (go)
    (##spheres-load app))))

(SDL_Log (object->string (##cond-expand-features)))

;; Spawn a REPL or just run interpreted
(cond-expand
 (ios
  ;; Spawn a REPL
  (if (remote-repl-setup! "localhost" port: 20000)
      (begin
        (remote-repl-run!)
        (SDL_Log "***** Successfully connected to Gambit Debug Server *****"))
      (SDL_Log "***** Unable to connect to Gambit Debug Server. Are you running 'sense'? *****"))
  ;; Wait until the thread receives a message to leave
  (define *main-thread* (current-thread))
  (thread-receive))
 (host
  (##spheres-load app)))


