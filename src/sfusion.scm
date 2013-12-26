;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; SFusion: generate Scheme Spheres projects from generators

(define help:general
  "
Usage: sfusion [command] [flags] [operand]

Commands:
    help [command]
        show help for the command
    new [flags] [name-of-the-project]
        create new project
        --generator '-t' generator to use for project creation (required)
        --platform '-p' generate for this target platform (required)
    generators
        list available generators
    platforms [generator]
        list available platforms for given generator

")

(define help:new
  "
Use 'new' command to create a new project from scratch, using a generator

Usage: sfusion new [flags] [name-of-the-project]
    --generator '-t' generator to use for project creation (required)
    --platform '-p' generate for this target platform (required)

Example: sfusion new -t opengl -p linux -p android my-new-project
    Creates the \"my-new-project\" using generator \"opengl\" and targetting
    platforms \"Linux\" and \"Android\"

")

(define help:generators
  "
Use 'generators' command to list available generators

Usage: sfusion generators

")

(define help:platforms
  "
Use 'platforms' command to list the available platforms offered by a generator
Obtain the list of available generators with the 'generators' command

Usage: sfusion platforms [generator]

")

;;! Where generators are expected to be installed
(define generators-path
  (make-parameter "~~spheres/fusion/generators/"))

;;! Get a list of current subdirectories
(define (directory-subdirs path)
  (filter (lambda (f) (eq? 'directory (file-type (string-append (path-strip-trailing-directory-separator path) "/" f))))
          (directory-files path)))

;;! Recursively replicate the generator with its different platform implementations
;; A "common" platform is processed first for common code
(define (create-project name generator platforms)
  (let* ((generator-path (string-append (generators-path) generator "/"))
         (platform-paths (map (lambda (p) (string-append generator-path p "/"))
                              (cons "common" platforms)))
         (project-path (string-append (current-directory) name "/")))
    (unless (file-exists? generator-path)
            (println "Generator not available: please see available generators.")
            (exit error:no-such-file-or-directory))
    (unless (file-exists? (car platform-paths))
            (println "Generator has no \"common\" directory (generator is incomplete): please contact generator author.")
            (exit error:no-such-file-or-directory))
    (unless (every file-exists? platform-paths)
            (println "Platform not available: please see available generators.")
            (exit error:no-such-file-or-directory))
    (when (file-exists? name)
          (println "Aborting: project folder already exists")
          (exit error:file-exists))
    (create-directory project-path)
    (for-each
     (lambda (source-path)
       ((Y (lambda (recur) ; Anonymous recursion: call recur with the new relative-path argument
             (lambda (relative-path)
               (for-each
                (lambda (file)
                  (case (file-info-type (file-info (string-append source-path relative-path file)))
                    ((regular)
                     (let ((original-file (string-append source-path relative-path file))
                           (new-file (string-append project-path relative-path file)))
                       (when (file-exists? new-file)
                             (println
                              "Error creating project: generators for chosen platforms collide. Please contact generator author.")
                             (exit error:operation-not-permitted))
                       ;; Test whether it's a generator file to process
                       (if (string=? ".sct" (path-extension original-file))
                           (call-with-output-file
                               (path-strip-extension new-file)
                             (lambda (f) (display
                                     ((build-template-from-file (string-append source-path relative-path file) 'generators)
                                      (map string->symbol platforms))
                                     f)))
                           (copy-file original-file new-file))))
                    ((directory)
                     (create-directory (string-append project-path relative-path file))
                     (recur (string-append relative-path file "/")))))
                (directory-files (string-append source-path relative-path)))))) ""))
     platform-paths)
    (println (string-append "Project " name " succesfully created"))))

(define (main)
  (cond
   ((null? (cdr (command-line)))
    (display help:general))
   ;; Command: help
   ((string=? (cadr (command-line)) "help")
    (if (null? (cddr (command-line)))
        (display help:general)
        (let ((help-al `((new . ,help:new) (generators . ,help:generators) (platforms . ,help:platforms))))
          (display (or (aif it (assq (string->symbol (caddr (command-line))) help-al) (cdr it))
                       "No help for this command\n")))))
   ;; Command: new
   ((string=? (cadr (command-line)) "new")
    (args-fold-receive (project-name generator)
                       (args-fold (cddr (command-line))
                                  ;; Option processors
                                  (list (option '(#\g "generator") #t #f
                                                (lambda (option name arg generator)
                                                  (values arg generator))))
                                  ;; Unrecognized option processor
                                  (lambda (option name arg . seeds)
                                    (println (string-append "Unrecognized option: -" (string name)))
                                    (exit error:invalid-argument))
                                  ;; Operand processor: its output gets passed to receive
                                  (lambda (operand generator)
                                    (values operand generator))
                                  ;; Default argument values
                                  #f
                                  '())
                       (lambda (project-name generator)
                         (unless generator
                                 (println "Missing argument: generator")
                                 (exit error:invalid-argument)))
                       (lambda args
                         (println "Missing or malformed arguments. Try \"sfusion help\" for more information.")
                         (exit error:invalid-argument))
                       (create-project project-name generator #f)))
   ;; Command: generators
   ((string=? (cadr (command-line)) "generators")
    (println "Available generators:")
    (for-each (lambda (d) (display "  - ") (println d))
              (directory-subdirs (generators-path))))
   ;; Command: platforms
   ((string=? (cadr (command-line)) "platforms")
    (when (null? (cddr (command-line)))
          (println "Missing argument: generator")
          (exit error:invalid-argument))
    (let* ((generator (caddr (command-line)))
           (generator-path (string-append (generators-path) generator "/")))
      (unless (file-exists? generator-path)
              (println "Platform doesn't exist. Try \"sfusion generators\" command for a list of available generators.")
              (exit error:invalid-argument))
      (println "Available platforms for generator " generator ":")
      (for-each (lambda (d) (display "  - ") (println d))
                (remove (curry string=? "common") (directory-subdirs generator-path)))))
   ;; Unrecognized command
   (else
    (println "sfusion: unrecognized command. Try \"sfusion help\" for more information. ")
    (exit error:invalid-argument)))
  (exit error:success))

;; Run!
(main)
