;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; SFusion: generate Scheme Spheres projects from templates

(define help:general
  "
Usage: sfusion [command] [flags] [operand]

Commands:
    help [command]
        show help for the command
    new [flags] [name-of-the-project]
        create new project
        --template '-t' template to use for project creation (required)
        --platform '-p' generate for this target platform (required)
    templates
        list available templates
    platforms [template]
        list available platforms for given template

")

(define help:new
  "
Use 'new' command to create a new project from scratch, using a template

Usage: sfusion new [flags] [name-of-the-project]
    --template '-t' template to use for project creation (required)
    --platform '-p' generate for this target platform (required)

Example: sfusion new -t opengl -p linux -p android my-new-project
    Creates the \"my-new-project\" using template \"opengl\" and targetting
    platforms \"Linux\" and \"Android\"

")

(define help:templates
  "
Use 'templates' command to list available templates

Usage: sfusion templates

")

(define help:platforms
  "
Use 'platforms' command to list the available platforms offered by a template
Obtain the list of available templates with the 'templates' command

Usage: sfusion platforms [template]

")

;;! Where templates are expected to be installed
(define templates-path
  (make-parameter "~~spheres/fusion/templates/"))

;;! Get a list of current subdirectories
(define (directory-subdirs path)
  (filter (lambda (f) (eq? 'directory (file-type (string-append (path-strip-trailing-directory-separator path) "/" f))))
          (directory-files path)))

;;! Recursively replicate the template with its different platform implementations
;; A "common" platform is processed first for common code
(define (create-project name template platforms)
  (let* ((template-path (string-append (templates-path) template "/"))
         (platform-paths (map (lambda (p) (string-append template-path p "/"))
                              (cons "common" platforms)))
         (project-path (string-append (current-directory) name "/")))
    (unless (file-exists? template-path)
            (println "Template not available: please see available templates.")
            (exit error:no-such-file-or-directory))
    (unless (file-exists? (car platform-paths))
            (println "Template has no \"common\" directory (template is incomplete): please contact template author.")
            (exit error:no-such-file-or-directory))
    (unless (every file-exists? platform-paths)
            (println "Platform not available: please see available templates.")
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
                     (let ((new-file (string-append project-path relative-path file)))
                       (when (file-exists? new-file)
                             (println
                              "Error creating project: templates for chosen platforms collide. Please contact template author.")
                             (exit error:operation-not-permitted))
                       (call-with-output-file
                           new-file
                         (lambda (f) (display
                                 ((build-template-from-file (string-append source-path relative-path file) 'platforms)
                                  (map string->symbol platforms))
                                 f)))))
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
        (let ((help-al`((new . ,help:new) (templates . ,help:templates) (platforms . ,help:platforms))))
          (display (or (aif it (assq (string->symbol (caddr (command-line))) help-al) (cdr it))
                       "No help for this command\n")))))
   ;; Command: new
   ((string=? (cadr (command-line)) "new")
    (args-fold-receive (project-name template platforms)
                       (args-fold (cddr (command-line))
                                  ;; Option processors
                                  (list (option '(#\t "template") #t #f
                                                (lambda (option name arg template platforms)
                                                  (values arg platforms)))
                                        (option '(#\p "platform") #t #f
                                                (lambda (option name arg template platforms)
                                                  (values template (cons arg platforms)))))
                                  ;; Unrecognized option processor
                                  (lambda (option name arg . seeds)
                                    (println (string-append "Unrecognized option: -" (string name)))
                                    (exit error:invalid-argument))
                                  ;; Operand processor: its output gets passed to receive
                                  (lambda (operand template platforms)
                                    (values operand template platforms))
                                  ;; Default argument values
                                  #f
                                  '())
                       (lambda (project-name template platforms)
                         (unless template
                                 (println "Missing argument: template")
                                 (exit error:invalid-argument))
                         (when (null? platforms)
                               (println "Missing argument: platform")
                               (exit error:invalid-argument)))
                       (lambda args
                         (println "Missing or malformed arguments. Try \"sfusion help\" for more information.")
                         (exit error:invalid-argument))
                       (create-project project-name template platforms)))
   ;; Command: templates
   ((string=? (cadr (command-line)) "templates")
    (println "Available templates:")
    (for-each (lambda (d) (display "  - ") (println d))
              (directory-subdirs (templates-path))))
   ;; Command: platforms
   ((string=? (cadr (command-line)) "platforms")
    (when (null? (cddr (command-line)))
          (println "Missing argument: template")
          (exit error:invalid-argument))
    (let* ((template (caddr (command-line)))
           (template-path (string-append (templates-path) template "/")))
      (unless (file-exists? template-path)
              (println "Platform doesn't exist. Try \"sfusion templates\" command for a list of available templates.")
              (exit error:invalid-argument))
      (println "Available platforms for template " template ":")
      (for-each (lambda (d) (display "  - ") (println d))
                (remove (curry string=? "common") (directory-subdirs template-path)))))
   ;; Unrecognized command
   (else
    (println "sfusion: unrecognized command. Try \"sfusion help\" for more information. ")
    (exit error:invalid-argument)))
  (exit error:success))

;; Run!
(main)
