;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; SFusion: generate Scheme Spheres projects from templates

(define templates-directory
  (make-parameter "~~spheres/fusion/templates/"))

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

(define (directory-dirs path)
  (filter (lambda (f) (eq? 'directory (file-type (string-append (path-strip-trailing-directory-separator path) "/" f))))
          (directory-files path)))

(define (create-project name template platforms)
  (let ((template-dir (string-append (templates-directory) template "/")))
    (if (file-exists? template-dir)
        (for-each pp (directory-dirs template-dir))))
  ;; (if (file-exists? project-name)
  ;;                   (exit error:file-exists)
  ;;                   (create-directory project-name))
  ;; (create-directory (string-append project-name "/doc"))
  ;; (create-directory (string-append project-name "/src"))
  ;; (create-directory (string-append project-name "/test"))
  ;; (call-with-output-file
  ;;     (string-append project-name "/sakefile.scm")
  ;;   (lambda (f) (pp sakefile f)))
  ;; (call-with-output-file
  ;;                   (string-append project-name "/README.md")
  ;;                 (lambda (f) (display "" f)))
  ;; (process-template "text-template" (string-append project-name "/config.scm"))
  )

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
              (directory-dirs (templates-directory))))
   ;; Command: platforms
   ((string=? (cadr (command-line)) "platforms")
    (when (null? (cddr (command-line)))
          (println "Missing argument: template")
          (exit error:invalid-argument))
    (let* ((template (caddr (command-line)))
           (template-dir (string-append (templates-directory) template "/")))
      (unless (file-exists? template-dir)
              (println "Platform doesn't exist. Try \"sfusion templates\" command for a list of available templates.")
              (exit error:invalid-argument))
      (println "Available platforms for template " template ":")
      (for-each (lambda (d) (display "  - ") (println d))
                (directory-dirs template-dir))))
   ;; Unrecognized command
   (else
    (println "sfusion: unrecognized command. Try \"sfusion help\" for more information. ")
    (exit error:invalid-argument)))
  (exit error:success))

(main)

