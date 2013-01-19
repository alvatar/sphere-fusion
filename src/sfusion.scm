;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; SFusion: generate Scheme Spheres multiplatform programs

(define templates-directory
  (make-parameter "~~spheres/fusion/templates/"))

(define (display-help)
  (display
   "
Usage: sfusion [command] [flags] [operand]

Commands:
    help [command]
        show help for the command
    new [name-of-the-project]
        create new project
        --template '-t' template to use for project creation (required)
        --platform '-p' generate for this target platform (required)
    templates
        list available templates
    platforms [template]
        list available platforms for given template

"))

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
    (display-help)
    (exit))
   ;; Command: help
   ((string=? (cadr (command-line)) "help")
    (error "HELP: TODO"))
   ;; Command: new
   ((string=? (cadr (command-line)) "new")
    (when (null? (cddr (command-line)))
          (println "Missing argument: project name")
          (exit error:invalid-argument))
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
   (else
    (println "sfusion: unrecognized command. Try \"sfusion help\" for more information. "))))

(main)

