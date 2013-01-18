;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; SFusion: generate Scheme Spheres multiplatform programs


(define (display-help)
  (display
   "
Usage: sfusion [command] [flags] [operand]

Commands:
    help [command]
        show help for the command
    new [name-of-the-project]
        create new project
        --template '-t' template to use for project creation
    templates
        list available templates

"))

(define sakefile
  '(define sakefile-test 0))


(define templates-directory "~~spheres/fusion/templates/")

(define (replicate-project-structure project-name project-template)
  (pp project-name)
  (pp project-template)
  (let ((template-dir (string-append templates-directory project-template "/")))
    (if (file-exists? template-dir)
        (let recur ((dir template-dir))
          (for-each (lambda (f)
                      (let ((path (string-append dir f)))
                        (if (eq? 'directory (file-type path))
                            (recur (string-append path "/"))
                            (println (string-append "DO TEMPLATE FILE: " path)))))
                    (directory-files dir)))))
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
    ((string=? (cadr (command-line)) "help")
     (error "HELP: TODO"))
    ((string=? (cadr (command-line)) "new")
     (receive (project-name template)
              (args-fold (cddr (command-line))
                         ;; Option processors
                         (list (option '(#\t "template") #t #f
                                       (lambda (option name arg template)
                                         (values arg))))
                         ;; Unrecognized option processor
                         (lambda (option name arg . seeds)
                           (println (string-append "Unrecognized option: " name))
                           (exit error:invalid-argument))
                         ;; Operand processor: its output gets passed to receive
                         (lambda (operand template)
                           (values operand template))
                         ;; Default argument values
                         '())                                
              (replicate-project-structure project-name template)))
    ((string=? (cadr (command-line)) "templates")
     (println "Available templates:")
     (for-each (lambda (d) (display "  - ") (println d))
               (directory-files templates-directory)))
    (else
     (println "Sfusion: unrecognized command"))))

(main)

