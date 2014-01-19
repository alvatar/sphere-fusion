;;; Copyright (c) 2012-2013 by Álvaro Castro Castilla
;;; SFusion: generate Scheme Spheres projects from generators

(cond-expand
 (optimize
  (declare (standard-bindings) (extended-bindings) (not safe) (block)))
 (debug
  (declare (safe) (debug) (debug-location) (debug-source) (debug-environments)))
 (else (void)))






(define (log type . message)
  (display "*** ")
  (display type)
  (display " -- ")
  (for-each print message)
  (newline))

(define (info . message)
  (apply log (cons "INFO" message)))

(define (info/color color . message)
  (let ((color-string
         (case color
           ((black) "\033[00;30m")
           ((dark-gray) "\033[01;30m")
           ((blue) "\033[00;34m")
           ((light-blue) "\033[01;34m")
           ((green) "\033[00;32m")
           ((light-green) "\033[01;32m")
           ((cyan) "\033[00;36m")
           ((light-cyan) "\033[01;36m")
           ((red) "\033[00;31m")
           ((light-red) "\033[01;31m")
           ((purple) "\033[00;35m")
           ((light-purple) "\033[01;35m")
           ((brown) "\033[00;33m")
           ((yellow) "\033[01;33m")
           ((light-gray) "\033[00;37m")
           ((white) "\033[01;37m")
           (else ""))))
    (apply log (append `("INFO" ,color-string) message))
    (display "\033[00m")))

(define (warn . message)
  (display "\033[00;33m")
  (apply log (cons "WARNING" message))
  (display "\033[00m"))

(define (err . message)
  (display "\033[00;31m")
  (apply log (cons "ERROR" message))
  (display "\033[00m")
  (error "error, aborting"))



;-------------------------------------------------------------------------------
; Filesets and combinators
;-------------------------------------------------------------------------------

(define (extension=? ext)
  (ends-with? ext))

(define (ends-with? end)
  (lambda (name)
    (and (>= (string-length name) (string-length end))
         (string=? (substring name (- (string-length name) (string-length end)) (string-length name))
                   end))))

(##define (newer-than? ext #!key
                       (dir (current-directory)))
  (lambda (name)
    (let ((name0 (string-append 
                  dir
                  (path-strip-extension (path-strip-directory name))
                  ext)))
      (or (not (file-exists? name0))
          (>= (time->seconds (file-last-modification-time name))
              (time->seconds (file-last-modification-time name0)))))))

(define (f-and . ts)
  (lambda (name)
    (let f-and ((ts ts))
      (or (null? ts)
          (and ((car ts) name)
               (f-and (cdr ts)))))))

(define (f-or . ts)
  (lambda (name)
    (let f-or ((ts ts))
      (and (pair? ts)
           (or ((car ts) name)
               (f-or (cdr ts)))))))

(define (shift fn)
  (lambda (t)
    (lambda (name)
      (fn (t name)))))
    
(define f-not (shift not))

(define (any? name) #t)
(define (none? name) #f)

(##define (fileset #!key 
                   (dir (current-directory))
                   (test any?)
                   (recursive #f))
  (let ((dir (path-add-trailing-directory-separator dir)))
    (reduce append '() 
            (map (lambda (name) 
                   (let* ((f (string-append dir name))
                          (childs (if (and recursive (directory? f))
                                      (fileset dir: (path-add-trailing-directory-separator f)
                                               test: test
                                               recursive: recursive)
                                      '())))
                     (if (test f)
                         (cons f childs)
                         childs)))
                 (directory-files `(path: ,dir ignore-hidden: dot-and-dot-dot))))))

;-------------------------------------------------------------------------------
; File handling
;-------------------------------------------------------------------------------

(define (path-add-trailing-directory-separator dir)
  (string-append (path-strip-trailing-directory-separator dir) "/"))

(define (directory? name)
  (eq? (file-type name) 'directory))

(define (regular? name)
  (eq? (file-type name) 'regular))

;;; Make directory

(define (make-directory dir)
  (let ((dir0 (path-strip-trailing-directory-separator dir)))
    (if (file-exists? dir0) #t
        (begin
          (make-directory (path-directory dir0))
          (create-directory dir0)))))

;;; Improved delete-file

(##define (delete-file file #!key (recursive #f) (force #t))
  (let ((file (path-expand file)))
    (info "deleting " file)
    (cond
     ((not (file-exists? file)) 'ok)
     ((directory? file)
      (delete-directory file recursive: recursive force: force))
     (else
      (##delete-file file)))))

;;; Improved delete-directory

(##define (delete-directory dir #!key (recursive #f) (force #t))
  (if force (for-each ##delete-file (fileset dir: dir recursive: #f test: regular?)))
  (if recursive (for-each (lambda (dir) (delete-file
                                    (path-add-trailing-directory-separator dir)
                                    recursive: recursive
                                    force: force))
                          (fileset dir: dir recursive: #f test: directory?)))
  (if (null? (fileset dir: dir recursive: #t test: any?)) 
      (##delete-directory dir)
      (warn dir " is not empty")))

;;; Delete a list of files

(define (delete-files files)
  (for-each (lambda (f) (delete-file f recursive: #t)) files))

;;; Improved copy-file

(##define (copy-file file dest #!key (force #f))
  (let ((file (path-expand file)))
    (cond
     ((directory? file)
      (info "copying " file " to " dest)
      (copy-directory file dest force: force))
     ((and force (file-exists? dest))
      (delete-file dest recursive: #t)
      (copy-file file dest force: #f))
     ((not (file-exists? dest))
      (info "copying " file " to " dest)
      (##copy-file file dest))
     (else
      (warn dest " already exists")))))

;;; Copy a directory

(##define (copy-directory file dest #!key (force #f) (merge #t))
  (when (and force merge)
        (warn "You can't both force and merge when copying directories, merge chosen for safety.")
        (set! force #f))
  (cond
   ((and force (file-exists? dest))
    (delete-file dest recursive: #t force: #t)
    (copy-directory file dest force: force))
   ((or merge (not (file-exists? dest)))
    (if (not (file-exists? dest)) (create-directory dest))
    (for-each
     (lambda (filename)
       (copy-file filename
                  (string-append
                   (path-strip-trailing-directory-separator dest)
                   "/" (path-strip-directory filename))))
     (fileset dir: file recursive: #f)))
   (else
    (warn dest " already exists"))))

;;; Copy a list of files and directories

(##define (copy-files files dest #!key (force #f))
  (for-each
   (lambda (file) 
     (copy-file file
                (string-append (path-strip-trailing-directory-separator dest)
                               "/"
                               (path-strip-directory file))
                force: force))
   files))

(define (read-file file)
  (call-with-input-file (path-expand file)
    (lambda (in) (read-line in #f))))

(define (read-files files)
  (call-with-output-string ""
    (lambda (out)
      (for-each (lambda (file) (display (read-file file) out))
                files))))

(define (append-files files dest)
  (call-with-output-file dest
    (lambda (out)
      (for-each (lambda (file) (display (read-file file) out))
                files))))









(define help:general
  "
Usage: sfusion [command] [flags] [operand]

Commands:
    help [command]
        show help for the command
    new [flags] -g [generator] [name-of-the-project]
        create new project
        --generator '-g' generator to use for project creation (required)
        --target '-t' select a target (if unused, all available ones will be selected)
        --source '-s' alternate source directory containing generators
    generators
        list available generators
    targets [generator]
        list available targets for a given generator
    add -g [generator] [target]
        add target using generator

")

(define help:new
  "
Use 'new' command to create a new project from scratch, using a generator

Usage: sfusion new [flags] [name-of-the-project]
    --generator '-g' generator to use for project creation (required)
    --target '-t' select a target (if unused, all available ones will be selected)
    --source '-s' alternate source directory containing generators
    
Example: sfusion new -g opengl2d my-new-project
    Creates the \"my-new-project\" using generator \"opengl\"
    
")

(define help:generators
  "
Use 'generators' command to list available generators

Usage: sfusion generators

")

(define help:targets
  "
Use 'targets' command to list the available targets offered by a generator
Obtain the list of available generators with the 'generators' command

Usage: sfusion targets [generator]

")

;;------------------------------------------------------------------------------
;;!! Utils


;;! Where generators are expected to be installed
(define generators-path
  (make-parameter "~~spheres/fusion/generators/"))

;;! Get a list of current subdirectories
(define (directory-subdirs path)
  (filter (lambda (f) (eq? 'directory (file-type (string-append (path-strip-trailing-directory-separator path) "/" f))))
          (directory-files path)))

;;! Get the script path from the target path
(define (target-path->script-path target-path)
  (let ((script-file (substring target-path 0 (-- (string-length target-path)))))
    (string-append script-file ".scm")))

;;! Get a list of a generator's targets
(define (generator-targets generator-path)
  (remove (curry string=? "base") (directory-subdirs generator-path)))

;;------------------------------------------------------------------------------
;;!! Generator tasks

;; A target is one of the platforms that the generator can create code for
;; A script is a file containing instructions to instantiate a target's code

(define instantiate-target
  (lambda (destination-path source-path)
    ((Y (lambda (recur) ; Anonymous recursion: call recur with the new relative-path argument
          (lambda (relative-path)
            (for-each
             (lambda (file)
               (case (file-info-type (file-info (string-append source-path relative-path file)))
                 ((regular)
                  (let ((original-file (string-append source-path relative-path file))
                        (new-file (string-append destination-path relative-path file)))
                    (when (file-exists? new-file)
                          (println
                           "Error creating project: generators for chosen targets collide. Please contact generator author.")
                          (exit error:operation-not-permitted))
                    ;; Test whether it's a generator file to process
                    (if (string=? ".sct" (path-extension original-file))
                        (call-with-output-file
                            (path-strip-extension new-file)
                          (lambda (f) (display
                                  ((build-template-from-file
                                    (string-append source-path relative-path file)
                                    'generators)
                                   'no-arguments)
                                  f)))
                        (copy-file original-file new-file))))
                 ((directory)
                  (create-directory (string-append destination-path relative-path file))
                  (recur (string-append relative-path file "/")))))
             (directory-files (string-append source-path relative-path)))))) "")))


;;------------------------------------------------------------------------------
;;!! Sfusion commands and main

;;! Recursively replicate the generator with its different target implementations
;; A "base" target is processed first for common code
(define (create-project name generator targets source)
  (parameterize
   ((generators-path (or source (generators-path))))
   (let* ((generator-path (string-append (generators-path) generator "/"))
          ;; If no target is given, add all by default
          (targets (if (null? targets) (directory-subdirs generator-path) (cons "base" targets)))
          (target-paths (map (lambda (p) (string-append generator-path p "/")) targets))
          (project-path (string-append (current-directory) name "/")))
     (unless (file-exists? generator-path)
             (println "Generator not available: please see available generators (run 'sfusion generators').")
             (exit error:no-such-file-or-directory))
     (unless (file-exists? (car target-paths))
             (println "Generator has no \"base\" directory (generator is incomplete): please contact generator author.")
             (exit error:no-such-file-or-directory))
     (unless (every file-exists? target-paths)
             (println "Target not available: please see available targets (run 'sfusion targets [generator]').")
             (exit error:no-such-file-or-directory))
     (when (file-exists? name)
           (println "Aborting: project folder already exists")
           (exit error:file-exists))
     (create-directory project-path)
     ;; Generators consist of templates organized by directories, which depend  on the target.
     ;; Templates generate the custom code for each project. A scheme script named like the
     ;; target takes care of running the necessary actions for instantiating the code.
     (for-each
      (lambda (target target-path)
        (let ((script-file (target-path->script-path target-path)))
          ;; Check if there is a script file and run the process through it.
          (if (file-exists? script-file)
              ;; The script file gets evaluated here.
              (eval `(,@(with-input-from-file script-file read-all)
                      ;; We inject the target information as a list, so it can be used by the script file
                      `((project-path: ,,project-path)
                        (target: ,,target)
                        (target-path: ,,target-path))))
              ;; Otherwise, proceed with regular target instantiation
              (instantiate-target project-path target-path))))
      targets
      target-paths)
     (println (string-append "Project " name " succesfully created")))))

(define (main arguments)
  (cond
   ((null? (cdr arguments))
    (display help:general))
   ;; Command: help
   ((string=? (cadr arguments) "help")
    (if (null? (cddr arguments))
        (display help:general)
        (let ((help-al `((new . ,help:new) (generators . ,help:generators) (targets . ,help:targets))))
          (display (or (aif it (assq (string->symbol (caddr arguments)) help-al) (cdr it))
                       "No help for this command\n")))))
   ;; Command: new
   ((string=? (cadr arguments) "new")
    (args-fold-receive (project-name generator targets source)
                       (args-fold (cddr arguments)
                                  ;; Option processors
                                  (list (option '(#\g "generator") #t #f
                                                (lambda (option name arg generator targets source)
                                                  (values arg targets source)))
                                        (option '(#\t "target") #t #f
                                                (lambda (option name arg generator targets source)
                                                  (values generator (cons arg targets) source)))
                                        (option '(#\s "source") #t #f
                                                (lambda (option name arg generator targets source)
                                                  (values generator targets (path-normalize arg)))))
                                  ;; Unrecognized option processor
                                  (lambda (option name arg . seeds)
                                    (println (string-append "Unrecognized option: -" (string name)))
                                    (exit error:invalid-argument))
                                  ;; Operand processor: its output gets passed to receive
                                  (lambda (operand generator targets source)
                                    (values operand generator targets source))
                                  ;; Default argument values
                                  #f
                                  '()
                                  #f)
                       (lambda (project-name generator targets source)
                         (unless generator
                                 (println "Missing argument: generator")
                                 (exit error:invalid-argument)))
                       (lambda args
                         (println "Missing or malformed arguments. Try \"sfusion help\" for more information.")
                         (exit error:invalid-argument))
                       (create-project project-name generator targets source)))
   ;; Command: generators
   ((string=? (cadr arguments) "generators")
    (println "Available generators:")
    (for-each (lambda (d) (display "  - ") (println d))
              (directory-subdirs (generators-path))))
   ;; Command: targets
   ((string=? (cadr arguments) "targets")
    (when (null? (cddr arguments))
          (println "Missing argument: generator")
          (exit error:invalid-argument))
    (let* ((generator (caddr arguments))
           (generator-path (string-append (generators-path) generator "/")))
      (unless (file-exists? generator-path)
              (println "Target doesn't exist. Try \"sfusion generators\" command for a list of available generators.")
              (exit error:invalid-argument))
      (println "Available targets for generator " generator ":")
      (for-each (lambda (d) (display "  - ") (println d))
                (generator-targets generator-path))))
   ;; Command: add
   ((string=? (cadr arguments) "add")
    'add)
   ;; Unrecognized command
   (else
    (println "sfusion: unrecognized command. Try \"sfusion help\" for more information. ")
    (exit error:invalid-argument)))
  (exit error:success))

;; Run!
(main (command-line))
