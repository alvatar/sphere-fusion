;;; Copyright (c) 2012 by Álvaro Castro Castilla
;;; SFusion: generate Scheme Spheres multiplatform programs

(define error:operation-not-permitted 1)
(define error:no-such-file-or-directory 2)
(define error:no-such-process 3)
(define error:interrupted-system-call 4)
(define error:i/o-error 5)
(define error:no-such-device-or-address 6)
(define error:arg-list-too-long 7)
(define error:exec-format-error 8)
(define error:bad-file-number 9)
(define error:no-child-processes 10)
(define error:try-again 11)
(define error:out-of-memory 12)
(define error:permission-denied 13)
(define error:bad-address 14)
(define error:block-device-required 15)
(define error:device-or-resource-busy 16)
(define error:file-exists 17)
(define error:cross-device-link 18)
(define error:no-such-device 19)
(define error:not-a-directory 20)
(define error:is-a-directory 21)
(define error:invalid-argument 22)
(define error:file-table-overflow 23)
(define error:too-many-open-files 24)
(define error:not-a-typewriter 25)
(define error:text-file-busy 26)
(define error:file-too-large 27)
(define error:no-space-left-on-device 28)
(define error:illegal-seek 29)
(define error:read-only-file-system 30)
(define error:too-many-links 31)
(define error:broken-pipe 32)
(define error:math-argument-out-of-domain-of-func 33)
(define error:math-result-not-representable 34)
(define error:resource-deadlock-would-occur 35)
(define error:file-name-too-long 36)
(define error:no-record-locks-available 37)
(define error:function-not-implemented 38)
(define error:directory-not-empty 39)
(define error:too-many-symbolic-links-encountered 40)
(define error:operation-would-block 41)
(define error:no-message-of-desired-type 42)
(define error:identifier-removed 43)
(define error:channel-number-out-of-range 44)
(define error:level-2-not-synchronized 45)
(define error:level-3-halted 46)
(define error:level-3-reset 47)
(define error:link-number-out-of-range 48)
(define error:protocol-driver-not-attached 49)
(define error:no-CSI-structure-available 50)
(define error:level-2-halted 51)
(define error:invalid-exchange 52)
(define error:invalid-request-descriptor 53)
(define error:exchange-full 54)
(define error:no-anode 55)
(define error:invalid-request-code 56)
(define error:invalid-slot 57)

(define error:bad-font-file-format 59)
(define error:device-not-a-stream 60)
(define error:no-data-available 61)
(define error:timer-expired 62)
(define error:out-of-streams-resources 63)
(define error:machine-is-not-on-the-network 64)
(define error:package-not-installed 65)
(define error:object-is-remote 66)
(define error:link-has-been-severed 67)
(define error:advertise-error 68)
(define error:srmount-error 69)
(define error:communication-error-on-send 70)
(define error:protocol-error 71)
(define error:multihop-attempted 72)
(define error:rfs-specific-error 73)
(define error:not-a-data-message 74)
(define error:value-too-large-for-defined-data-type 75)
(define error:name-not-unique-on-network 76)
(define error:file-descriptor-in-bad-state 77)
(define error:remote-address-changed 78)
(define error:can-not-access-a-needed-shared-library 79)
(define error:accessing-a-corrupted-shared-library 80)
(define error:.lib-section-in-a.out-corrupted 81)
(define error:attempting-to-link-in-too-many-shared-libraries 82)
(define error:cannot-exec-a-shared-library-directly 83)
(define error:illegal-byte-sequence 84)
(define error:interrupted-system-call-should-be-restarted 85)
(define error:streams-pipe-error 86)
(define error:too-many-users 87)
(define error:socket-operation-on-non-socket 88)
(define error:destination-address-required 89)
(define error:message-too-long 90)
(define error:protocol-wrong-type-for-socket 91)
(define error:protocol-not-available 92)
(define error:protocol-not-supported 93)
(define error:socket-type-not-supported 94)
(define error:operation-not-supported-on-transport-endpoint 95)
(define error:protocol-family-not-supported 96)
(define error:address-family-not-supported-by-protocol 97)
(define error:address-already-in-use 98)
(define error:cannot-assign-requested-address 99)
(define error:network-is-down 100)
(define error:network-is-unreachable 101)
(define error:network-dropped-connection-because-of-reset 102)
(define error:software-caused-connection-abort 103)
(define error:connection-reset-by-peer 104)
(define error:no-buffer-space-available 105)
(define error:transport-endpoint-is-already-connected 106)
(define error:transport-endpoint-is-not-connected 107)
(define error:cannot-send-after-transport-endpoint-shutdown 108)
(define error:too-many-references:-cannot-splice 109)
(define error:connection-timed-out 110)
(define error:connection-refused 111)
(define error:host-is-down 112)
(define error:no-route-to-host 113)
(define error:operation-already-in-progress 114)
(define error:operation-now-in-progress 115)
(define error:stale-nfs-file-handle 116)
(define error:structure-needs-cleaning 117)
(define error:not-a-xenix-named-type-file 118)
(define error:no-xenix-semaphores-available 119)
(define error:is-a-named-type-file 120)
(define error:remote-I/O-error 121)
(define error:quota-exceeded 122)
(define error:no-medium-found 123)
(define error:wrong-medium-type 124)
(define error:operation-canceled 125)
(define error:required-key-not-available 126)
(define error:key-has-expired 127)
(define error:key-has-been-revoked 128)
(define error:key-was-rejected-by-service 129)
(define error:owner-died 130)
(define error:state-not-recoverable 131)


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

;; An idea of how templates could be:
;; <title>=(string-append "Hello" " world")</title>
;; <h1>Header</h1>
;; <h2>Header 2 will receive an injected variable from the caller: =(let-templates () (external-var) (object->string external-var))</h2>
;; =(let-templates (a b) () ; this template won't use the external-var so it doesn't need to specify it
;;    (form-for shoe html: '((multipart #t))
;;      (lambda (f)
;;        (if (not (null? shoes))
;;            (a f shoes) ; template a is passed the variables "f" and "shoes", template b doesn't get any variable
;;            (b)))))
;; -( <div>Text 1</div>
;;    =(let-templates (aa) (f shoes) ; we need to receive the parameters passed by the parent template. If empty list then the params are not passed
;;       (string-append "<div>" (aa) "</div>"))
;;    -( just some plain text inside text1, which will be surrounded by divs )
;;    <div>Text 1 (continues)</div> )
;; -( <div>Text 2</div> )


(define (process-template template-file output-file)
  (define (evaluate-template-code char-list)
    (let recur ((cs char-list))
      (cond ((null? cs)
             '())
            ;; open code portion
            ((and (char=? #\# (car cs))
                  (not (null? (cddr cs)))
                  (char=? #\| (cadr cs)))
             (receive (code-char-list rest)
                      (let code-loop ((code-cs (cddr cs)))
                        (cond ((null? code-cs)
                               (error "malformed template"))
                              ;; close code portion
                              ((and (char=? #\| (car code-cs))
                                    (not (null? (cddr code-cs)))
                                    (char=? #\# (cadr code-cs)))
                               (values '() (cddr code-cs)))
                              (else (receive (code-char-list rest)
                                             (code-loop (cdr code-cs))
                                             (values (cons (car code-cs)
                                                           code-char-list)
                                                     rest)))))
                      (recur (append
                              (string->list
                               (eval (cons 'begin (with-input-from-string (list->string code-char-list) read-all))))
                              rest))))
            (else (cons (car cs)
                        (recur (cdr cs)))))))
  (let ((result (list->string
                 (evaluate-template-code
                  (call-with-input-file
                      template-file
                    (lambda (i) (read-all i read-char)))))))
    (if output-file
        (call-with-output-file
            output-file
          (lambda (output)
            (display
             result
             output)))
        (display result))))

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

