;;; logging

(c-define (test-proc x y) (int int) int "scheme_proc" ""
          (+ x y))

(define android:log-info
  (c-lambda (char-string char-string)
            void
   "__android_log_print(ANDROID_LOG_INFO,___arg1,___arg2);"))

(define android:log
  (c-lambda (int char-string char-string)
            void
            "__android_log_print"))

;;; On events code generation

(define-macro
  (generate-callback suffix params c-name)
  (let* ((global (string->symbol (string-append "*android:" (symbol->string suffix) "*")))
         (exec-name (string->symbol (string-append "android:" (symbol->string suffix))))
         (setter-name (string->symbol (string-append "android:" (symbol->string suffix) "-set!"))))
    `(begin
       (define (,global) (lambda () #f))
       (c-define (,exec-name) ,params void ,c-name ""
                 (,global))
       (define (,setter-name f)
         (set! ,global f)))))

;(generate-callback on-create () "gambit_on_create")
;(generate-callback on-pause () "gambit_on_pause")
;(generate-callback on-resume () "gambit_on_resume")
;(generate-callback on-destroy () "gambit_on_destroy")
;(generate-callback on-surface-created () "gambit_surface_created")
;(generate-callback on-surface-destroy () "gambit_surface_destroy")
;(generate-callback on-surface-changed () "gambit_surface_changed")
;(generate-callback on-touch-down () "gambit_on_touch_down")
;(generate-callback on-touch-up () "gambit_on_touch_up")
;(generate-callback on-touch-move () "gambit_on_touch_move")

;;; Main driver code

(c-declare #<<end-of-c-declare

#include <android/log.h>

#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, "SDL", __VA_ARGS__))

#define LINKER ____20_playground_2d_prototype__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;


void SDL_main() {

    // <-- BEGIN GAMBIT INITIALIZATION
    // Taken from gambit, lib/main.c. 
    int debug_settings = ___DEBUG_SETTINGS_INITIAL;

      // -:d- (force repl io to be stdin/stdout since terminal isn't
      // -attached)
      debug_settings =
          (debug_settings
           & ~___DEBUG_SETTINGS_REPL_MASK)
          | (___DEBUG_SETTINGS_REPL_STDIO
             << ___DEBUG_SETTINGS_REPL_SHIFT);
      // -:da
      debug_settings =
          (debug_settings
           & ~___DEBUG_SETTINGS_UNCAUGHT_MASK)
          | (___DEBUG_SETTINGS_UNCAUGHT_ALL
             << ___DEBUG_SETTINGS_UNCAUGHT_SHIFT);
      // -:dr
      debug_settings =
          (debug_settings
           & ~___DEBUG_SETTINGS_ERROR_MASK)
          | (___DEBUG_SETTINGS_ERROR_REPL
             << ___DEBUG_SETTINGS_ERROR_SHIFT);

      ___setup_params_reset (&setup_params);
    setup_params.version = ___VERSION;
    setup_params.linker = LINKER;
      setup_params.debug_settings = debug_settings;
    
    ___setup(&setup_params);
    // <-- END GAMBIT INITIALIZATION


    LOGI("+++++++++++++++++++++++++++++++++++++++++++");
    int i = 0;
    for(i=0; i<20; i++) {
        LOGI("Fib(): %i\n", fib(3));
    }
}
end-of-c-declare
)
