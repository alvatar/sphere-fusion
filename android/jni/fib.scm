(define (fib x)
	(letrec 
		((helper (lambda (n a b)
			(if (= n 0)
				a
				(helper (- n 1) b (+ a b))))))
		(helper x 0 1)))

(define (test-ports)
	(read-line (open-process "ls") "failed"))

(c-define (c-fib x) (int32) int "fib" "" (fib x))

(c-define (c-simple x) () void "simple" "" (void))

(c-define (c-testports) () char-string "testports" "" (test-ports))

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


    LOGI("********************************");
    int i = 0;
    for(i=0; i<20; i++) {
        LOGI("Fib(): %i\n", fib(3));
    }
}
end-of-c-declare
)
