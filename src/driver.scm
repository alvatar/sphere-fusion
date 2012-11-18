;;; Main driver code

(c-declare #<<end-of-c-declare

#include <android/log.h>

#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_INFO, "scheme-fusion", __VA_ARGS__))

#define LINKER ____20_linkfile__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;


int SDL_main(int argc, char* argv[]) {

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

    LOGI("Initializing Scheme Environment");
    LOGI("Scheme environment output: %i\n", scheme_main());
    LOGI("Finalizing Scheme Environment");
}
end-of-c-declare
)

;;; Entry point for Scheme code

(c-define (entry-point) () int "scheme_main" "" (begin (main) 0))
