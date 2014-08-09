// Gambit version
#define ___VERSION 407003

#include "gambit.h"

// Link file main symbol
#define SCHEME_LIBRARY_LINKER ____20_linkfile__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;



#include <string.h>
#include <stdio.h>
#include <android/log.h>

#define LOGD(LOG_TAG, ...) __android_log_print(ANDROID_LOG_DEBUG, LOG_TAG, __VA_ARGS__)
#define LOGE(LOG_TAG, ...) __android_log_print(ANDROID_LOG_ERROR, LOG_TAG, __VA_ARGS__)



void ___run_gambit()
{
    printf("%s", "Setting up Gambit...\n");
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
	setup_params.linker = SCHEME_LIBRARY_LINKER;
    setup_params.debug_settings = debug_settings;
	
    LOGD("SchemeSpheres", "%s", "Running Gambit...\n");
    
    ___setup(&setup_params);

    LOGD("SchemeSpheres", "%s", "Cleaning up Gambit...\n");
    ___cleanup();
}

int SDL_main(int argc, char *argv[])
{
    ___run_gambit();
    
    return 0;
}
