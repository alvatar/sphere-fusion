#define ___VERSION 407003

#include "gambit.h"

#define SCHEME_LIBRARY_LINKER ____20_linkfile__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;



#include <string.h>
#include <stdio.h>
#include <stdlib.h>



void run_gambit()
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

  printf("%s", "Setting up Gambit...\n");

  ___setup(&setup_params);

  printf("%s", "Gambit setup finished...\n");
}

int SDL_main(int argc, char *argv[])
{
    atexit(___cleanup);
    run_gambit();

    return 0;
}
