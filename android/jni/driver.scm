(c-declare #<<end-c-declare

#include "org_playground_gambit.h"
#include <string.h>
#include <jni.h>
#include <stdio.h>

#define LINKER ____20_playground_2d_prototype__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;

void Java_org_playground_gambit_MainActivity_initGambit(JNIEnv *env, jobject obj)
{
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
}

int fib(int x);
char* testports();

jstring Java_org_playground_gambit_MainActivity_testFib(JNIEnv *env, jobject obj)
{
	char buffer[100];
	int n = sprintf(buffer, "fib of 10 is: %d", fib(10));
	
    return (*env)->NewStringUTF(env, buffer);
}

jstring Java_org_playground_gambit_MainActivity_testPorts(JNIEnv *env, jobject obj)
{
    return (*env)->NewStringUTF(env, testports());
}

end-c-declare
)
