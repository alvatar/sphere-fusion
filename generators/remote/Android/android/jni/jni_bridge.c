/*
 * Copyright (C) 2009 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#define ___VERSION 407001

#include "org_schemespheres_fusion.h"
#include <string.h>
#include <jni.h>
#include <stdio.h>
#include <android/log.h>
#include "gambit.h"

#define LOGD(LOG_TAG, ...) __android_log_print(ANDROID_LOG_DEBUG, LOG_TAG, __VA_ARGS__)
#define LOGE(LOG_TAG, ...) __android_log_print(ANDROID_LOG_ERROR, LOG_TAG, __VA_ARGS__)

#define LINKER ____20_linkfile__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;


void scheme_main();

JavaVM *g_java_vm = NULL;
JNIEnv *g_java_gambit_env;

JNIEXPORT jint JNICALL JNI_OnLoad (JavaVM *vm, void *pvt)
{
  LOGD("SchemeSpheres", "JNI_OnLoad called");
  JNIEnv *env;
  if ((*vm)->GetEnv(vm, (void**)(&env), JNI_VERSION_1_6) != JNI_OK)
    {
      return -1;
    }
  g_java_vm = vm;
  g_java_gambit_env = env;
  return JNI_VERSION_1_6;
}

JNIEXPORT void JNICALL JNI_OnUnload (JavaVM *vm, void *pvt)
{
  LOGD("SchemeSpheres", "JNI_OnUnload called");
}


JNIEXPORT void JNICALL Java_org_schemespheres_fusion_GambitRunnable_initGambit(JNIEnv *env, jobject obj)
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

JNIEXPORT void JNICALL Java_org_schemespheres_fusion_GambitRunnable_schemeMain(JNIEnv *env, jobject obj)
{
  LOGD("SchemeSpheres", "Running (main)...");
  scheme_main();
}

void input_from_scheme (const char const * input_string) {
  LOGD("SchemeSpheres", "input_from_scheme C: %s", input_string);
  if(!g_java_gambit_env || !g_java_vm) {
    LOGD("SchemeSpheres", "Error: Java VM and environment are not ready");
  }
  JNIEnv *env = g_java_gambit_env;
  // Get GambitRunnable class
  jclass gambit_thread = (*g_java_gambit_env)->FindClass(env, "org/schemespheres/fusion/GambitRunnable");
  if ((*env)->ExceptionCheck(env)) {
    LOGD("SchemeSpheres", "Class org/schemespheres/fusion/GambitRunnable not found!");
    return;
  }
  // Get the static printFromScheme method
  jmethodID method = (*env)->GetStaticMethodID(env, gambit_thread, "printFromScheme", "(Ljava/lang/String;)V");
  if ((*env)->ExceptionCheck(env)) {
    LOGD("SchemeSpheres", "Method printFromScheme(String) not found!");
    return;
  }
  // Call the static method
  (*env)->CallStaticVoidMethod(env, gambit_thread, method, (*env)->NewStringUTF(env, input_string));
  if ((*env)->ExceptionCheck(env)) {
     return;
  }
}
