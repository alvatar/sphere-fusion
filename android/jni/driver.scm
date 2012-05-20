;;; logging

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

(generate-callback on-create () "gambit_on_create")
(generate-callback on-pause () "gambit_on_pause")
(generate-callback on-resume () "gambit_on_resume")
(generate-callback on-destroy () "gambit_on_destroy")
(generate-callback on-surface-created () "gambit_surface_created")
(generate-callback on-surface-destroy () "gambit_surface_destroy")
(generate-callback on-surface-changed () "gambit_surface_changed")
(generate-callback on-touch-down () "gambit_on_touch_down")
(generate-callback on-touch-up () "gambit_on_touch_up")
(generate-callback on-touch-move () "gambit_on_touch_move")

;;; Main driver code

(c-declare #<<end-c-declare

#include "org_playground_gambit.h"
#include <string.h>
#include <stdio.h>

#include <jni.h>
#include <android/log.h>

/*******************************************************************************
 Functions called by JNI
*******************************************************************************/

#define LOG_TAG "Playground"
#define LOGI(...)  __android_log_print(ANDROID_LOG_INFO,LOG_TAG,__VA_ARGS__)
#define LOGE(...)  __android_log_print(ANDROID_LOG_ERROR,LOG_TAG,__VA_ARGS__)

///////////////// TODO: DEFINE IF C++
#define EXTERN 


static JNIEnv* java_env = NULL;
static JavaVM* java_vm;

EXTERN jint JNI_OnLoad(JavaVM* vm, void* reserved) {
    JNIEnv *env;
    java_vm = vm;
    LOGI("JNI_OnLoad called");
    if ((*java_vm)->GetEnv(java_vm, (void**)&env, JNI_VERSION_1_4) != JNI_OK) {
        LOGE("Failed to get the environment using GetEnv()");
        return -1;
    }
    return JNI_VERSION_1_4;
}

// Gambit thread class
static jclass gambit_class;
//  Methods
static jmethodID send_message_to_activity;

EXTERN void Java_org_playground_gambit_GambitThread_jniInit(JNIEnv* env, jclass cls, jobject obj) {
    __android_log_print(ANDROID_LOG_INFO, LOG_TAG, "jni_init_bindings()");
    java_env = env;
    gambit_class = (jclass)(*env)->NewGlobalRef(env,cls);
    send_message_to_activity  = (*java_env)->GetStaticMethodID(java_env, gambit_class, "sendMessageToActivity", "()V");
    if(!send_message_to_activity) {
        __android_log_print(ANDROID_LOG_WARN, LOG_TAG, "Couldn't locate Java callbacks");
    }
}

/*******************************************************************************
 Gambit
*******************************************************************************/

#define LINKER ____20_playground_2d_prototype__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;

void Java_org_playground_gambit_GambitThread_initGambit(JNIEnv *env, jobject obj)
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


    //(*java_env)->CallStaticVoidMethod(java_env, gambit_class, send_message_to_activity); 

    // FIXME: NEED TO DO CLEANUP AFTER!
}

int fib(int x);
char* testports();

jstring Java_org_playground_gambit_PlaygroundActivity_testFib(JNIEnv *env, jobject obj)
{
	char buffer[100];
	int n = sprintf(buffer, "fib of 10 is: %d", fib(10));
	
    return (*env)->NewStringUTF(env, buffer);
}

jstring Java_org_playground_gambit_PlaygroundActivity_testPorts(JNIEnv *env, jobject obj)
{
    return (*env)->NewStringUTF(env, testports());
}

void Java_org_playground_gambit_PlaygroundActivity_nativeCreate(JNIEnv *env, jobject obj)
{
    gambit_on_create();
    __android_log_print(ANDROID_LOG_INFO, "Playground", "nativeCreate()");
}

void Java_org_playground_gambit_PlaygroundActivity_nativePause(JNIEnv *env, jobject obj)
{
    gambit_on_pause();
    __android_log_print(ANDROID_LOG_INFO, "Playground", "nativePause()");
}

void Java_org_playground_gambit_PlaygroundActivity_nativeResume(JNIEnv *env, jobject obj)
{
    gambit_on_resume();
    __android_log_print(ANDROID_LOG_INFO, "Playground", "nativeResume()");
}

void Java_org_playground_gambit_PlaygroundActivity_nativeDestroy(JNIEnv *env, jobject obj)
{
    gambit_on_destroy();
    __android_log_print(ANDROID_LOG_INFO, "Playground", "nativeDestroy()");
}

void Java_org_playground_gambit_PlaygroundActivity_nativeSurfaceCreated(JNIEnv *env, jobject obj)
{
    gambit_surface_created();
    __android_log_print(ANDROID_LOG_INFO, "Playground", "nativeSurfaceCreated()");
}
void Java_org_playground_gambit_PlaygroundActivity_nativeSurfaceDestroy(JNIEnv *env, jobject obj)
{
    gambit_surface_destroy();
    __android_log_print(ANDROID_LOG_INFO, "Playground", "nativeSurfaceDestroy()");
}
void Java_org_playground_gambit_PlaygroundActivity_nativeSurfaceChanged(JNIEnv *env, jobject obj)
{
    gambit_surface_changed();
    __android_log_print(ANDROID_LOG_INFO, "Playground", "nativeSurfaceChanged()");
}
void Java_org_playground_gambit_PlaygroundActivity_nativeOnTouchDown(JNIEnv *env, jobject obj)
{
    gambit_on_touch_down();
    __android_log_print(ANDROID_LOG_INFO, "Playground", "nativeOnTouchDown()");
}

void Java_org_playground_gambit_PlaygroundActivity_nativeOnTouchUp(JNIEnv *env, jobject obj)
{
    gambit_on_touch_up();
    __android_log_print(ANDROID_LOG_INFO, "Playground", "nativeOnTouchUp()");
}

void Java_org_playground_gambit_PlaygroundActivity_nativeOnTouchMove(JNIEnv *env, jobject obj)
{
    gambit_on_touch_move();
    __android_log_print(ANDROID_LOG_INFO, "Playground", "nativeOnTouchMove()");
}


end-c-declare
)
