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
static jmethodID j_send_string_message_to_activity;
static jmethodID j_send_binary_message_to_activity;

EXTERN void Java_org_playground_gambit_PlayThread_jniInit(JNIEnv* env, jclass cls, jobject obj)
{
    __android_log_print(ANDROID_LOG_INFO, LOG_TAG, "jni_init_bindings()");
    java_env = env;
    gambit_class = (jclass)(*env)->NewGlobalRef(env,cls);
    j_send_string_message_to_activity  = (*java_env)->GetStaticMethodID(java_env, gambit_class, "sendStringMessageToActivity", "()V");
    if(!j_send_string_message_to_activity) {
        __android_log_print(ANDROID_LOG_WARN, LOG_TAG, "Couldn't locate Java callbacks");
    }
}

EXTERN void send_string_message_to_activity()
{
    (*java_env)->CallStaticVoidMethod(java_env, gambit_class, j_send_string_message_to_activity); 
}











#include <EGL/egl.h>
#include <GLES/gl.h>

/**
 * Our saved state data.
 */
 /*
struct saved_state {
    float angle;
    int32_t x;
    int32_t y;
};
*/

/**
 * Shared state for our app.
 */
 /*
struct engine {
    int animating;
    EGLDisplay display;
    EGLSurface surface;
    EGLContext context;
    int32_t width;
    int32_t height;
};
*/

NativeWindowType display_window;
//struct engine engine;

/**
 * Initialize an EGL context for the current display.
 */
static int egl_init() {
    // initialize OpenGL ES and EGL

    /*
     * Here specify the attributes of the desired configuration.
     * Below, we select an EGLConfig with at least 8 bits per color
     * component compatible with on-screen windows
     */
    const EGLint attribs[] = {
            EGL_SURFACE_TYPE, EGL_WINDOW_BIT,
            EGL_BLUE_SIZE, 8,
            EGL_GREEN_SIZE, 8,
            EGL_RED_SIZE, 8,
            EGL_NONE
    };
    EGLint w, h, dummy, format;
    EGLint egl_major_version, egl_minor_version;
    EGLint numConfigs;
    EGLConfig egl_config;
    EGLSurface egl_surface;
    EGLContext egl_context;

    EGLDisplay egl_display = eglGetDisplay(EGL_DEFAULT_DISPLAY);

    display_window = android_createDisplaySurface();
    //display_window = eglCreateWindowSurface();

    eglInitialize(egl_display, &egl_major_version, &egl_minor_version);
    printf("GL Version: %d.%d\n", egl_major_version, egl_minor_version);

    /* Here, the application chooses the configuration it desires. In this
     * sample, we have a very simplified selection process, where we pick
     * the first EGLConfig that matches our criteria */
    if (!eglChooseConfig(egl_display, attribs, &egl_config, 1, &numConfigs))
    {
      printf("eglChooseConfig failed\n");
      if (egl_context == 0) printf("Error code: %x\n", eglGetError());
    }

    /* EGL_NATIVE_VISUAL_ID is an attribute of the EGLConfig that is
     * guaranteed to be accepted by ANativeWindow_setBuffersGeometry().
     * As soon as we picked a EGLConfig, we can safely reconfigure the
     * ANativeWindow buffers to match, using EGL_NATIVE_VISUAL_ID. */
    eglGetConfigAttrib(egl_display, egl_config, EGL_NATIVE_VISUAL_ID, &format);

    ANativeWindow_setBuffersGeometry(display_window, 0, 0, format);

    /*
    surface = eglCreateWindowSurface(display, config, engine->app->window, NULL);
    context = eglCreateContext(display, config, NULL, NULL);
    */

    egl_context = eglCreateContext(egl_display, egl_config, EGL_NO_CONTEXT, NULL);
    if (egl_context == 0) LOGE("Error code: %x\n", eglGetError());

    egl_surface = eglCreateWindowSurface(egl_display, egl_config, display_window, NULL);
    if (egl_surface == 0) LOGE("Error code: %x\n", eglGetError());

    if (eglMakeCurrent(egl_display, egl_surface, egl_surface, egl_context) == EGL_FALSE) {
        LOGE("Unable to eglMakeCurrent");
        return -1;
    }

    /*
    eglQuerySurface(display, surface, EGL_WIDTH, &w);
    eglQuerySurface(display, surface, EGL_HEIGHT, &h);

    engine->display = display;
    engine->context = context;
    engine->surface = surface;
    engine->width = w;
    engine->height = h;
    engine->state.angle = 0;
    */

    // Initialize GL state.
    glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_FASTEST);
    glEnable(GL_CULL_FACE);
    glShadeModel(GL_SMOOTH);
    glDisable(GL_DEPTH_TEST);

    return 0;
}

/**
 * Just the current frame in the display.
 */
 /*
static void engine_draw_frame(struct engine* engine) {
    if (engine->display == NULL) {
        // No display.
        return;
    }

    // Just fill the screen with a color.
    glClearColor(((float)engine->state.x)/engine->width, engine->state.angle,
            ((float)engine->state.y)/engine->height, 1);
    glClear(GL_COLOR_BUFFER_BIT);

    eglSwapBuffers(engine->display, engine->surface);
}
*/

/**
 * Tear down the EGL context currently associated with the display.
 */
 /*
static void engine_term_display(struct engine* engine) {
    if (engine->display != EGL_NO_DISPLAY) {
        eglMakeCurrent(engine->display, EGL_NO_SURFACE, EGL_NO_SURFACE, EGL_NO_CONTEXT);
        if (engine->context != EGL_NO_CONTEXT) {
            eglDestroyContext(engine->display, engine->context);
        }
        if (engine->surface != EGL_NO_SURFACE) {
            eglDestroySurface(engine->display, engine->surface);
        }
        eglTerminate(engine->display);
    }
    engine->animating = 0;
    engine->display = EGL_NO_DISPLAY;
    engine->context = EGL_NO_CONTEXT;
    engine->surface = EGL_NO_SURFACE;
}
*/



/*******************************************************************************
 Gambit
*******************************************************************************/

#define LINKER ____20_playground_2d_prototype__

___BEGIN_C_LINKAGE
extern ___mod_or_lnk LINKER (___global_state_struct*);
___END_C_LINKAGE

___setup_params_struct setup_params;

void Java_org_playground_gambit_PlayThread_initGambit(JNIEnv *env, jobject obj)
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





  send_string_message_to_activity();

  egl_init();







  send_string_message_to_activity();

    // FIXME: NEED TO DO CLEANUP AFTER!
    ___cleanup();
}

/*
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
*/


end-c-declare
)
