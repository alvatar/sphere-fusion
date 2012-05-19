#include <jni.h>

#ifndef _Included_org_playground_gambit_PlaygroundActivity
#define _Included_org_playground_gambit_PlaygroundActivity
#ifdef __cplusplus
extern "C" {
#endif

JNIEXPORT jstring JNICALL Java_org_playground_gambit_PlaygroundActivity_testFib(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_org_playground_gambit_PlaygroundActivity_testPorts(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_org_playground_gambit_PlaygroundActivity_initGambit(JNIEnv *, jobject);

#ifdef __cplusplus
}
#endif
#endif
