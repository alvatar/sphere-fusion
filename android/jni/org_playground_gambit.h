#include <jni.h>

#ifndef _Included_org_playground_gambit_MainActivity
#define _Included_org_playground_gambit_MainActivity
#ifdef __cplusplus
extern "C" {
#endif

JNIEXPORT jstring JNICALL Java_org_playground_gambit_MainActivity_testFib(JNIEnv *, jobject);

JNIEXPORT jstring JNICALL Java_org_playground_gambit_MainActivity_testPorts(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_org_playground_gambit_MainActivity_initGambit(JNIEnv *, jobject);

#ifdef __cplusplus
}
#endif
#endif
