#include <jni.h>

#ifndef _Included_org_schemespheres_fusion_MainActivity
#define _Included_org_schemespheres_fusion_MainActivity
#ifdef __cplusplus
extern "C" {
#endif

JNIEXPORT void JNICALL Java_org_schemespheres_fusion_GambitRunnable_initGambit(JNIEnv *, jobject);

JNIEXPORT void JNICALL Java_org_schemespheres_fusion_GambitRunnable_schemeMain(JNIEnv *, jobject);

void input_from_scheme(const char const *);

#ifdef __cplusplus
}
#endif
#endif
