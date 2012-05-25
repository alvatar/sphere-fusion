
LOCAL_PATH := $(call my-dir)

# If compiling cairo & pixman:
#include jni/pixman.mk
#include jni/cairo.mk
include $(CLEAR_VARS)

LOCAL_MODULE := gambit
LOCAL_SRC_FILES :=  build/driver.c \
 build/fib.c \
 build/playground-prototype_.c
LOCAL_CFLAGS += -O2 -fno-short-enums -Wno-missing-field-initializers -I./gambit
LOCAL_LDLIBS := -ldl -fno-short-enums -lc -llog -lGLESv1_CM -L./gambit -lgambc

include $(BUILD_SHARED_LIBRARY)
