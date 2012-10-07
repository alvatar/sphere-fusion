LOCAL_PATH := $(call my-dir)

LIBPIXMAN_SRC= \
	       pixman/pixman-access.c                          \
	       pixman/pixman-access-accessors.c                \
	       pixman/pixman-cpu.c                             \
	       pixman/pixman-gradient-walker.c                 \
	       pixman/pixman-region16.c                        \
	       pixman/pixman-region32.c                        \
	       pixman/pixman-image.c                           \
	       pixman/pixman-implementation.c                  \
	       pixman/pixman-general.c                         \
	       pixman/pixman.c                                 \
	       pixman/pixman-fast-path.c                       \
	       pixman/pixman-solid-fill.c                      \
	       pixman/pixman-conical-gradient.c                \
	       pixman/pixman-linear-gradient.c                 \
	       pixman/pixman-radial-gradient.c                 \
	       pixman/pixman-bits-image.c                      \
	       pixman/pixman-utils.c                           \
	       pixman/pixman-edge.c                            \
	       pixman/pixman-edge-accessors.c                  \
	       pixman/pixman-trap.c                            \
	       pixman/pixman-timer.c                           \
	       pixman/pixman-matrix.c                          \
	       pixman/pixman-noop.c                            \
	       pixman/pixman-arm-simd.c                        \
	       pixman/pixman-arm-simd-asm.S                    \
	       pixman/pixman-arm-neon.c                        \
	       pixman/pixman-arm-neon-asm.S                    \
	       pixman/pixman-arm-neon-asm-bilinear.S           \
	       ../pixman-extra/pixman-combine32.c              \
	       ../pixman-extra/pixman-combine64.c              \



LIBPIXMAN_CFLAGS:=-D_USE_MATH_DEFINES -DPIXMAN_NO_TLS -DPACKAGE="android-cairo" -DUSE_ARM_NEON -DUSE_ARM_SIMD -include "limits.h"

include $(CLEAR_VARS)

LOCAL_MODULE    := libpixman
LOCAL_CFLAGS    := -O2 $(LIBPIXMAN_CFLAGS) -Ipixman/pixman -Ipixman-extra -include "pixman-elf-fix.h" -Wno-missing-field-initializers
LOCAL_LDFLAGS   := 
LOCAL_SRC_FILES := $(LIBPIXMAN_SRC)
LOCAL_STATIC_LIBRARIES := cpufeatures

include $(BUILD_STATIC_LIBRARY)

$(call import-module,android/cpufeatures)
