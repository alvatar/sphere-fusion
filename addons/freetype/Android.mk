LOCAL_PATH := $(call my-dir)

LOCAL_ARM_MODE := arm

SOURCES := truetype/truetype.c \
	         autofit/autofit.c   \
	         base/ftbase.c       \
	         base/ftinit.c       \
	         type1/type1.c       \
	         cid/type1cid.c      \
	         cff/cff.c           \
	         pfr/pfr.c           \
	         bdf/bdf.c           \
	         smooth/smooth.c     \
	         sfnt/sfnt.c         \
	         raster/raster.c     \
	         type42/type42.c     \
	         winfonts/winfnt.c   \
	         psnames/psmodule.c  \
	         pcf/pcf.c           \
	         psaux/psaux.c       \
	         pshinter/pshinter.c \
	         gzip/ftgzip.c \
	         lzw/ftlzw.c \
	         base/ftsystem.c

SOURCES := $(addprefix src/,$(SOURCES))

include $(CLEAR_VARS)

LOCAL_MODULE := libfreetype
LOCAL_SRC_FILES := $(SOURCES)
LOCAL_C_INCLUDES := freetype/include
#LOCAL_LDLIBS := -landroid #?
LOCAL_CFLAGS := -O2 -fPIC -DPIC -DFT2_BUILD_LIBRARY

include $(BUILD_STATIC_LIBRARY)

