#!/bin/sh

export PATH=$PATH:$ANDROID_NDK_PATH/toolchains/arm-linux-androideabi-4.8/prebuilt/linux-x86_64/bin/
export LD=arm-linux-androideabi-ld
export AR=arm-linux-androideabi-ar
export STRIP=arm-linux-androideabi-strip
export RANLIB=arm-linux-androideabi-ranlib
export CC=arm-linux-androideabi-gcc
export CXX=arm-linux-androideabi-g++
export PREFIX=$ANDROID_NDK_PATH/../gambc
export ANDROID_PLATFORM=$ANDROID_NDK_PATH/platforms/android-19/arch-arm/usr
./configure --enable-single-host --prefix=$PREFIX -host=arm-linux-androideabi CC=arm-linux-androideabi-gcc CPPFLAGS="-DANDROID -I$ANDROID_PLATFORM/include/ -fno-short-enums" CFLAGS="-DANDROID -fno-short-enums -I$ANDROID_PLATFORM/include/ -nostdlib" LDFLAGS="-Wl,-rpath-link=$ANDROID_PLATFORM/lib/ -L$ANDROID_PLATFORM/lib" LIBS="-lc -ldl"
make
