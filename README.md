# Scheme-fusion
Scheme-fusion is an open source project that aims to facilitate multiplatform project building in the Scheme language. It is based on SDL2, and makes it easy to add C-based multiplatform libraries.

## Installation

### Dependencies
This software must be installed in order to use _scheme-fusion_. Follow instructions in their project pages:
* __Gambit Scheme compiler__: best to use latest version: http://www.iro.umontreal.ca/~gambit See Android Support for notes on compiling Gambit for Android
* __Sake__: use this repo: http://github.com/alvatarc/scheme-sake
* __Scheme Base__: http://github.com/alvatarc/scheme-base

### Android support

First, you need Android SDK and NDK

Then, in order to compile Gambit for Android, follow this instructions. Note that you can find it already compiled in jni/ext but in case you want to customize or compile against latest version, this works for me:

First of all, you need to define ANDROID_NDK_DIR to your NDK path
Then, adapt the following:


    export PATH=$PATH:$ANDROID_NDK_DIR/toolchains/arm-linux-androideabi-4.4.3/prebuilt/linux-x86/bin
    export LD=arm-linux-androideabi-ld
    export AR=arm-linux-androideabi-ar
    export STRIP=arm-linux-androideabi-strip
    export RANLIB=arm-linux-androideabi-ranlib
    export CC=arm-linux-androideabi-gcc
    export CXX=arm-linux-androideabi-g++
    export PREFIX=$ANDROID_NDK_DIR/../gambc
    ./configure --enable-single-host --prefix=$PREFIX --host=arm-eabi CC=arm-linux-androideabi-gcc CPPFLAGS="-DANDROID -I$ANDROID_NDK_DIR/platforms/android-14/arch-arm/usr/include/ -fno-short-enums" CFLAGS="-DANDROID -fno-short-enums -I$ANDROID_NDK_DIR/platforms/android-14/arch-arm/usr/include/ -nostdlib" LDFLAGS="-Wl,-rpath-link=$ANDROID_NDK_DIR/platforms/android-14/arch-arm/usr/lib/ -L$ANDROID_NDK_DIR/platforms/android-14/arch-arm/usr/lib" LIBS="-lc -ldl"
    make

It will break when trying to compile gsi, but you don't need it. You only need libgambc.a

### Android troubleshooting

For every specific version of Gambit:
* Recompile Gambit for the host
* Recompile Gambit for the Android architecture, and copy the libgambc.a generated file to jni/ext

If you get a segmentation fault with Gambit when loading a module that is precompiled, probably it was compiled with a different Gambit version

## Important sake tasks

* __init__: inits and prepares Fusion environment
* __test__: compile all tests
* __clean__: cleans Fusion files and environment

