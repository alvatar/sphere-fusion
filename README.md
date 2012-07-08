# Scheme Playground, a C/Scheme library
Scheme Playground is an open source project in C and Scheme that aims to be useful for people who want to create images, animations, and interactions in several platforms supporting OpenGL graphics.

## Installation

### Dependencies
This software must be installed in order to use _Scheme Playground_. Follow instructions in their project pages:
* __Gambit Scheme compiler__: best to use latest version: http://www.iro.umontreal.ca/~gambit See Android Support for notes on compiling Gambit for Android
* __Sake__: use this fork: http://github.com/alvatarc/sake
* __Scheme Prelude__: http://github.com/alvatarc/scheme-prelude

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

* __clean__: cleans both host and cross-compiled code
* __run__: runs in host environment
* __android__: compile and install in Android platform

