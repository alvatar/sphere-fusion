[![Spheres Release](http://img.shields.io/github/release/alvatar/sphere-core.svg)](http://schemespheres.org)
[![Gambit Version Supported](http://img.shields.io/badge/supported Gambit version-4.7.2-blue.svg)](http://schemespheres.org)


## Installation and Quickstart
If you have Sphere Core installed, then

    % spheres install fusion

Otherwise, please read the [quickstart guide](http://www.schemespheres.org/guides/en/quickstart).

### Android support

You need the Android SDK and NDK, an the following variables set:

* ANDROID_SDK_PATH=your_path_to_the_sdk
* ANDROID_NDK_PATH=your_path_to_the_ndk

Then you can compile Gambit for Android using the following script:

    % sh tools/build_android_lib.sh

You might have to edit the script to set the proper paths if you get an error. However, in any case it will break when trying to compile gsi, but you don't need it. You only need _libgambc.a_, copy it from lib/libgambc.a include/gambit.h include/gambit-not{gambit-version}.h to android/jni/gambit

### Updating Gambit System

Follow this process:
* Recompile Gambit for the host
* Recompile Gambit for the Android architecture, and copy the libgambc.a generated file to android/jni/gambit

If you get a segmentation fault with Gambit when loading a module that is precompiled, probably it was compiled with a different Gambit version



[![Bitdeli Badge](https://d2weczhvl823v0.cloudfront.net/alvatar/sphere-fusion/trend.png)](https://bitdeli.com/free "Bitdeli Badge")

