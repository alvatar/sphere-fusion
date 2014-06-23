[![Spheres Release](http://img.shields.io/github/release/alvatar/sphere-core.svg)](http://schemespheres.org)
[![Gambit Version Supported](http://img.shields.io/badge/supported Gambit version-4.7.2-blue.svg)](http://schemespheres.org)


## Installation and Quickstart
If you have Sphere Core installed, then

    % sspheres install fusion

Otherwise, please read the [quickstart guide](http://www.schemespheres.org/guides/en/quickstart).

### Setting up Android support

1. Install Android [SDK](https://developer.android.com/sdk/index.html) and [NDK](https://developer.android.com/tools/sdk/ndk/index.html).

2. Set up the following environment paths depending on your installation:

```
export ANDROID_SDK_PATH=your_path_to_the_sdk
export ANDROID_NDK_PATH=your_path_to_the_ndk
```

The following sections only apply if you need to use a different Gambit version different to the one configured in the generators.

#### Updating Gambit in your project and generators

If you get a segmentation fault with Gambit when loading a module that is precompiled, probably it was compiled with a different Gambit version. Remember that you need to do two things:

* Recompile Gambit for the host.
* Update the project files, following the process outlined in __Generating Android support files__.

#### Generating Android support files

These files are already bundled in Fusion, so you don't need to follow this process unless you have a different Gambit version. If that is the case, follow this process:

1. You need the following variables set, as described above:

```
export ANDROID_SDK_PATH=your_path_to_the_sdk
export ANDROID_NDK_PATH=your_path_to_the_ndk
```

2. Then you can compile Gambit for Android using the following script:

```
sh tools/build_android_lib.sh
```

3. You might have to edit the script to set the proper paths if you get an error. However, in any case it will break when trying to compile gsi, but you don't need it. You only need _libgambc.a_, copy it from _lib/libgambc.a_ to _android/jni/gambit_.

4. Copy _include/gambit.h_ and _include/gambit-not{gambit-version}.h_ to _android/jni/gambit_.

