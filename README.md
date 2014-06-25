[![Spheres Release](http://img.shields.io/github/release/alvatar/sphere-core.svg)](http://schemespheres.org)
[![Gambit Version Supported](http://img.shields.io/badge/supported Gambit version-4.7.2-blue.svg)](http://schemespheres.org)


## Installation and Quickstart
If you have Sphere Core installed, then

    % sspheres install fusion

Otherwise, please read the [quickstart guide](http://www.schemespheres.org/guides/en/quickstart).

### Android support

1. Install Android [SDK](https://developer.android.com/sdk/index.html) and [NDK](https://developer.android.com/tools/sdk/ndk/index.html).

2. Set up the following environment paths depending on your installation:

```
export ANDROID_SDK_PATH=your_path_to_the_sdk
export ANDROID_NDK_PATH=your_path_to_the_ndk
```

### iOS support (only on OSX)

No special configuration is needed for SchemeSpheres on iOS, besides Xcode and Apple developer certificates, just as any other iOS app.



## Advanced: generating and updating support files for multiplatform projects

### Updating Gambit for the host platform

If you get a segmentation fault with Gambit when loading a module that is precompiled, probably it was compiled with a different Gambit version. Remember that you need to rebuild everything:

* Recompile Gambit for the host.
* Recompile all installed Spheres
* Recompile all files in the project

### Generating Android support files

These files are already bundled in Fusion, so this is only necessary for rebuilding in case of Gambit update:

1. First, you need to set these variables, as described above:

```
export ANDROID_SDK_PATH=your_path_to_the_sdk
export ANDROID_NDK_PATH=your_path_to_the_ndk
```

2. Then run the compilation script:

```
tools/build-gambit-android
```

### Generating iOS support files

Just as the Android support files, these files are already bundled in Fusion, so this is only necessary for rebuilding the static lib in case of Gambit update. You only need to run the compilation script (it takes very long):

```
tools/build-gambit-iOS
```

### Updating Android generators

* Update Gambit:

```
sphere-fusion/tools/build-gambit-android :: compile Gambit static lib for Android
{project_directory}/android/jni-generator/deps/gambit/* :: update with new libgambc.a and gambit*.h
All dependencies (C code generated in other spheres) must be updated as well
```

* Update SDL2:

```
{sphere-sdl2}/deps/* :: update SDL2, and the sphere if necessary, then reinstall it
{project_directory}/sakefile.scm :: update SDL2 symlink to {sphere-sdl2}/deps/* in android:setup
{project_directory}/android/jni-generator/deps/SDL :: remove old link and run android:setup
{project_directory}/android/src-generator/* :: update SDL2 Java classes (overwrite)

```

### Updating iOS generators

* Update Gambit:

```
sphere-fusion/tools/build-gambit-iOS
{project_directory}/ios/gambit/* :: generated libgambc.a and gambit*.h
All dependencies (C code generated in other spheres) must be updated as well
```

* Update SDL2:

1. Download and unpack SDL2. Open the _SDL_ project on the _Xcode-iOS_ folder. Once in Xcode, build for both simulator and device (arm7, arm7s).
2. In Xcode, select "Show in Finder" (right button on Xcode’s generated library files). You need to do this after each compilation, to build a fat library from the ones for specific architectures.
3. Rename each file (SDLs and SDLd) and copy them into the same folder. Note: you might need a third library if you are supporting both arm7 and arm7s.
4. Combine them with: lipo libSDL2d.a libSDL2s.a -create -output libSDL2.a
5. Substitute the generated libSDL2.a within your project or generator in
```
{project_directory}/ios/SDL/* :: generated libgambc.a and gambit*.h
```

If SDL2 template changed drastically, you will need to generate the template from scratch, based on the one bundled with SDL2. You should continue from previous step 4 in that case:

1. Copy libSDL2.a in the _Template_ project in the _Xcode-iOS_ folder, inside a folder called _SDL/lib_.
2. Create a symlink (or copy) the SDL/include dir inside the just created SDL folder. For SchemeSpheres this is better handled automatically in _sakefile.scm_, within the _ios:setup_ task. The resulting directory should have _SDL/include_ (with the include files) and SDL/lib (with the combined libSDL2.a)
3. Open the _Template_ project in _Xcode-iOS_ folder, remove the link to the SDL folder, then add the one we've just created. Finally, change the project and scheme names.
4. Add support for Gambit and SchemeSpheres. __This needs further documentation__.

