package org.playground.gambit;

import java.util.Random;
import java.util.Vector;

import javax.microedition.khronos.egl.EGL10;
import javax.microedition.khronos.egl.EGLConfig;
import javax.microedition.khronos.egl.EGLContext;
import javax.microedition.khronos.opengles.GL10;
import javax.microedition.khronos.egl.*;

import android.app.Activity;
import android.opengl.GLSurfaceView;
import android.opengl.GLSurfaceView.Renderer;
import android.view.*;
import android.view.View.*;
import android.os.Handler;
import android.os.Message;
import android.os.Bundle;
import android.content.Context;
import android.util.Log;
import android.graphics.*;
import android.view.Window;
import android.view.WindowManager;

/**
    PlaygroundActivity. This is the Activity that handles the GLES drawing thread. 
*/
public class PlaygroundActivity extends Activity {

    private static PlaygroundActivity _singleton;
    private static PGSurface _surface;
    private static PGThread _playThread;

    static {
        System.loadLibrary("gambit");
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        Log.v(PGConfig.AppName, "onCreate()");
        super.onCreate(savedInstanceState);

        // Access this from static methods
        _singleton = this;
        // Window features
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
                WindowManager.LayoutParams.FLAG_FULLSCREEN);
        // Set up Surface
        _surface = new PGSurface(this, getApplication()); // FIXME: this instead of getApp????
        setContentView(_surface);
        SurfaceHolder holder = _surface.getHolder(); // FIXME: WTF????
    }

    @Override
    protected void onPause() {
        Log.v(PGConfig.AppName, "onPause()");
        super.onPause();
    }

    @Override
    protected void onResume() {
        Log.v(PGConfig.AppName, "onResume()");
        super.onResume();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        stopPGThread();
        Log.v(PGConfig.AppName, "onDestroy()");
    }

    public static void startPGThread() {
        if (_playThread == null) {
            _playThread = new PGThread(PlaygroundActivity.getSingleton());
            _playThread.start();
        } else {
            // Resume? TODO
        }
    }

    public static void stopPGThread() {
        if (_playThread != null) {
            try {
                _playThread.join();
            } catch(Exception e) {
                Log.v(PGConfig.AppName, "Problem stopping thread: " + e);
            }
            _playThread = null;
        }
    }


    public static PlaygroundActivity getSingleton() {
        return _singleton;
    }

    public static PGSurface getSurface() {
        return _surface;
    }

    public static PGThread getPGThread() {
        return _playThread;
    }

    ////////////////////////////////////////////////////////////////////////////
    // Message handling
    ////////////////////////////////////////////////////////////////////////////

    static Handler commandHandler = new Handler() {
        @Override
        public void handleMessage(Message msg) {
            Log.v(PGConfig.AppName, "ACTIVITY: message received [" + Thread.currentThread().getId() + "] -- " + (String)msg.obj);
            /*
            if (msg.arg1 == COMMAND_CHANGE_TITLE) {
                setTitle((String)msg.obj);
            }
            */
        }
    };
    void receiveMessage(Object data) {
        Message msg = commandHandler.obtainMessage();
        //msg.arg1 = command;
        msg.obj = data;
        commandHandler.sendMessage(msg);
    }
}

/**
    PGSurface. This is the surface used for grabbing events.
*/
class PGSurface extends SurfaceView implements SurfaceHolder.Callback, 
    View.OnTouchListener {
        PlaygroundActivity _parentActivity;

    public PGSurface(PlaygroundActivity parent, Context context) {
        super(context);
        getHolder().addCallback(this); 
        _parentActivity = parent;
    
        setFocusable(true);
        setFocusableInTouchMode(true);
        requestFocus();
        setOnTouchListener(this);   
    }

    @Override
    public void surfaceCreated(SurfaceHolder holder) {
        Log.v(PGConfig.AppName, "surfaceCreated()");
        holder.setType(SurfaceHolder.SURFACE_TYPE_GPU);
        //PGThread.createEGLSurface();
        /*
        PGThread thread = _parentActivity.getPGThread();
        if (thread != null) {
            thread.createEGLSurfaceInThread();
        }
        */

    }

    @Override
    public void surfaceDestroyed(SurfaceHolder holder) {
        Log.v(PGConfig.AppName, "surfaceDestroyed()");
        //PlaygroundActivity.nativePause();
    }

    @Override
    public void surfaceChanged(SurfaceHolder holder,
                               int format, int width, int height) {
        Log.v(PGConfig.AppName, "surfaceChanged()");

        int sdlFormat = 0x85151002; // SDL_PIXELFORMAT_RGB565 by default
        switch (format) {
        case PixelFormat.A_8:
            Log.v(PGConfig.AppName, "pixel format A_8");
            break;
        case PixelFormat.LA_88:
            Log.v(PGConfig.AppName, "pixel format LA_88");
            break;
        case PixelFormat.L_8:
            Log.v(PGConfig.AppName, "pixel format L_8");
            break;
        case PixelFormat.RGBA_4444:
            Log.v(PGConfig.AppName, "pixel format RGBA_4444");
            sdlFormat = 0x85421002; // SDL_PIXELFORMAT_RGBA4444
            break;
        case PixelFormat.RGBA_5551:
            Log.v(PGConfig.AppName, "pixel format RGBA_5551");
            sdlFormat = 0x85441002; // SDL_PIXELFORMAT_RGBA5551
            break;
        case PixelFormat.RGBA_8888:
            Log.v(PGConfig.AppName, "pixel format RGBA_8888");
            sdlFormat = 0x86462004; // SDL_PIXELFORMAT_RGBA8888
            break;
        case PixelFormat.RGBX_8888:
            Log.v(PGConfig.AppName, "pixel format RGBX_8888");
            sdlFormat = 0x86262004; // SDL_PIXELFORMAT_RGBX8888
            break;
        case PixelFormat.RGB_332:
            Log.v(PGConfig.AppName, "pixel format RGB_332");
            sdlFormat = 0x84110801; // SDL_PIXELFORMAT_RGB332
            break;
        case PixelFormat.RGB_565:
            Log.v(PGConfig.AppName, "pixel format RGB_565");
            sdlFormat = 0x85151002; // SDL_PIXELFORMAT_RGB565
            break;
        case PixelFormat.RGB_888:
            Log.v(PGConfig.AppName, "pixel format RGB_888");
            // Not sure this is right, maybe SDL_PIXELFORMAT_RGB24 instead?
            sdlFormat = 0x86161804; // SDL_PIXELFORMAT_RGB888
            break;
        default:
            Log.v(PGConfig.AppName, "pixel format unknown " + format);
            break;
        }
        //SDLActivity.onNativeResize(width, height, sdlFormat);
        Log.v(PGConfig.AppName, "Window size:" + width + "x" + height);

        PlaygroundActivity.startPGThread();
    }

    //public void onDraw(Canvas canvas) {}

    @Override
    public boolean onTouch(View v, MotionEvent event) {
        Log.v(PGConfig.AppName, "Surface ON TOUCH!");
        /*
        int raw_action = e.getAction();
        int action = raw_action & MotionEvent.ACTION_MASK;
        // Single touch
        int pointerIndex = (raw_action & MotionEvent.ACTION_POINTER_ID_MASK) >> MotionEvent.ACTION_POINTER_ID_SHIFT;
        int pointerId = e.getPointerId(pointerIndex);
        switch (action) {
            case MotionEvent.ACTION_DOWN:
            case MotionEvent.ACTION_POINTER_DOWN:
                //nativeOnTouchDown();
                break;
            case MotionEvent.ACTION_UP:
            case MotionEvent.ACTION_POINTER_UP:
            case MotionEvent.ACTION_CANCEL:
                //nativeOnTouchUp();
                break;
            case MotionEvent.ACTION_MOVE:
                //nativeOnTouchMove();
                break;
        }
        return true;
        */
        return true;
   } 
}

/**
    PGConfig. All globals are placed here.
*/
class PGConfig {
    static final String AppName = "Playground";
    static final int ON_PAUSE = 0;
    static final int ON_RESUME = 1;
}

/**
    PGThread. Class for building the thread that will be used for native
    GLES calls and communication callbacks.
*/
class PGThread extends Thread {
    private static EGLContext _EGLContext;
    private static EGLSurface _EGLSurface;
    private static EGLDisplay _EGLDisplay;
    private static EGLConfig _EGLConfig;
    private static int _GLMajorVersion;
    private static int _GLMinorVersion;

    static PlaygroundActivity _activity;
    static final int MAX_MESSAGES = 10;
        // TODO: use a better data structure
    private Vector<String> _messages = new Vector<String>();

    PGThread(PlaygroundActivity a) {
        _activity = a;
    }

    public void run() {
        PGThread.jniInit();
        // TODO: This can be called from Gambit or here. Where is best? Probably in Gambit, so it can choose configuration
        PGThread.initEGL(1,0);
        try { 
            PGThread.enterGambit();
        }  
        catch( InterruptedException e ) { }
    }

    ////////////////////////////////////////////////////////////////////////////
    // Messaging
    ////////////////////////////////////////////////////////////////////////////

    // PGThread receives a string message
    public synchronized void receiveStringMessage(String m) throws InterruptedException {
        while(pendingMessages() >= MAX_MESSAGES) {
            wait();
        }
        _messages.addElement(m);
        notify();
    }

    // Read the pending messages
    protected synchronized String getMessage() throws InterruptedException {
        //notify();
        //while(_messages.isEmpty()) {
            //wait();
        //}
        //String m = (String)_messages.firstElement();
        //_messages.removeElement(m);
        //return m;
        notify();
        String m = (String)_messages.firstElement();
        _messages.removeElement(m);
        return m;
    }

    public synchronized int pendingMessages() throws InterruptedException {
        return _messages.size();
    }

    // Send a message to the activity
    public static void sendStringMessageToActivity(String m) {
        _activity.receiveMessage(m);
    }

    ////////////////////////////////////////////////////////////////////////////
    // EGL
    ////////////////////////////////////////////////////////////////////////////
    
    public static boolean initEGL(int majorVersion, int minorVersion) {
        Log.v(PGConfig.AppName, "initEGL()");
        if (PGThread._EGLDisplay == null) {
            try {
                EGL10 egl = (EGL10)EGLContext.getEGL();
                EGLDisplay display = egl.eglGetDisplay(EGL10.EGL_DEFAULT_DISPLAY);
                int [] version = new int[2];
                egl.eglInitialize(display, version);
                int EGL_OPENGL_ES_BIT = 1;
                int EGL_OPENGL_ES2_BIT = 4;
                int renderableType = 0;
                if (majorVersion == 2) {
                    renderableType = EGL_OPENGL_ES2_BIT;
                } else if (majorVersion == 1) {
                    renderableType = EGL_OPENGL_ES_BIT;
                }
                int[] configSpec = {
                    EGL10.EGL_RENDERABLE_TYPE,
                    renderableType,
                    EGL10.EGL_NONE
                };
                EGLConfig[] configs = new EGLConfig[1];
                int [] num_config = new int[1];
                if (!egl.eglChooseConfig(display, configSpec, configs, 1, num_config) || num_config[0] == 0) {
                    Log.e("SDL", "No EGL config available");
                    return false;
                }
                EGLConfig config = configs[0];
                PGThread._EGLDisplay = display;
                PGThread._EGLConfig = config;
                PGThread._GLMajorVersion = majorVersion;
                PGThread._GLMinorVersion = minorVersion;
                PGThread.createEGLSurface();
            } catch (Exception e) {
                Log.v(PGConfig.AppName, e + "");
                for (StackTraceElement s : e.getStackTrace()) {
                    Log.v(PGConfig.AppName, s.toString());
                }
            }
        }
        else PGThread.createEGLSurface();
        
        return true;
    }

    public static void flipEGL() {
        try {
            EGL10 egl = (EGL10)EGLContext.getEGL();
            egl.eglWaitNative(EGL10.EGL_CORE_NATIVE_ENGINE, null);
            // drawing here
            egl.eglWaitGL();
            egl.eglSwapBuffers(PGThread._EGLDisplay, PGThread._EGLSurface);
        } catch(Exception e) {
            Log.v(PGConfig.AppName, "flipEGL(): " + e);
            for (StackTraceElement s : e.getStackTrace()) {
                Log.v("SDL", s.toString());
            }
        }
    }

    public static boolean createEGLSurface() {
        if (PGThread._EGLDisplay != null && PGThread._EGLConfig != null) {
            EGL10 egl = (EGL10)EGLContext.getEGL();
            if (PGThread._EGLContext == null) {
                createEGLContext();
            }
            Log.v(PGConfig.AppName, "Creating new EGL Surface");
            EGLSurface surface =
                egl.eglCreateWindowSurface(PGThread._EGLDisplay,
                        PGThread._EGLConfig,
                        PlaygroundActivity.getSurface(),
                        null);
            if (surface == EGL10.EGL_NO_SURFACE) {
                Log.e(PGConfig.AppName, "Couldn't create surface");
                return false;
            }
            if (!egl.eglMakeCurrent(PGThread._EGLDisplay, surface, surface, PGThread._EGLContext)) {
                Log.e(PGConfig.AppName, "Old EGL Context doesnt work, trying with a new one");
                createEGLContext();
                if (!egl.eglMakeCurrent(PGThread._EGLDisplay, surface, surface, PGThread._EGLContext)) {
                    Log.e(PGConfig.AppName, "Failed making EGL Context current");
                    return false;
                }
            }
            PGThread._EGLSurface = surface;
            return true;
        }
        return false;
    }

    // Used by createEGLSurface
    protected static boolean createEGLContext() {
        EGL10 egl = (EGL10)EGLContext.getEGL();
        int EGL_CONTEXT_CLIENT_VERSION=0x3098;
        int contextAttrs[] =
            new int[] { EGL_CONTEXT_CLIENT_VERSION,
                PGThread._GLMajorVersion,
                EGL10.EGL_NONE };
        PGThread._EGLContext =
            egl.eglCreateContext(PGThread._EGLDisplay,
                PGThread._EGLConfig,
                EGL10.EGL_NO_CONTEXT,
                contextAttrs);
        if (PGThread._EGLContext == EGL10.EGL_NO_CONTEXT || PGThread._EGLContext == null) {
            Log.e(PGConfig.AppName, "Couldn't create EGL context");
            return false;
        } else {
            Log.e(PGConfig.AppName, "New EGL context created");
        }
        return true;
    }

    ////////////////////////////////////////////////////////////////////////////
    // Native methods
    ////////////////////////////////////////////////////////////////////////////

    public static native void jniInit();
    public static native void enterGambit() throws InterruptedException;
}
