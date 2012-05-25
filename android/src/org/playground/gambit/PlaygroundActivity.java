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

public class PlaygroundActivity extends Activity {

    // Components
    /*
    private static GLSurfaceView _surface;
    */
    private static PlaygroundActivity _singleton;
    private static PlaySurface _surface;

    private static PlayThread _playThread;

    private static EGLContext _EGLContext;
    private static EGLSurface _EGLSurface;
    private static EGLDisplay _EGLDisplay;
    private static EGLConfig _EGLConfig;
    private static int _GLMajorVersion;
    private static int _GLMinorVersion;

    static {
        System.loadLibrary("gambit");
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        Log.v(PlayConfig.AppName, "onCreate()");
        super.onCreate(savedInstanceState);

        // Access this from static methods
        _singleton = this;
        // Window features
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
                WindowManager.LayoutParams.FLAG_FULLSCREEN);
        // Set up Surface
        _surface = new PlaySurface(getApplication()); // FIXME: this instead of getApp????
        setContentView(_surface);
        SurfaceHolder holder = _surface.getHolder(); // FIXME: WTF????
    }

    @Override
    protected void onPause() {
        Log.v(PlayConfig.AppName, "onPause()");
        super.onPause();
    }

    @Override
    protected void onResume() {
        Log.v(PlayConfig.AppName, "onResume()");
        super.onResume();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        stopPlayThread();
        Log.v(PlayConfig.AppName, "onDestroy()");
    }

    public static void startPlayThread() {
        if (_playThread == null) {
            _playThread = new PlayThread(PlaygroundActivity.getSingleton());
            _playThread.start();
        } else {
            // Resume? TODO
        }
    }

    public static void stopPlayThread() {
        if (_playThread != null) {
            try {
                _playThread.join();
            } catch(Exception e) {
                Log.v(PlayConfig.AppName, "Problem stopping thread: " + e);
            }
            _playThread = null;
        }
    }

    public static boolean initEGL(int majorVersion, int minorVersion) {
        /*
        if (PlaygroundActivity._EGLDisplay == null) {
            try {
            } catch (Exception e) {
                Log.v(PlayConfig.AppName, e + "");
                for (StackTraceElement s : e.getStackTrace()) {
                    Log.v(PlayConfig.AppName, s.toString());
                }
            }
        }
        else PlaygroundActivity.createEGLSurface();
        */
        
        return true;
    }

    public static boolean createEGLContext() {
        return true;
    }

    public static boolean createEGLSurface() {
        return true;
    }

    public static PlaygroundActivity getSingleton() {
        return _singleton;
    }

    /*
    @Override
    public boolean onTouch(View v, MotionEvent e) {
        Log.v(PlayConfig.AppName, "Surface OnTouch();");
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
    }
    */

    //********************
    // MESSAGE HANDLING
    //********************
    static Handler commandHandler = new Handler() {
        public void handleMessage(Message msg) {
            Log.v(PlayConfig.AppName, "ACTIVITY: message received [" + Thread.currentThread().getId() + "] -- " + (String)msg.obj);
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

class PlayThread extends Thread {
    static PlaygroundActivity _activity;
    static final int MAX_MESSAGES = 10;
        // TODO: use a better data structure
    private Vector<String> _messages = new Vector<String>();

    PlayThread(PlaygroundActivity a) {
        _activity = a;
    }

    public void run() {
        // Set up Gambit
        jniInit();
        initGambit();
        ////////////////////// Here we enter Gambit
        try { 
            while ( true ) { // Instead of a loop, this will be Gambit's execution
                // This block will be called from within Gambit on demand
                if(availableMessages() > 0) {
                    Log.v(PlayConfig.AppName, "GAMBIT: message received [" + Thread.currentThread().getId() + "] -- " + getMessage());
                }
                sleep( 500 ); 
                _activity.receiveMessage("COMMAND FROM GAMBIT TO ACTIVITY (SIMULATED FROM JAVA)");
            }
        }  
        catch( InterruptedException e ) { }
        //////////////////////
    }

    public synchronized void receiveMessage(String m) throws InterruptedException {
        while(_messages.size() == MAX_MESSAGES) {
            wait();
        }
        _messages.addElement(m);
        notify();
    }

    protected synchronized int availableMessages() {
        return _messages.size();
    }

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

    // Called from C
    public static void sendStringMessageToActivity() {
        // TODO: Fill with String argument
        _activity.receiveMessage("GAMBIT SAYS HELLO!!!");
    }

    public static native void initGambit();
    public static native void jniInit();
}




class PlaySurface extends SurfaceView implements SurfaceHolder.Callback, 
    View.OnTouchListener {

    public PlaySurface(Context context) {
        super(context);
        getHolder().addCallback(this); 
    
        setFocusable(true);
        setFocusableInTouchMode(true);
        requestFocus();
        setOnTouchListener(this);   
    }

    // Called when we have a valid drawing surface
    @Override
    public void surfaceCreated(SurfaceHolder holder) {
        Log.v(PlayConfig.AppName, "surfaceCreated()");
        holder.setType(SurfaceHolder.SURFACE_TYPE_GPU);
        PlaygroundActivity.createEGLSurface();
    }

    // Called when we lose the surface
    @Override
    public void surfaceDestroyed(SurfaceHolder holder) {
        Log.v(PlayConfig.AppName, "surfaceDestroyed()");
        //PlaygroundActivity.nativePause();
    }

    // Called when the surface is resized
    @Override
    public void surfaceChanged(SurfaceHolder holder,
                               int format, int width, int height) {
        Log.v(PlayConfig.AppName, "surfaceChanged()");

        int sdlFormat = 0x85151002; // SDL_PIXELFORMAT_RGB565 by default
        switch (format) {
        case PixelFormat.A_8:
            Log.v(PlayConfig.AppName, "pixel format A_8");
            break;
        case PixelFormat.LA_88:
            Log.v(PlayConfig.AppName, "pixel format LA_88");
            break;
        case PixelFormat.L_8:
            Log.v(PlayConfig.AppName, "pixel format L_8");
            break;
        case PixelFormat.RGBA_4444:
            Log.v(PlayConfig.AppName, "pixel format RGBA_4444");
            sdlFormat = 0x85421002; // SDL_PIXELFORMAT_RGBA4444
            break;
        case PixelFormat.RGBA_5551:
            Log.v(PlayConfig.AppName, "pixel format RGBA_5551");
            sdlFormat = 0x85441002; // SDL_PIXELFORMAT_RGBA5551
            break;
        case PixelFormat.RGBA_8888:
            Log.v(PlayConfig.AppName, "pixel format RGBA_8888");
            sdlFormat = 0x86462004; // SDL_PIXELFORMAT_RGBA8888
            break;
        case PixelFormat.RGBX_8888:
            Log.v(PlayConfig.AppName, "pixel format RGBX_8888");
            sdlFormat = 0x86262004; // SDL_PIXELFORMAT_RGBX8888
            break;
        case PixelFormat.RGB_332:
            Log.v(PlayConfig.AppName, "pixel format RGB_332");
            sdlFormat = 0x84110801; // SDL_PIXELFORMAT_RGB332
            break;
        case PixelFormat.RGB_565:
            Log.v(PlayConfig.AppName, "pixel format RGB_565");
            sdlFormat = 0x85151002; // SDL_PIXELFORMAT_RGB565
            break;
        case PixelFormat.RGB_888:
            Log.v(PlayConfig.AppName, "pixel format RGB_888");
            // Not sure this is right, maybe SDL_PIXELFORMAT_RGB24 instead?
            sdlFormat = 0x86161804; // SDL_PIXELFORMAT_RGB888
            break;
        default:
            Log.v(PlayConfig.AppName, "pixel format unknown " + format);
            break;
        }
        //SDLActivity.onNativeResize(width, height, sdlFormat);
        Log.v(PlayConfig.AppName, "Window size:" + width + "x"+height);

        PlaygroundActivity.startPlayThread();
    }

    //public void onDraw(Canvas canvas) {}

    @Override
    public boolean onTouch(View v, MotionEvent event) {
        Log.v(PlayConfig.AppName, "Surface ON TOUCH!");
        return true;
   } 

}

class PlayConfig {
    static String AppName = "Playground";
}

