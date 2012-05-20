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
import android.view.Window;
import android.view.WindowManager;

public class PlaygroundActivity extends Activity implements OnTouchListener {

    // Components
    private static GLSurfaceView _surface;
    private static GambitThread _gambit;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        Log.v(PlaygroundConfig.AppName, "onCreate()");
        super.onCreate(savedInstanceState);
        // Window features
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
                WindowManager.LayoutParams.FLAG_FULLSCREEN);
        // Set up Surface
        _surface = new GLSurfaceView(this);
        _surface.setRenderer(new PlaygroundRenderer());
        _surface.setRenderMode(GLSurfaceView.RENDERMODE_WHEN_DIRTY);
        setContentView(_surface);
        _surface.setFocusable(true);
        _surface.setFocusableInTouchMode(true);
        _surface.requestFocus();
        _surface.setOnTouchListener(this);
        //nativeCreate();

        _gambit = new GambitThread(this);
        _gambit.start();

        /*
           TextView  tv = new TextView(this);
           String fib = testFib();
           String ls = testPorts();

           tv.setText(fib + "\n" + ls);
           setContentView(tv);
           */
    }

    @Override
    protected void onPause() {
        Log.v(PlaygroundConfig.AppName, "onPause()");
        super.onPause();
        //this.nativePause();
        _surface.onPause();
    }

    @Override
    protected void onResume() {
        Log.v(PlaygroundConfig.AppName, "onResume()");
        super.onResume();
        //this.nativeResume();
        _surface.onResume();

        if (_gambit == null) {
            Log.e(PlaygroundConfig.AppName, "This shouldn't happen. A resumed instace had the Gambit thread nullifed");
            _gambit = new GambitThread(this);
            _gambit.start();
        }
        try {
            //_gambit.putMessage("RESUME GAMBIT");
            _gambit.receiveMessage("ON RESUME SENT TO GAMBIT");
        } catch (InterruptedException e) {}
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        Log.v(PlaygroundConfig.AppName, "onDestroy()");
        //this.nativeDestroy();
        /*
        if (_gambit != null) {
            try {
                _gambit.join();
            } catch(Exception e) {
                Log.v(PlaygroundConfig.AppName, "Problem stopping thread: " + e);
            }
            _gambit = null;
        }
        */
    }

    @Override
    public boolean onTouch(View v, MotionEvent e) {
        Log.v(PlaygroundConfig.AppName, "Surface OnTouch();");
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

    //********************
    // MESSAGE HANDLING
    //********************
    static Handler commandHandler = new Handler() {
        public void handleMessage(Message msg) {
            Log.v(PlaygroundConfig.AppName, "ACTIVITY: message received [" + Thread.currentThread().getId() + "] -- " + (String)msg.obj);
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


    static class PlaygroundRenderer implements Renderer {
        @Override
        public void onSurfaceCreated(GL10 gl, EGLConfig config) {
            Log.v(PlaygroundConfig.AppName, "Renderer Surface Created();");
        }

        @Override
        public void onSurfaceChanged(GL10 gl, int width, int height) {
            Log.v(PlaygroundConfig.AppName, "Renderer Surface Changed();");
        }

        Random rand = new Random();
        @Override
        public void onDrawFrame(GL10 gl) {
            //Log.v(PlaygroundConfig.AppName, "Renderer OnDrawFrame();");
            gl.glClearColor(rand.nextFloat(), rand.nextFloat(), rand.nextFloat(), 1);
            gl.glClear(GL10.GL_COLOR_BUFFER_BIT);
        }
    }

    // JNI

    /*
       public static native void nativeCreate();
       public static native void nativePause();
       public static native void nativeResume();
       public static native void nativeDestroy();

       public static native void nativeSurfaceCreated();
       public static native void nativeSurfaceDestroy();
       public static native void nativeSurfaceChanged();
       public static native void nativeOnTouchDown();
       public static native void nativeOnTouchUp();
       public static native void nativeOnTouchMove();

       public native String testFib();

       public native String testPorts();
       */

    /*
    static final int MAX_MESSAGES = 10;
        // TODO: use a better data structure
    private Vector _messages = new Vector();

    public synchronized void putMessage(String m) throws InterruptedException {
        while(_messages.size() == MAX_MESSAGES) {
            wait();
        }
        _messages.addElement(m);
        notify();
        //Log.v(PlaygroundConfig.AppName, m);
    }

    protected synchronized String getMessage() throws InterruptedException {
        notify();
        while(_messages.isEmpty()) {
            wait();
        }
        String m = (String)_messages.firstElement();
        _messages.removeElement(m);
        return m;
    }
    */

    static {
        System.loadLibrary("gambit");
    }
}

class GambitThread extends Thread {
    static PlaygroundActivity _activity;
    static final int MAX_MESSAGES = 10;
        // TODO: use a better data structure
    private Vector<String> _messages = new Vector<String>();

    GambitThread(PlaygroundActivity a) {
        _activity = a;
    }

    /*
    Handler commandHandler = new Handler() {
        public void handleMessage(Message msg) {
            Log.v(PlaygroundConfig.AppName, "GAMBIT: message received [" + Thread.currentThread().getId() + "] -- " + (String)msg.obj);
        }
    };
    void receiveMessage(Object data) {
        Message msg = commandHandler.obtainMessage();
        msg.obj = data;
        commandHandler.sendMessage(msg);
    }
    */

    public void run() {
        // Set up Gambit
        jniInit();
        initGambit();
        ////////////////////// Here we enter Gambit
        try { 
            while ( true ) { // Instead of a loop, this will be Gambit's execution
                // This block will be called from within Gambit on demand
                if(availableMessages() > 0) {
                    Log.v(PlaygroundConfig.AppName, "GAMBIT: message received [" + Thread.currentThread().getId() + "] -- " + getMessage());
                }
                sleep( 500 ); 
                _activity.receiveMessage("COMMAND FROM GAMBIT TO ACTIVITY");
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
        //Log.v(PlaygroundConfig.AppName, m);
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
    public static void sendMessageToActivity() {
        _activity.receiveMessage("GAMBIT SAYS HELLO!!!");
    }

    public static native void initGambit();
    public static native void jniInit();
}

