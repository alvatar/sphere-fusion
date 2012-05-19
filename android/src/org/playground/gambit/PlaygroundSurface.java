package org.playground.gambit;

import android.view.*;
import android.view.View.*;
import android.graphics.Canvas;
import android.content.Context;
import android.util.Log;

public class PlaygroundSurface extends SurfaceView
    implements SurfaceHolder.Callback,
               OnTouchListener {

    public PlaygroundSurface(Context context) {
        super(context);
        getHolder().addCallback(this);

        setFocusable(true);
        setFocusableInTouchMode(true);
        requestFocus();
        setOnTouchListener(this);
    }

    @Override
    public void surfaceCreated(SurfaceHolder holder) {
        //Log.v(PlaygroundConfig.AppName, "Surface Created();");
    }

    @Override
    public void surfaceDestroyed(SurfaceHolder holder) {
        //Log.v(PlaygroundConfig.AppName, "Surface Destroyed();");
    }

    @Override
    public void surfaceChanged(SurfaceHolder holder, int format, int width, int height) {
        //Log.v(PlaygroundConfig.AppName, "Surface Changed();");
    }

    /*
    @Override
    public void onDraw(Canvas canvas) {
        Log.v(PlaygroundConfig.AppName, "onDraw();");
    }
    */

    @Override
    public boolean onTouch(View v, MotionEvent e) {
        //Log.v(PlaygroundConfig.AppName, "Surface OnTouch();");
        int raw_action = e.getAction();
        int action = raw_action & MotionEvent.ACTION_MASK;
        // Single touch
        int pointerIndex = (raw_action & MotionEvent.ACTION_POINTER_ID_MASK) >> MotionEvent.ACTION_POINTER_ID_SHIFT;
        int pointerId = e.getPointerId(pointerIndex);
        switch (action) {
            case MotionEvent.ACTION_DOWN:
            case MotionEvent.ACTION_POINTER_DOWN:
                nativeOnTouchDown();
                break;
            case MotionEvent.ACTION_UP:
            case MotionEvent.ACTION_POINTER_UP:
            case MotionEvent.ACTION_CANCEL:
                nativeOnTouchUp();
                break;
            case MotionEvent.ACTION_MOVE:
                nativeOnTouchMove();
                break;
        }
        return true;
    }

    // JNI

    public static native void nativeSurfaceCreated();
    public static native void nativeSurfaceDestroy();
    public static native void nativeSurfaceChanged();
    public static native void nativeOnTouchDown();
    public static native void nativeOnTouchUp();
    public static native void nativeOnTouchMove();
}
