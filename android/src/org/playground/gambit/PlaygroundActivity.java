package org.playground.gambit;

import android.app.Activity;
import android.os.Bundle;
import android.util.Log;
import android.view.Window;
import android.view.WindowManager;
//import android.widget.TextView;

public class PlaygroundActivity extends Activity {

  // Components
  private static PlaygroundSurface _surface;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    Log.v(PlaygroundConfig.AppName, "onCreate()");
    super.onCreate(savedInstanceState);

    requestWindowFeature(Window.FEATURE_NO_TITLE);
    getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
            WindowManager.LayoutParams.FLAG_FULLSCREEN);

    // Set up Surface
    _surface = new PlaygroundSurface(this);
    setContentView(_surface);
    // Set up Gambit
    initGambit();
    // Callbacks
    this.nativeCreate();

    /*
    TextView  tv = new TextView(this);
    String fib = testFib();
    String ls = testPorts();

    tv.setText(fib + "\n" + ls);
    setContentView(tv);
    */
  }

  protected void onPause() {
    Log.v(PlaygroundConfig.AppName, "onPause()");
    super.onPause();
    this.nativePause();
  }

  protected void onResume() {
    Log.v(PlaygroundConfig.AppName, "onResume()");
    super.onResume();
    this.nativeResume();
  }

  protected void onDestroy() {
    super.onDestroy();
    Log.v(PlaygroundConfig.AppName, "onDestroy()");
    this.nativeDestroy();
  }

  // JNI

  public native void initGambit();

  public static native void nativeCreate();

  public static native void nativePause();

  public static native void nativeResume();

  public static native void nativeDestroy();



  public native String testFib();

  public native String testPorts();

  static {
    System.loadLibrary("gambit");
  }

}
