#ifdef GL_ES
precision mediump float;
precision mediump int;
#endif

attribute vec4 position;

uniform mat4 perspectiveMatrix;

void main() {
    //gl_Position = position; //perspectiveMatrix * position;
    gl_Position = perspectiveMatrix * position;
}
