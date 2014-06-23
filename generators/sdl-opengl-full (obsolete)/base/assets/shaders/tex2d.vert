#ifdef GL_ES
precision highp float;
precision mediump int;
#endif

attribute vec2 position;
attribute vec2 texCoord;

varying vec2 colorCoord;

uniform mat4 perspectiveMatrix;

void main()
{
    colorCoord = texCoord;
    gl_Position = perspectiveMatrix * vec4(position, 0.0, 1.0);
}
