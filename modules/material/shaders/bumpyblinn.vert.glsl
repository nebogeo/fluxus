// Copyright (C) 2009 Dave Griffiths
// Licence: GPLv2 (see COPYING)
// Fluxus Shader Library
// ---------------------
// Blinn/Phong Shader with bumps

uniform vec3 LightPos;
varying vec3 N;
varying vec3 P;
varying vec3 V;
varying vec3 L;
varying vec2 T;

void main()
{    
    N = normalize(gl_NormalMatrix*gl_Normal);
    P = gl_Vertex.xyz;
    V = -vec3(gl_ModelViewMatrix*gl_Vertex);
	L = vec3(gl_ModelViewMatrix*(vec4(LightPos,1)-gl_Vertex));
	T = gl_MultiTexCoord0.xy;
    gl_Position = ftransform();
}
