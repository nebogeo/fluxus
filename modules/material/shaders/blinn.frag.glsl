// Copyright (C) 2007 Dave Griffiths
// Licence: GPLv2 (see COPYING)
// Fluxus Shader Library
// ---------------------
// Blinn/Phong Shader
// This is the standard per-fragment lighting
// shading model

uniform vec3 AmbientColour;
uniform vec3 DiffuseColour;
uniform vec3 SpecularColour;
uniform float AmbientIntensity;
uniform float DiffuseIntensity;
uniform float SpecularIntensity;
uniform float Roughness;

varying vec3 N;
varying vec3 P;
varying vec3 V;
varying vec3 L;

void main()
{ 
    vec3 l = normalize(L);
    vec3 n = normalize(N);
    vec3 v = normalize(V);
    vec3 h = normalize(l+v);

    float diffuse = dot(l,n);
    float specular = pow(max(0.0,dot(n,h)),1/Roughness);
    
    gl_FragColor = vec4(AmbientColour*AmbientIntensity + 
                        DiffuseColour*diffuse*DiffuseIntensity +
                        SpecularColour*specular*SpecularIntensity,1);
}
