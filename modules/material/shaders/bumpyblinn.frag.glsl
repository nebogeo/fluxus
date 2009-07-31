// Copyright (C) 2007 Dave Griffiths
// Licence: GPLv2 (see COPYING)
// Fluxus Shader Library
// ---------------------
// Blinn/Phong Shader with bumps

uniform vec3 AmbientColour;
uniform vec3 DiffuseColour;
uniform vec3 SpecularColour;
uniform float AmbientIntensity;
uniform float DiffuseIntensity;
uniform float SpecularIntensity;
uniform float Roughness;
uniform sampler2D BaseMap;
uniform sampler2D NormalMap;
uniform float Bumpyness;

varying vec3 N;
varying vec3 P;
varying vec3 V;
varying vec3 L;
varying vec2 T;

void main()
{ 
	vec3 bump = normalize(texture2D(NormalMap,T).xyz*2.0-1.0)-vec3(0,0,1);
    vec3 l = normalize(L);
    vec3 n = normalize(N+bump*Bumpyness);
    vec3 v = normalize(V);
    vec3 h = normalize(l+v);

    float diffuse = dot(l,n);
    float specular = pow(max(0.0,dot(n,h)),1/Roughness);
	vec3 base = texture2D(BaseMap, T).xyz;
	
    gl_FragColor = vec4(AmbientColour*base*AmbientIntensity + 
                        DiffuseColour*diffuse*base*DiffuseIntensity +
                        SpecularColour*specular*SpecularIntensity,1);
}
