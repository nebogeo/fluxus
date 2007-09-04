// Copyright (C) 2007 Dave Griffiths
// Licence: GPLv2 (see COPYING)
// Fluxus Shader Library
// ---------------------
// Wood Shader
// Cheezy procedural wood grain shader

uniform vec3 AmbientColour;
uniform vec3 DiffuseColour1;
uniform vec3 DiffuseColour2;
uniform vec3 SpecularColour;
uniform float AmbientIntensity;
uniform float DiffuseIntensity;
uniform float SpecularIntensity1;
uniform float SpecularIntensity2;
uniform float Roughness;
uniform vec3 GrainCentre;
uniform float GrainMult;

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
	
    float d = length(P-GrainCentre);
    float g = clamp(sin(d*GrainMult),0.0,1.0);
  
    vec3 DiffuseColour = mix(DiffuseColour1,DiffuseColour2,g);
    float SpecularIntensity = mix(SpecularIntensity1,
                                  SpecularIntensity2,g);

    float diffuse = dot(l,n);
    float specular = pow(dot(n,h),1/Roughness);

    gl_FragColor = vec4(AmbientColour*AmbientIntensity + 
                        DiffuseColour*diffuse*DiffuseIntensity +
                        SpecularColour*specular*SpecularIntensity,1);
}
