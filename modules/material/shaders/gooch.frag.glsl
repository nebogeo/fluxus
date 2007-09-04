// Copyright (C) 2007 Dave Griffiths
// Licence: GPLv2 (see COPYING)
// Fluxus Shader Library
// ---------------------
// Gooch NPR Shading Model
// Orginally for technical drawing style 
// rendering, uses warm and cool colours
// to depict shading to keep detail in the
// shadowed areas

uniform vec3 WarmColour;
uniform vec3 CoolColour;
uniform vec3 SurfaceColour;
uniform float OutlineWidth;

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
    float specular = pow(dot(n,h),32.0);
    
    vec3 cool = min(CoolColour+SurfaceColour,1.0);
    vec3 warm = min(WarmColour+SurfaceColour,1.0);
    
    vec3 colour = min(mix(cool,warm,diffuse)+specular,1.0);
    
    if (dot(n,v)<OutlineWidth) colour=vec3(0,0,0);

    gl_FragColor = vec4(colour,1);
}
