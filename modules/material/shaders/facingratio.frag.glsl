uniform vec4 OuterColour;
uniform vec4 InnerColour;

varying vec3 Normal;
varying vec3 EyeDir;

void main()
{ 
    float ratio = dot(normalize(EyeDir),normalize(Normal));
    clamp(ratio,0.0,1.0);
    gl_FragColor = mix(OuterColour,InnerColour,ratio);
}
