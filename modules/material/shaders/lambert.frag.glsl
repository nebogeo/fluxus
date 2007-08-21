uniform vec4 Tint;
varying vec3 Normal;
varying vec3 LightVec;

void main()
{ 
    float lambert = dot(normalize(LightVec),normalize(Normal));
    gl_FragColor = Tint*lambert;
}
