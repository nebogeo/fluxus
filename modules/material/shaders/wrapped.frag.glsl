uniform vec4 Tint;
uniform float WrapAngle;
varying vec3 Normal;
varying vec3 LightVec;

void main()
{ 
    float lambert = dot(normalize(LightVec),normalize(Normal));
    gl_FragColor = Tint*1-acos(lambert)/WrapAngle;
}
