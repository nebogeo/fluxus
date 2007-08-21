uniform vec3 LightPos;
varying vec3 Normal;
varying vec3 EyeVec;

void main()
{    
    Normal = normalize(gl_NormalMatrix*gl_Normal);
	EyeVec = -gl_Vertex.xyz;
    gl_Position = ftransform();
}
