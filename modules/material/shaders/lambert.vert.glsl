uniform vec3 LightPos;
varying vec3 Normal;
varying vec3 LightVec;

void main()
{    
    Normal = normalize(gl_NormalMatrix*gl_Normal);
	LightVec = LightPos-gl_Vertex.xyz;
    gl_Position = ftransform();
}
