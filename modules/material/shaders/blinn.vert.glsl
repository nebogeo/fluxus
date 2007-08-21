uniform vec3 LightPos;
varying vec3 Normal;
varying vec3 Position;

void main()
{    
    Normal = normalize(gl_NormalMatrix*gl_Normal);
	Position = gl_Vertex.xyz;
    gl_Position = ftransform();
}
