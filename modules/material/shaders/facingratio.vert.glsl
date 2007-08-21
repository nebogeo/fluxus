varying vec3 Normal;
varying vec3 EyeDir;

void main()
{    
    Normal = normalize(gl_NormalMatrix*gl_Normal); 
	EyeDir = -vec3(gl_ModelViewMatrix*gl_Vertex);
    gl_Position = ftransform();
}
