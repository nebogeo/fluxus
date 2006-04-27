#include <GL/glew.h>
#include <GL/glu.h>
#include <GL/glut.h>
#include <string>

#ifndef FLUXUS_GLSL_SHADER
#define FLUXUS_GLSL_SHADER

class GLSLShader
{
public:
	GLSLShader(const std::string &vertexfilename, const std::string &fragmentfilename);
	~GLSLShader();
	
	/// Returns a handle to a compiled and linked GLSL program
	bool Load(const std::string &vertexfilename, const std::string &fragmentfilename);
	
	void Use() { glUseProgram(m_Program); }

	
private:

	unsigned int LoadShader(std::string filename, unsigned int type);
	unsigned int m_Program;

};

#endif
