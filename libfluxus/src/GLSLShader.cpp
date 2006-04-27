#include "GLSLShader.h"
#include <iostream>

using namespace std;

GLSLShader::GLSLShader(const string &vertexfilename, const string &fragmentfilename) :
m_Program(0)
{
	Load(vertexfilename, fragmentfilename);
}

bool GLSLShader::Load(const string &vertexfilename, const string &fragmentfilename)
{
	if (m_Program!=0) glDeleteProgram(m_Program);

	m_Program = glCreateProgram();

	bool bOk = true;
	unsigned int vertex = LoadShader(vertexfilename,GL_VERTEX_SHADER);
	if (vertex==0) 
	{ 
		glDeleteProgram(m_Program);
		m_Program = 0;
		return false; 
	}
	glAttachShader(m_Program, vertex);

	unsigned int fragment = LoadShader(fragmentfilename,GL_FRAGMENT_SHADER);
	if (fragment==0) 
	{ 
		glDeleteProgram(m_Program);
		m_Program = 0;
		return false; 
	}
	glAttachShader(m_Program, fragment);

	glLinkProgram(m_Program);

	int status = GL_FALSE;
	glGetProgramiv(m_Program, GL_LINK_STATUS, &status);
	if(status != GL_TRUE)
	{
		int size = 0;
		char log[1024];
		glGetProgramInfoLog(m_Program, 1024, &size, log);
		cout<<log<<endl;
	}
	
	return true;
}
	
	
unsigned int GLSLShader::LoadShader(string filename, unsigned int type)
{
	FILE* file = fopen(filename.c_str(), "r");
	if (!file) return 0;

	fseek(file, 0, SEEK_END);
	unsigned int size = ftell(file);
	fseek(file, 0, SEEK_SET);

	char* code = new char[size+1];

	if (fread(code,1,size,file)!=size)
	{
		cerr<<"Error reading shader ["<<filename<<"]"<<endl;
		delete[] code;
		return 0;
	}
	else
	{
		unsigned int shader = glCreateShader(type);
		glShaderSource(shader, 1, (const char**)&code, NULL);
		glCompileShader(shader);

		int status = GL_FALSE;
		glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
		if(status != GL_TRUE)
		{	
			int size = 0;
			char log[1024];

			glGetShaderInfoLog(shader, 1024, &size, log);
			cerr<<"compile errors for ["<<filename<<"]"<<endl;
			cerr<<log<<endl;

			glDeleteShader(shader);
			delete[] code;
			return 0;
		}
		
		delete[] code;
		return shader;
	}
	
	return 0;
}
