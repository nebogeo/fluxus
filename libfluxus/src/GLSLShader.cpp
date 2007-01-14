// Copyright (C) 2005 Dave Griffiths
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "GLSLShader.h"
#include <iostream>

using namespace std;
using namespace fluxus;

bool GLSLShader::m_Enabled(false);

GLSLShader::GLSLShader(const string &vertexfilename, const string &fragmentfilename) :
m_Program(0)
{
	Load(vertexfilename, fragmentfilename);
}

GLSLShader::~GLSLShader()
{
	if (!m_Enabled) return;
	glDeleteProgram(m_Program);
}

void GLSLShader::Init()
{
	m_Enabled = glewIsSupported("GL_VERSION_2_0");
}

void GLSLShader::Apply() 
{ 
	if (!m_Enabled) return;
	glUseProgram(m_Program); 
}

void GLSLShader::Unapply() 
{ 
	if (!m_Enabled) return;
	glUseProgram(0); 
}

void GLSLShader::SetInt(const string &name, int s)
{
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform1i(param,s);
}

void GLSLShader::SetFloat(const string &name, float s)
{
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform1f(param,s);
}

void GLSLShader::SetVector(const string &name, dVector s)
{
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform3f(param,s.x,s.y,s.z);
}

void GLSLShader::SetColour(const string &name, dColour s)
{
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform4f(param,s.r,s.g,s.b,s.a);
}

void GLSLShader::SetFloatArray(const string &name, const vector<float> &s)
{
	if (!m_Enabled) return;
	GLuint attrib = glGetAttribLocation(m_Program, name.c_str());
	glEnableVertexAttribArray(attrib);
	glVertexAttribPointer(attrib,1,GL_FLOAT,false,0,&(*s.begin()));
}

void GLSLShader::SetVectorArray(const string &name, const vector<dVector> &s)
{
	if (!m_Enabled) return;
	GLuint attrib = glGetAttribLocation(m_Program, name.c_str());
	glEnableVertexAttribArray(attrib);
	glVertexAttribPointer(attrib,4,GL_FLOAT,false,0,&(*s.begin()));
}

void GLSLShader::SetColourArray(const string &name, const vector<dColour> &s)
{
	if (!m_Enabled) return;
	GLuint attrib = glGetAttribLocation(m_Program, name.c_str());
	glEnableVertexAttribArray(attrib);
	glVertexAttribPointer(attrib,4,GL_FLOAT,false,0,&(*s.begin()));
}

bool GLSLShader::Load(const string &vertexfilename, const string &fragmentfilename)
{
	if (!m_Enabled) return true;
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

	GLint status = GL_FALSE;
	glGetProgramiv(m_Program, GL_LINK_STATUS, &status);
	if(status != GL_TRUE)
	{
		GLsizei size = 0;
		char log[1024];
		glGetProgramInfoLog(m_Program, 1024, &size, log);
		cout<<log<<endl;
	}

	return true;
}
	
	
unsigned int GLSLShader::LoadShader(string filename, unsigned int type)
{
	if (!m_Enabled) return 0;
	FILE* file = fopen(filename.c_str(), "r");
	if (!file) return 0;

	fseek(file, 0, SEEK_END);
	unsigned int size = ftell(file);
	fseek(file, 0, SEEK_SET);

	char* code = new char[size+1];
	code[size]='\0';

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

		GLint status = GL_FALSE;
		glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
		if(status != GL_TRUE)
		{	
			GLsizei size = 0;
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
