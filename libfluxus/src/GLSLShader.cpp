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
#include "Trace.h"
#include "SearchPaths.h"

using namespace std;
using namespace Fluxus;

bool GLSLShader::m_Enabled(false);

GLSLShaderPair::GLSLShaderPair(bool load, const string &vertex, const string &fragment)
{
	if (load)
	{
		if (!Load(vertex, fragment))
		{
			Trace::Stream<<"Problem loading shaderpair ["<<vertex<<", "<<fragment<<"]"<<endl;
		}
	}
	else
	{
		if (!Make(vertex, fragment))
		{
			Trace::Stream<<"Problem making shaderpair"<<endl;
		}
	}
}

GLSLShaderPair::~GLSLShaderPair()
{
}

bool GLSLShaderPair::Make(const string &vertexsource, const string &fragmentsource)
{
	#ifdef GLSL
	if (!GLSLShader::m_Enabled) return true;

	m_VertexShader = MakeShader("Inline vertex shader source",vertexsource,GL_VERTEX_SHADER);
	if (m_VertexShader==0) return false; 
	m_FragmentShader = MakeShader("Inline fragment shader source",fragmentsource,GL_FRAGMENT_SHADER);
	if (m_FragmentShader==0) return false; 

	#endif
	return true;
}

bool GLSLShaderPair::Load(const string &vertexfilename, const string &fragmentfilename)
{
	#ifdef GLSL
	if (!GLSLShader::m_Enabled) return true;

	m_VertexShader = LoadShader(SearchPaths::Get()->GetFullPath(vertexfilename),GL_VERTEX_SHADER);
	if (m_VertexShader==0) return false; 
	m_FragmentShader = LoadShader(SearchPaths::Get()->GetFullPath(fragmentfilename),GL_FRAGMENT_SHADER);
	if (m_FragmentShader==0) return false; 

	#endif
	return true;
}
	
	
unsigned int GLSLShaderPair::LoadShader(string filename, unsigned int type)
{
	#ifdef GLSL
	if (!GLSLShader::m_Enabled) return 0;
	FILE* file = fopen(filename.c_str(), "r");
	if (!file) 
	{
		Trace::Stream<<"Couldn't open shader ["<<filename<<"]"<<endl;
		return 0;
	}

	fseek(file, 0, SEEK_END);
	unsigned int size = ftell(file);
	fseek(file, 0, SEEK_SET);

	char* code = new char[size+1];
	code[size]='\0';

	if (fread(code,1,size,file)!=size)
	{
		Trace::Stream<<"Error reading shader ["<<filename<<"]"<<endl;
		delete[] code;
		fclose(file);
		return 0;
	}
	else
	{
		unsigned int shader = MakeShader(filename,code,type);		
		delete[] code;
		fclose(file);
		return shader;
	}
	#endif
	return 0;
}


unsigned int GLSLShaderPair::MakeShader(const string &filename, const string &source, unsigned int type)
{
	#ifdef GLSL
	unsigned int shader = glCreateShader(type);
	const char *t = source.c_str();
	glShaderSource(shader, 1, &t, NULL);

	glCompileShader(shader);

	GLint status = GL_FALSE;
	glGetShaderiv(shader, GL_COMPILE_STATUS, &status);
	if(status != GL_TRUE)
	{	
		GLsizei size = 0;
		char log[1024];

		glGetShaderInfoLog(shader, 1024, &size, log);
		Trace::Stream<<"compile errors for ["<<filename<<"]"<<endl;
		Trace::Stream<<log<<endl;

		glDeleteShader(shader);
		return 0;
	}
	return shader;
	#else
	return 0;
	#endif
}



/////////////////////////////////////////

GLSLShader::GLSLShader(const GLSLShaderPair &pair) :
m_Program(0),
m_RefCount(1)
{	
	#ifdef GLSL
	m_Program = glCreateProgram();
	glAttachShader(m_Program, pair.GetVertexShader());
	glAttachShader(m_Program, pair.GetFragmentShader());
	glLinkProgram(m_Program);

	GLint status = GL_FALSE;
	glGetProgramiv(m_Program, GL_LINK_STATUS, &status);
	if(status != GL_TRUE)
	{
		GLsizei size = 0;
		char log[1024];
		glGetProgramInfoLog(m_Program, 1024, &size, log);
		Trace::Stream<<log<<endl;
	}	
	else
	{
		m_IsValid = true;
	}
	#endif
}

GLSLShader::~GLSLShader()
{
	#ifdef GLSL
	if (!m_Enabled) return;
	glDeleteProgram(m_Program);
	#endif
}

void GLSLShader::Init()
{
	#ifdef GLSL
	m_Enabled = glewIsSupported("GL_VERSION_2_0");
	#endif
}

void GLSLShader::Apply() 
{ 
	#ifdef GLSL
	if (!m_Enabled) return;
	glUseProgram(m_Program); 
	#endif
}

void GLSLShader::Unapply() 
{ 
	#ifdef GLSL
	if (!m_Enabled) return;
	glUseProgram(0); 
	#endif
}

void GLSLShader::SetInt(const string &name, int s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform1i(param,s);
	#endif
}

void GLSLShader::SetFloat(const string &name, float s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform1f(param,s);
	#endif
}

void GLSLShader::SetVector(const string &name, dVector s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform3f(param,s.x,s.y,s.z);
	#endif
}

void GLSLShader::SetColour(const string &name, dColour s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform4f(param,s.r,s.g,s.b,s.a);
	#endif
}

void GLSLShader::SetFloatArray(const string &name, const vector<float> &s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint attrib = glGetAttribLocation(m_Program, name.c_str());
	glEnableVertexAttribArray(attrib);
	glVertexAttribPointer(attrib,1,GL_FLOAT,false,0,&(*s.begin()));
	#endif
}

void GLSLShader::SetVectorArray(const string &name, const vector<dVector> &s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint attrib = glGetAttribLocation(m_Program, name.c_str());
	glEnableVertexAttribArray(attrib);
	glVertexAttribPointer(attrib,4,GL_FLOAT,false,0,&(*s.begin()));
	#endif
}

void GLSLShader::SetColourArray(const string &name, const vector<dColour> &s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint attrib = glGetAttribLocation(m_Program, name.c_str());
	glEnableVertexAttribArray(attrib);
	glVertexAttribPointer(attrib,4,GL_FLOAT,false,0,&(*s.begin()));
	#endif
}


