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

#include <stdio.h>
#include <iostream>

#include "GLSLShader.h"
#include "Trace.h"
#include "SearchPaths.h"
#include "DebugGL.h"

using namespace std;
using namespace Fluxus;

bool GLSLShader::m_Enabled(false);

GLSLShaderPair::GLSLShaderPair(bool load, const string &vertex, const string &fragment) :
m_VertexShader(0),
m_FragmentShader(0)
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
	#ifdef GLSL
	if (!GLSLShader::m_Enabled)
	{
		if (m_VertexShader!=0) glDeleteShader(m_VertexShader);
		if (m_FragmentShader!=0) glDeleteShader(m_FragmentShader);
	}
	#endif
}

bool GLSLShaderPair::Make(const string &vertexsource, const string &fragmentsource)
{
	#ifdef GLSL
	if (!GLSLShader::m_Enabled) return true;

	if (vertexsource.empty())
	{
		m_VertexShader = 0;
	}
	else
	{
		m_VertexShader = MakeShader("Inline vertex shader source",vertexsource,GL_VERTEX_SHADER);
		if (m_VertexShader==0) return false;
	}

	if (fragmentsource.empty())
	{
		m_FragmentShader = 0;
	}
	else
	{
		m_FragmentShader = MakeShader("Inline fragment shader source",fragmentsource,GL_FRAGMENT_SHADER);
		if (m_FragmentShader==0) return false;
	}

	if (!m_VertexShader && !m_FragmentShader)
	{
		Trace::Stream << "No shaders specifed" << endl;
		return false;
	}
	#endif
	return true;
}

bool GLSLShaderPair::Load(const string &vertexfilename, const string &fragmentfilename)
{
	#ifdef GLSL
	if (!GLSLShader::m_Enabled) return true;

	if (vertexfilename.empty())
	{
		m_VertexShader = 0;
	}
	else
	{
		m_VertexShader = LoadShader(SearchPaths::Get()->GetFullPath(vertexfilename),GL_VERTEX_SHADER);
		if (m_VertexShader == 0) return false;
	}

	if (fragmentfilename.empty())
	{
		m_FragmentShader = 0;
	}
	else
	{
		m_FragmentShader = LoadShader(SearchPaths::Get()->GetFullPath(fragmentfilename),GL_FRAGMENT_SHADER);
		if (m_FragmentShader == 0) return false;
	}

	if (!m_VertexShader && !m_FragmentShader)
	{
		Trace::Stream << "No shaders specifed" << endl;
		return false;
	}
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
	if (!GLSLShader::m_Enabled) return 0;
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
	if (!m_Enabled) return;

	m_Program = glCreateProgram();
	if (pair.GetVertexShader())
		glAttachShader(m_Program, pair.GetVertexShader());
	if (pair.GetFragmentShader())
		glAttachShader(m_Program, pair.GetFragmentShader());
	glLinkProgram(m_Program);

	GLint status = GL_FALSE;
	glGetProgramiv(m_Program, GL_LINK_STATUS, &status);
	if (status != GL_TRUE)
	{
		char log[1024];
		glGetProgramInfoLog(m_Program, 1024, NULL, log);
		Trace::Stream << log << endl;
	}

	glValidateProgram(m_Program);
	glGetProgramiv(m_Program, GL_VALIDATE_STATUS, &status);
	if (status != GL_TRUE)
	{
		char log[1024];
		glGetProgramInfoLog(m_Program, 1024, NULL, log);
		Trace::Stream << log << endl;
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

void GLSLShader::SetVector(const string &name, dVector s, int size /* = 4 */)
{
	#ifdef GLSL
	if (!m_Enabled) return;

	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	switch (size)
	{
		case 2:
			glUniform2f(param, s.x, s.y);
			break;
		case 3:
			glUniform3f(param, s.x, s.y, s.z);
			break;
		case 4:
			glUniform4f(param, s.x, s.y, s.z, s.w);
			break;
	}
	CHECK_GL_ERRORS("glUniform");
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

void GLSLShader::SetIntArray(const string &name, const vector<int,FLX_ALLOC(int) > &s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform1iv(param,s.size(),&(*s.begin()));
	#endif
}

void GLSLShader::SetFloatArray(const string &name, const vector<float,FLX_ALLOC(float) > &s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform1fv(param,s.size(),&(*s.begin()));
	#endif
}

void GLSLShader::SetVectorArray(const string &name, const vector<dVector,FLX_ALLOC(dVector) > &s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform4fv(param,s.size(),&s.begin()->x);
	#endif
}

void GLSLShader::SetColourArray(const string &name, const vector<dColour,FLX_ALLOC(dColour) > &s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint param = glGetUniformLocation(m_Program, name.c_str());
	glUniform4fv(param,s.size(),&s.begin()->r);
	#endif
}

void GLSLShader::SetFloatAttrib(const string &name, const vector<float,FLX_ALLOC(float) > &s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint attrib = glGetAttribLocation(m_Program, name.c_str());
	glEnableVertexAttribArray(attrib);
	glVertexAttribPointer(attrib,1,GL_FLOAT,false,0,&(*s.begin()));
	#endif
}

void GLSLShader::SetVectorAttrib(const string &name, const vector<dVector,FLX_ALLOC(dVector) > &s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint attrib = glGetAttribLocation(m_Program, name.c_str());
	glEnableVertexAttribArray(attrib);
	glVertexAttribPointer(attrib,4,GL_FLOAT,false,0,&(*s.begin()));
	#endif
}

void GLSLShader::SetColourAttrib(const string &name, const vector<dColour,FLX_ALLOC(dColour) > &s)
{
	#ifdef GLSL
	if (!m_Enabled) return;
	GLuint attrib = glGetAttribLocation(m_Program, name.c_str());
	glEnableVertexAttribArray(attrib);
	glVertexAttribPointer(attrib,4,GL_FLOAT,false,0,&(*s.begin()));
	#endif
}


