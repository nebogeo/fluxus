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

#ifdef GLSL
#include "GL/glew.h"
#endif
#ifndef __APPLE__
#include "GL/gl.h"
#include "GL/glu.h"
#include "GL/glut.h"
#else
#include "OpenGL/gl.h"
#include "OpenGL/glu.h"
#include "GLUT/glut.h"
#endif
#include <string>
#include <vector>
#include "dada.h"

#ifndef FLUXUS_GLSL_SHADER
#define FLUXUS_GLSL_SHADER

using namespace std;

namespace Fluxus
{

//////////////////////////////////////////////////////
/// A pair of shaders, loaded and compiled - 
/// needs to be made into a GLSLShader for use
class GLSLShaderPair
{
public:
	/// The constructor attempts to load the shader pair immediately
	GLSLShaderPair(const string &vertexfilename, const string &fragmentfilename);
	~GLSLShaderPair();
	
	unsigned int GetVertexShader() const { return m_VertexShader; }
	unsigned int GetFragmentShader() const { return m_FragmentShader; }
	
private:
	/// Returns a handle to a compiled and linked GLSL program
	bool Load(const string &vertexfilename, const string &fragmentfilename);
	unsigned int LoadShader(string filename, unsigned int type);

	unsigned int m_VertexShader;
	unsigned int m_FragmentShader;
};

//////////////////////////////////////////////////////
/// A hardware shader for use on an object
class GLSLShader
{
public:
	/// The constructor attempts to load the shader pair immediately
	GLSLShader() : m_RefCount(1), m_IsValid(false) {}
	GLSLShader(const GLSLShaderPair &pair);
	~GLSLShader();

	// Temp fix, maybe
	void IncRef() { m_RefCount++; }
	bool DecRef() { m_RefCount--; return (m_RefCount==0); }

	/////////////////////////////////////////////
	///@name Renderer interface
	///@{
	static void Init();
	void Apply();
	static void Unapply();
	bool IsValid() { return m_IsValid; }
	///@}
	
	/////////////////////////////////////////////
	///@name Uniform variables
	///@{
	void SetInt(const string &name, int s);
	void SetFloat(const string &name, float s);
	void SetVector(const string &name, dVector s);
	void SetColour(const string &name, dColour s);
	///@}
	
	/////////////////////////////////////////////
	///@name Attribute variables
	///@{
	void SetFloatArray(const string &name, const vector<float> &s);
	void SetVectorArray(const string &name, const vector<dVector> &s);
	void SetColourArray(const string &name, const vector<dColour> &s);
	///@}
	
	static bool m_Enabled;
	
private:
	unsigned int m_Program;
	unsigned int m_RefCount;
	bool m_IsValid;
};

}

#endif
