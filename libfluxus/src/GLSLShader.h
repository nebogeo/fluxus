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

#ifndef FLUXUS_GLSL_SHADER
#define FLUXUS_GLSL_SHADER

#include "OpenGL.h"

#include <string>
#include <vector>
#include "dada.h"
#include "Allocator.h"

using namespace std;

namespace Fluxus
{

//////////////////////////////////////////////////////
/// A pair of shaders, loaded and compiled -
/// needs to be made into a GLSLShader for use
class GLSLShaderPair
{
public:
	/// If load is true, the constructor attempts to load the shader pair immediately
	/// if it's false, the strings are treated as the shader source.
	GLSLShaderPair(bool load, const string &vertex, const string &fragment);
	~GLSLShaderPair();

	unsigned int GetVertexShader() const { return m_VertexShader; }
	unsigned int GetFragmentShader() const { return m_FragmentShader; }

private:
	/// Returns a handle to a compiled and linked GLSL program
	bool Load(const string &vertexfilename, const string &fragmentfilename);
	bool Make(const string &vertexsource, const string &fragmentsource);
	unsigned int LoadShader(string filename, unsigned int type);
	unsigned int MakeShader(const string &filename, const string &source, unsigned int type);

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
	void SetVector(const string &name, dVector s, int size = 4);
	void SetColour(const string &name, dColour s);
	void SetIntArray(const string &name, const vector<int,FLX_ALLOC(int) > &s);
	void SetFloatArray(const string &name, const vector<float,FLX_ALLOC(float) > &s);
	void SetMatrix(const string &name, dMatrix &m);
	void SetVectorArray(const string &name, const vector<dVector,FLX_ALLOC(dVector) > &s);
	void SetColourArray(const string &name, const vector<dColour,FLX_ALLOC(dColour) > &s);
	///@}

	/////////////////////////////////////////////
	///@name Attribute variables
	///@{
	void SetFloatAttrib(const string &name, const vector<float,FLX_ALLOC(float) > &s);
	void SetVectorAttrib(const string &name, const vector<dVector,FLX_ALLOC(dVector) > &s);
	void SetColourAttrib(const string &name, const vector<dColour,FLX_ALLOC(dColour) > &s);
	///@}

	static bool m_Enabled;

private:
	unsigned int m_Program;
	unsigned int m_RefCount;
	bool m_IsValid;
};

}

#endif

