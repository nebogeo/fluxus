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

#include "dada.h"
#include "iostream"  
                 	
#ifndef __APPLE__
#include "GL/gl.h"
#include "GL/glu.h"
#include "GL/glut.h"
#else
#include "OpenGL/gl.h"
#include "OpenGL/glu.h"
#include "GLUT/glut.h"
#endif

#ifndef N_STATE
#define N_STATE

namespace fluxus
{

#define HINT_NONE     0x0000
#define HINT_SOLID    0x0001
#define HINT_WIRE     0x0002
#define HINT_NORMAL   0x0004
#define HINT_POINTS   0x0008
#define HINT_AALIAS   0x0010
#define HINT_BOUND    0x0020
#define HINT_UNLIT    0x0040
#define HINT_VERTCOLS 0x0080
#define HINT_MULTITEX 0x0100

#define ENABLE_MULTITEXTURE

#ifdef ENABLE_MULTITEXTURE
#define MAX_TEXTURES  8
#else
#define MAX_TEXTURES  1
#endif

class State
{
public:
	State();
	
	void Apply();
	void Spew();
	
	dColour Colour;
	dColour Specular;
	dColour Emissive;
	dColour Ambient;
	float Shinyness;
	float Opacity;
	unsigned int Textures[MAX_TEXTURES];
	
	int Parent;
	int Hints;
	float LineWidth;
	float PointWidth;
	
	int SourceBlend;
	int DestinationBlend;
	
	dColour WireColour;
	
	void SetBlendMode(string s, string d);
	
	dMatrix Transform;
};

};

#endif
