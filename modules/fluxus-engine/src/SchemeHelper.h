// Copyright (C) 2007 Dave Griffiths
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

#include <string>
#include <escheme.h>
#include "dada.h"
#include "Renderer.h"

#undef MZ_GC_DECL_REG
#undef MZ_GC_UNREG
#define MZ_GC_DECL_REG(size) void *__gc_var_stack__[size+2] = { (void*)0, (void*)size };
#define MZ_GC_UNREG() (GC_variable_stack = (void**)__gc_var_stack__[0])

namespace SchemeHelper
{
	// utility functions to make life easier for binding, and also to allow us to 
	// replace the plt functions used for optimisation later, without having 
	// to rewrite everything...
	
	float FloatFromScheme(Scheme_Object *ob);
	int IntFromScheme(Scheme_Object *ob);
	void FloatsFromScheme(Scheme_Object *src, float *dst, unsigned int size);
	string StringFromScheme(Scheme_Object *ob);
	Scheme_Object *FloatsToScheme(float *src, unsigned int size);
	fluxus::dVector VectorFromScheme(Scheme_Object *src);
	fluxus::dColour ColourFromScheme(Scheme_Object *src);
	fluxus::dQuat QuatFromScheme(Scheme_Object *src);
	fluxus::dMatrix MatrixFromScheme(Scheme_Object *src);
	void ArgCheck(const std::string &funcname, const std::string &format, int argc, Scheme_Object **argv);
	
	#define DECL_ARGV() MZ_GC_DECL_REG(1); \
					    MZ_GC_VAR_IN_REG(0, argv); \
				        MZ_GC_REG(); 
}
