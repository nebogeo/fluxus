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

#include <libguile.h>
#include <GraphicsUtils.h>
#include <set>
#include "AudioCollector.h"
#include "TurtleBuilder.h"
#include "FluxusMain.h"
#include "SchemePrim.h"

#ifndef FLUXUS_BINDING
#define FLUXUS_BINDING

#ifdef NEED_ELLIPSIS_IN_CASTS
#define CALLBACK_CAST SCM (*)(...)
#else
#define CALLBACK_CAST SCM (*)()
#endif

class FluxusBinding
{
public:
	FluxusBinding(int w, int h);
	FluxusBinding() {}
	~FluxusBinding();
	
	static FluxusMain *Fluxus;
	static AudioCollector *Audio;
	static TurtleBuilder turtle;
	static SCM FrameHook;
	static int GrabbedID;
	static deque<int> GrabbedIDStack;

	static PolyPrimitive* StaticCube;
	static PolyPrimitive* StaticPlane;
	static PolyPrimitive* StaticSphere;
	static PolyPrimitive* StaticCylinder;
	
	static set<int> m_KeySet;
	static set<int> m_SpecialKeySet;
	
	void RegisterProcs();
	
	static SCM Prim2Smob(int id);
	static int Smob2Prim(SCM smob);
	
	static SCM flx_floats_to_scm(float *d, unsigned int len);
	static void flx_floats_from_scm(SCM v, float *ptr);

	static SCM frame_hook();
	static SCM repl_princ(SCM c);
	static SCM repl_print(SCM s);
	static SCM flx_major_version();
	static SCM flx_minor_version();

};
#endif
