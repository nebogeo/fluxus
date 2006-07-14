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

#include <fstream>
#include <deque>
#include <libguile.h>
#include "FluxusBinding.h"
#include "SearchPaths.h"
#include "Repl.h"
#include <LinePrimitive.h>
#include <ParticlePrimitive.h>
#include <PixelPrimitive.h>
#include <LocatorPrimitive.h>

FluxusMain     *FluxusBinding::Fluxus=NULL;
AudioCollector *FluxusBinding::Audio=NULL;
TurtleBuilder   FluxusBinding::turtle;
SCM		FluxusBinding::FrameHook=NULL;
PolyPrimitive*  FluxusBinding::StaticCube=NULL;
PolyPrimitive*  FluxusBinding::StaticPlane=NULL;
PolyPrimitive*  FluxusBinding::StaticSphere=NULL;
PolyPrimitive*  FluxusBinding::StaticCylinder=NULL;
set<int>        FluxusBinding::m_KeySet;
set<int>        FluxusBinding::m_SpecialKeySet;
deque<int>		FluxusBinding::GrabbedIDStack;
int				FluxusBinding::GrabbedID=-1;

// little helper function for making vectors
// need to check both of these to make sure they are not leaking
SCM FluxusBinding::flx_floats_to_scm(float *d, unsigned int len)
{

	float *t=(float*)malloc(sizeof(float)*len);
	memcpy(t,d,len*sizeof(float));
	SCM ret=scm_take_f32vector(t,len);
	
	// this is probably much slower but means we can use these vectors 
	// normally without having to use functions like (f32vector-set)	
	// I'm sure there is a better way
//	SCM ret=scm_make_vector(scm_from_int(len),scm_from_double(0));	
//	for (unsigned int n=0; n<len; n++) 
//	{
//		scm_vector_set_x(ret,scm_from_int(n),scm_from_double(d[n]));
//	}
	return ret;
}

void FluxusBinding::flx_floats_from_scm(SCM v, float *ptr)
{
	scm_t_array_handle handle;
	size_t lenp;
	ssize_t incp;
	const float *sp = scm_f32vector_elements(scm_any_to_f32vector(v), &handle, &lenp, &incp);
	
	// todo, get rid of this copy
	memcpy(ptr,sp,lenp*sizeof(float));
}

////////////////////////////////////////////////////////////////////////////////////////////////


FluxusBinding::FluxusBinding(int w, int h) 
{
	StaticCube = new PolyPrimitive(PolyPrimitive::QUADS);
    MakeCube(StaticCube);

	StaticPlane = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(StaticPlane);

	StaticSphere = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeSphere(StaticSphere,1,5,10);

	StaticCylinder = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeCylinder(StaticCylinder,1,1,5,10);
	
	Fluxus = new FluxusMain(w,h);

}

FluxusBinding::~FluxusBinding()
{
	if (Audio!=NULL) delete Audio;
	delete Fluxus;
}

SCM FluxusBinding::Prim2Smob(int id)
{
	SCM smob;
	SchemePrim *pb = new SchemePrim(id, Fluxus->GetRenderer());
	SCM_NEWSMOB (smob, SchemePrim::Tag, pb);
	return smob;
}

int FluxusBinding::Smob2Prim(SCM smob)
{
	SchemePrim::Assert(smob);
	SchemePrim *pb = (SchemePrim *)SCM_SMOB_DATA(smob);
	return pb->GetID();
}

///////////////////////////////////////////////////////////////////////////////////////

SCM FluxusBinding::frame_hook()
{
	return FrameHook;
}


SCM FluxusBinding::repl_princ(SCM c)
{
	Fluxus->GetRepl()->Print(c);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::repl_print(SCM s)
{
	// FIXME this is wrong
	if (!scm_is_string(s)) 
		return repl_princ(s);
	Fluxus->GetRepl()->Print(scm_to_locale_string(s));
	return SCM_UNSPECIFIED;
}

void FluxusBinding::RegisterProcs()
{
	scm_c_define_gsubr("repl-princ", 1,0,0,(CALLBACK_CAST)repl_princ);
	scm_c_define_gsubr("repl-print", 1,0,0,(CALLBACK_CAST)repl_print);
    scm_c_define_gsubr("frame-hook",0,0,0,(CALLBACK_CAST) frame_hook);
}
