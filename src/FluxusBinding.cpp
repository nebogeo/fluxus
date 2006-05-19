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
deque<int>		FluxusBinding::GrabbedIDStack;
int				FluxusBinding::GrabbedID=-1;

// little helper function for making vectors
// need to check both of these to make sure they are not leaking
SCM flx_floats_to_scm(float *d, unsigned int len)
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

void flx_floats_from_scm(SCM v, float *ptr)
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
    MakePlane(StaticCube);

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

SCM FluxusBinding::build_polygons(SCM s_size, SCM s_type)
{
	SCM_ASSERT(scm_is_number(s_size), s_size, SCM_ARG1, "build-polygons");
	SCM_ASSERT(scm_is_number(s_type), s_type, SCM_ARG2, "build-polygons");
	
	PolyPrimitive *Prim = new PolyPrimitive((PolyPrimitive::Type)scm_to_int(s_type));
	Prim->Resize(scm_to_int(s_size));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusBinding::build_nurbs(SCM s_size)
{
	SCM_ASSERT(scm_is_number(s_size), s_size, SCM_ARG1, "build-nurbs");
	
	NURBSPrimitive *Prim = new NURBSPrimitive();
	Prim->Resize(scm_to_int(s_size));
	return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusBinding::build_cube()
{
	PolyPrimitive *BoxPrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakeCube(BoxPrim);
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(BoxPrim));    
}

SCM FluxusBinding::build_sphere(SCM s_hsegments, SCM s_rsegments)
{
	SCM_ASSERT(scm_is_number(s_hsegments), s_hsegments, SCM_ARG1, "build_sphere");
	SCM_ASSERT(scm_is_number(s_rsegments), s_rsegments, SCM_ARG2, "build_sphere");
	int hsegments=scm_to_int(s_hsegments);
    int rsegments=scm_to_int(s_rsegments);	
	
	PolyPrimitive *SphPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeSphere(SphPrim, 1, hsegments, rsegments);
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(SphPrim));
}

SCM FluxusBinding::build_plane()
{
	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(PlanePrim);
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(PlanePrim));
}

SCM FluxusBinding::build_plane(SCM s_xsegments, SCM s_ysegments)
{
	SCM_ASSERT(scm_is_number(s_xsegments), s_xsegments, SCM_ARG1, "build-plane");
	SCM_ASSERT(scm_is_number(s_ysegments), s_ysegments, SCM_ARG2, "build-plane");

	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(PlanePrim,scm_to_int(s_xsegments),scm_to_int(s_ysegments));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(PlanePrim));
}

SCM FluxusBinding::build_cylinder(SCM s_hsegments, SCM s_rsegments)
{
	SCM_ASSERT(scm_is_number(s_hsegments), s_hsegments, SCM_ARG1, "build_cylinder");
	SCM_ASSERT(scm_is_number(s_rsegments), s_rsegments, SCM_ARG2, "build_cylinder");
	
	PolyPrimitive *CylPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeCylinder(CylPrim, 1, 1, scm_to_int(s_hsegments), scm_to_int(s_rsegments));

    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(CylPrim));
}

SCM FluxusBinding::build_line(SCM s_numpoints)
{
	SCM_ASSERT(scm_is_number(s_numpoints), s_numpoints, SCM_ARG1, "build-line");
	
	LinePrimitive *Prim = new LinePrimitive();
	Prim->Resize(scm_to_int(s_numpoints));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusBinding::build_text(SCM s_text)
{
	SCM_ASSERT(scm_is_string(s_text), s_text, SCM_ARG1, "build_text");
	
	char *text=scm_to_locale_string(s_text);
	
	TextPrimitive *TextPrim = new TextPrimitive(15/256.0f,25/256.0f,17,40);
	TextPrim->SetText(text,20,-20);
	
	return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(TextPrim));
}
	
SCM FluxusBinding::build_nurbs_sphere(SCM s_hsegments, SCM s_rsegments)
{
	SCM_ASSERT(scm_is_number(s_hsegments), s_hsegments, SCM_ARG1, "build_nurbs_sphere");
	SCM_ASSERT(scm_is_number(s_rsegments), s_rsegments, SCM_ARG2, "build_nurbs_sphere");
	
	NURBSPrimitive *SphPrim = new NURBSPrimitive;
    MakeNURBSSphere(SphPrim, 1, scm_to_int(s_hsegments), scm_to_int(s_rsegments));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(SphPrim));
}

SCM FluxusBinding::build_nurbs_plane(SCM s_usegments, SCM s_vsegments)
{
	SCM_ASSERT(scm_is_number(s_usegments), s_usegments, SCM_ARG1, "build_nurbs_sphere");
	SCM_ASSERT(scm_is_number(s_vsegments), s_vsegments, SCM_ARG2, "build_nurbs_sphere");
		
	NURBSPrimitive *Prim = new NURBSPrimitive;
    MakeNURBSPlane(Prim, scm_to_int(s_usegments), scm_to_int(s_vsegments));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusBinding::build_particles(SCM s_count)
{
	SCM_ASSERT(scm_is_number(s_count), s_count, SCM_ARG1, "build_particles");
	ParticlePrimitive *Prim = new ParticlePrimitive;
	int count=scm_to_int(s_count);
	for (int i=0; i<count; i++)
	{
		Prim->AddParticle(dVector(0,0,0),dColour(0,0,0),dVector(0.1,0.1,0.1));
	}
	
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusBinding::build_locator()
{
	LocatorPrimitive *Prim = new LocatorPrimitive();
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusBinding::build_pixels(SCM s_w, SCM s_h)
{
	SCM_ASSERT(scm_is_number(s_w), s_w, SCM_ARG1, "build-pixels");
	SCM_ASSERT(scm_is_number(s_h), s_h, SCM_ARG1, "build-pixels");
	
	PixelPrimitive *Prim = new PixelPrimitive((int)scm_to_double(s_w), (int)scm_to_double(s_h));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusBinding::upload_pixels()
{	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
	if (Grabbed) 
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Grabbed);
		if (pp)
		{
			pp->Upload();
		    return SCM_UNSPECIFIED;
		}
	}
	
	cerr<<"upload-pixels can only be called while a pixelprimitive is grabbed"<<endl;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pixels2texture(SCM s_ob)
{		
	Primitive *Prim=Fluxus->GetRenderer()->GetPrimitive(Smob2Prim(s_ob));
	if (Prim) 
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Prim);
		if (pp)
		{
		    return scm_from_int(pp->GetTexture());
		}
	}
	
	cerr<<"pixels->texture can only be called on a pixelprimitive"<<endl;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::draw_instance(SCM s_ob)
{    	
    Fluxus->GetRenderer()->RenderPrimitive(Fluxus->GetRenderer()->GetPrimitive(Smob2Prim(s_ob)));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::draw_cube()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticCube);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::draw_plane()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticPlane);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::draw_sphere()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticSphere);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::draw_cylinder()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticCylinder);
    return SCM_UNSPECIFIED;
}


SCM FluxusBinding::key_pressed(SCM s_key)
{
	SCM_ASSERT(scm_is_string(s_key), s_key, SCM_ARG1, "key_pressed");
	char *key=scm_to_locale_string(s_key);
	bool pressed;
	if (m_KeySet.find(key[0])!=m_KeySet.end()) pressed=true;
	else pressed=false;
    free(key);
    return scm_from_bool(pressed);
}

SCM FluxusBinding::show_axis(SCM s_id)
{
	SCM_ASSERT(scm_is_number(s_id), s_id, SCM_ARG1, "show_axis");	
    Fluxus->GetRenderer()->ShowAxis(scm_to_double(s_id));
    Fluxus->ShowLocators(scm_to_double(s_id));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::show_fps(SCM s_id)
{
	SCM_ASSERT(scm_is_number(s_id), s_id, SCM_ARG1, "show_fps");	
    Fluxus->GetRenderer()->SetFPSDisplay(scm_to_double(s_id));
    return SCM_UNSPECIFIED;
}
SCM FluxusBinding::make_light(SCM cam)
{
	SCM_ASSERT(scm_is_number(cam), cam, SCM_ARG1, "make_light");
	Light *l=new Light;
	l->SetCameraLock((bool)scm_to_double(cam));
	return scm_from_int(Fluxus->GetRenderer()->AddLight(l));
}

SCM FluxusBinding::clear_lights()
{
	Fluxus->GetRenderer()->ClearLights();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::light_ambient(SCM id, SCM v)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light_ambient");	
	SCM_ASSERT(scm_is_generalized_vector(v), v, SCM_ARG2, "light_ambient");
	SCM_ASSERT(scm_c_generalized_vector_length(v)==3, v, SCM_ARG2, "light_ambient");
	float vec[3];
	flx_floats_from_scm(v,vec);
	Fluxus->GetRenderer()->GetLight(scm_to_int(id))->SetAmbient(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::light_diffuse(SCM id, SCM v)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light_diffuse");	
	SCM_ASSERT(scm_is_generalized_vector(v), v, SCM_ARG2, "light_diffuse");
	SCM_ASSERT(scm_c_generalized_vector_length(v)==3, v, SCM_ARG2, "light_diffuse");
	float vec[3];
	flx_floats_from_scm(v,vec);
	Fluxus->GetRenderer()->GetLight(scm_to_int(id))->SetDiffuse(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::light_specular(SCM id, SCM v)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light_specular");	
	SCM_ASSERT(scm_is_generalized_vector(v), v, SCM_ARG2, "light_specular");
	SCM_ASSERT(scm_c_generalized_vector_length(v)==3, v, SCM_ARG2, "light_specular");
	float vec[3];
	flx_floats_from_scm(v,vec);
	Fluxus->GetRenderer()->GetLight(scm_to_int(id))->SetSpecular(dColour(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::light_position(SCM id, SCM v)
{
	SCM_ASSERT(scm_is_number(id), id, SCM_ARG1, "light_position");	
	SCM_ASSERT(scm_is_generalized_vector(v), v, SCM_ARG2, "light_position");
	SCM_ASSERT(scm_c_generalized_vector_length(v)==3, v, SCM_ARG2, "light_position");	
	float vec[3];
	flx_floats_from_scm(v,vec);
	Fluxus->GetRenderer()->GetLight(scm_to_int(id))->SetPosition(dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::lock_camera(SCM s_ob)
{
	int ob=Smob2Prim(s_ob);	
    Fluxus->GetRenderer()->LockCamera(ob);
	scm_remember_upto_here_1(s_ob);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::camera_lag(SCM s_amount)
{
	SCM_ASSERT(scm_is_number(s_amount), s_amount, SCM_ARG1, "camera-lag");	
    Fluxus->GetRenderer()->SetCameraLag(scm_to_double(s_amount));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::destroy(SCM s_name)
{
	int name=0;
	name=Smob2Prim(s_name);	
	
	Primitive *p=Fluxus->GetRenderer()->GetPrimitive(name);
	if (p)
	{
    	if (p->IsPhysicalHint())
    	{
    		Fluxus->GetPhysics()->Free(name);
    	}
    	Fluxus->GetRenderer()->RemovePrimitive(name);
    }

	scm_remember_upto_here_1(s_name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::clear()
{
	Fluxus->GetRenderer()->Clear();
	Fluxus->GetPhysics()->Clear();
	if (FrameHook)
		scm_reset_hook_x(FrameHook);
	else {
		fprintf(stderr, "FrameHook == NULL during (clear)!\n");
	}
	
	GrabbedIDStack.clear();
	GrabbedID=-1;
	Fluxus->GetRenderer()->UnGrab();
	
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::grab(SCM s_id)
{
	int id = Smob2Prim(s_id);
	GrabbedIDStack.push_front(id);
	GrabbedID=id;
	Fluxus->GetRenderer()->Grab(GrabbedID);
	scm_remember_upto_here_1(s_id);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::ungrab()
{
	Fluxus->GetRenderer()->UnGrab();
	GrabbedID=-1;
	
	if (!GrabbedIDStack.empty())
	{
		GrabbedIDStack.pop_front();
		if (!GrabbedIDStack.empty())
		{
			GrabbedID=*GrabbedIDStack.begin();
			Fluxus->GetRenderer()->Grab(GrabbedID);
		}
	}
		
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::apply(SCM s_id)
{
	Fluxus->GetRenderer()->GetPrimitive(Smob2Prim(s_id))->ApplyTransform();
	scm_remember_upto_here_1(s_id);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::opacity(SCM s_opac)
{
    SCM_ASSERT(scm_is_number(s_opac), s_opac, SCM_ARG1, "opacity");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Opacity=scm_to_double(s_opac);
    else Fluxus->GetRenderer()->GetState()->Opacity=scm_to_double(s_opac);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::shinyness(SCM s_opac)
{
    SCM_ASSERT(scm_is_number(s_opac), s_opac, SCM_ARG1, "shinyness");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Shinyness=scm_to_double(s_opac);
    else Fluxus->GetRenderer()->GetState()->Shinyness=scm_to_double(s_opac);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::colour(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "colour");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "colour");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Colour=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Colour=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::wire_colour(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "wire-colour");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "wire-colour");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->WireColour=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->WireColour=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::specular(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "specular");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "specular");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Specular=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Specular=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::ambient(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "ambient");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "ambient");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Ambient=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Ambient=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::emissive(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "emissive");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "emissive");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Emissive=dColour(col[0],col[1],col[2]);
    else Fluxus->GetRenderer()->GetState()->Emissive=dColour(col[0],col[1],col[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::flux_identity()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.init();
    else Fluxus->GetRenderer()->GetState()->Transform.init();
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::translate(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "translate");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "translate");
	float trans[3];
	flx_floats_from_scm(s_vec,trans);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.translate(trans[0],trans[1],trans[2]);
    else Fluxus->GetRenderer()->GetState()->Transform.translate(trans[0],trans[1],trans[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::concat(SCM s_m)
{
    SCM_ASSERT(scm_is_generalized_vector(s_m), s_m, SCM_ARG1, "concat");
	SCM_ASSERT(scm_c_generalized_vector_length(s_m)==16, s_m, SCM_ARG1, "concat");
	dMatrix m;
	flx_floats_from_scm(s_m,m.arr());
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform*=m;
    else Fluxus->GetRenderer()->GetState()->Transform*=m;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::rotate(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "rotate");
	
	if (scm_c_generalized_vector_length(s_vec)==3)
	{
		// euler angles
		float rot[3];
		flx_floats_from_scm(s_vec,rot);
	    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
	    if (Grabbed)
	    {
	    	Grabbed->GetState()->Transform.rotxyz(rot[0],rot[1],rot[2]);
	    }
	    else
	    {
	    	Fluxus->GetRenderer()->GetState()->Transform.rotxyz(rot[0],rot[1],rot[2]);
	    }
	}
	else if (scm_c_generalized_vector_length(s_vec)==4)
	{
		// quaternion
		dQuat a;
		flx_floats_from_scm(s_vec,a.arr());
		dMatrix m=a.toMatrix();
	 	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
	    if (Grabbed)
	    {
	    	Grabbed->GetState()->Transform*=m;
	    }
	    else
	    {
	    	Fluxus->GetRenderer()->GetState()->Transform*=m;
	    }
	}
	else
	{
		cerr<<"rotate - wrong number of elements in vector"<<endl;
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::scale(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "scale");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "scale");
	float scale[3];
	flx_floats_from_scm(s_vec,scale);
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Transform.scale(scale[0],scale[1],scale[2]);
    else Fluxus->GetRenderer()->GetState()->Transform.scale(scale[0],scale[1],scale[2]);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::parent(SCM s_p)
{
    Fluxus->GetRenderer()->GetState()->Parent=Smob2Prim(s_p);
	scm_remember_upto_here_1(s_p);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::line_width(SCM s_p)
{
    SCM_ASSERT(scm_is_number(s_p), s_p, SCM_ARG1, "line_width");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->LineWidth=scm_to_double(s_p);
    else Fluxus->GetRenderer()->GetState()->LineWidth=scm_to_double(s_p);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::point_width(SCM s_p)
{
    SCM_ASSERT(scm_is_number(s_p), s_p, SCM_ARG1, "point_width");
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->PointWidth=scm_to_double(s_p);
    else Fluxus->GetRenderer()->GetState()->PointWidth=scm_to_double(s_p);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::blend_mode(SCM s_s, SCM s_d)
{
    SCM_ASSERT(scm_is_string(s_s), s_s, SCM_ARG1, "blend_mode");
    SCM_ASSERT(scm_is_string(s_d), s_d, SCM_ARG2, "blend_mode");
	
	char *s=scm_to_locale_string(s_s);	
	char *d=scm_to_locale_string(s_d);	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->SetBlendMode(s,d);
    else Fluxus->GetRenderer()->GetState()->SetBlendMode(s,d);
	free(s);
	free(d);
	
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_solid()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_SOLID;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_SOLID;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_wire()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_WIRE;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_WIRE;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_normal()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_NORMAL;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_NORMAL;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_points()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_POINTS;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_POINTS;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_anti_alias()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_AALIAS;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_AALIAS;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_unlit()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_UNLIT;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_UNLIT;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_vertcols()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_VERTCOLS;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_VERTCOLS;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_box()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_BOUND;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_BOUND;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_multitex()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_MULTITEX;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_MULTITEX;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_none()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints=0;
    else Fluxus->GetRenderer()->GetState()->Hints=0;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hint_origin()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) Grabbed->GetState()->Hints|=HINT_ORIGIN;
    else Fluxus->GetRenderer()->GetState()->Hints|=HINT_ORIGIN;
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::blur(SCM s_blur)
{
    SCM_ASSERT(scm_is_number(s_blur), s_blur, SCM_ARG1, "blur");
	double blur;
	blur=scm_to_double(s_blur);	
	if (!blur) Fluxus->GetRenderer()->SetMotionBlur(false);
    else Fluxus->GetRenderer()->SetMotionBlur(true, (float)blur);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::fog(SCM s_col, SCM s_d, SCM s_s, SCM s_e)
{
    SCM_ASSERT(scm_is_generalized_vector(s_col), s_col, SCM_ARG1, "fog");
	SCM_ASSERT(scm_c_generalized_vector_length(s_col)==3, s_col, SCM_ARG1, "fog");
    SCM_ASSERT(scm_is_number(s_d), s_d, SCM_ARG2, "fog");
    SCM_ASSERT(scm_is_number(s_s), s_s, SCM_ARG3, "fog");
    SCM_ASSERT(scm_is_number(s_e), s_e, SCM_ARG4, "fog");
	dColour c;
	flx_floats_from_scm(s_col,c.arr());
	Fluxus->GetRenderer()->SetFog(c,scm_to_double(s_d),scm_to_double(s_s),scm_to_double(s_e));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::feedback(SCM s_fb)
{
    SCM_ASSERT(scm_is_number(s_fb), s_fb, SCM_ARG1, "feedback");
	double fb;
	fb=scm_to_double(s_fb);	
	Fluxus->GetRenderer()->SetFeedBack(fb);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::feedback_transform(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "feedback_transform");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "feedback_transform");
	dMatrix m;
	flx_floats_from_scm(s_a,m.arr());
	Fluxus->GetRenderer()->SetFeedBackMat(m);
	return SCM_UNSPECIFIED;	
}

SCM FluxusBinding::push()
{
	if (GrabbedID!=-1)
	{
		cerr<<"error: can't (push) while an object is (grab)bed"<<endl;
		return SCM_UNSPECIFIED;
	}
	
    Fluxus->GetRenderer()->PushState();
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pop()
{
	if (GrabbedID!=-1)
	{
		cerr<<"error: can't (pop) while an object is (grab)bed"<<endl;
		return SCM_UNSPECIFIED;
	}

    Fluxus->GetRenderer()->PopState();
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::collisions(SCM s)
{
	SCM_ASSERT(scm_is_number(s), s, SCM_ARG1, "collisions");
	Fluxus->GetPhysics()->SetCollisions(scm_to_double(s));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::ground_plane(SCM s_ori, SCM s_off)
{
    SCM_ASSERT(scm_is_generalized_vector(s_ori), s_ori, SCM_ARG1, "ground_plane");
	SCM_ASSERT(scm_c_generalized_vector_length(s_ori)==3, s_ori, SCM_ARG1, "ground_plane");
    SCM_ASSERT(scm_is_number(s_off), s_off, SCM_ARG2, "ground_plane");
    float ori[3];
	flx_floats_from_scm(s_ori,ori);
	float off = (float) scm_to_double(s_off);
	Fluxus->GetPhysics()->GroundPlane(dVector(ori[0],ori[1],ori[2]),off);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::active_box(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::BOX);
	scm_remember_upto_here_1(s_name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::active_cylinder(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::CYLINDER);
	scm_remember_upto_here_1(s_name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::active_sphere(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakeActive(name,1.0f,Physics::SPHERE);
	scm_remember_upto_here_1(s_name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::passive_box(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::BOX);
	scm_remember_upto_here_1(s_name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::passive_cylinder(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::CYLINDER);
	scm_remember_upto_here_1(s_name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::passive_sphere(SCM s_name)
{
	int name=Smob2Prim(s_name);	
	Fluxus->GetPhysics()->MakePassive(name,1.0f,Physics::SPHERE);
	scm_remember_upto_here_1(s_name);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::surface_params(SCM s_slip1, SCM s_slip2, SCM s_softerp, SCM s_softcfm)
{
	SCM_ASSERT(scm_is_number(s_slip1),    s_slip1,    SCM_ARG1, "surface_params");
	SCM_ASSERT(scm_is_number(s_slip2),    s_slip2,    SCM_ARG2, "surface_params");
	SCM_ASSERT(scm_is_number(s_softerp),  s_softerp,  SCM_ARG3, "surface_params");
	SCM_ASSERT(scm_is_number(s_softcfm),  s_softcfm,  SCM_ARG4, "surface_params");
	Fluxus->GetPhysics()->SetGlobalSurfaceParams((float)scm_to_double(s_slip1),(float)scm_to_double(s_slip2),
		(float)scm_to_double(s_softerp),(float)scm_to_double(s_softcfm));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::build_balljoint(SCM s_ob1, SCM s_ob2, SCM s_anchor)
{
    SCM_ASSERT(scm_is_generalized_vector(s_anchor), s_anchor, SCM_ARG3, "build_balljoint");
	SCM_ASSERT(scm_c_generalized_vector_length(s_anchor)==3, s_anchor, SCM_ARG3, "build_balljoint");
    int name1=Smob2Prim(s_ob1);
	int name2=Smob2Prim(s_ob2);	
	float anchor[3];
	flx_floats_from_scm(s_anchor,anchor);
	scm_remember_upto_here_1(s_ob1);
	scm_remember_upto_here_1(s_ob2);
	return scm_from_int(Fluxus->GetPhysics()->CreateJointBall(name1, name2, dVector(anchor[0],anchor[1],anchor[2])));
}

SCM FluxusBinding::build_fixedjoint(SCM s_ob1)
{
    int name1=Smob2Prim(s_ob1);
	return scm_from_int(Fluxus->GetPhysics()->CreateJointFixed(name1));
}

SCM FluxusBinding::build_hingejoint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge)
{
    SCM_ASSERT(scm_is_generalized_vector(s_anchor), s_anchor, SCM_ARG3, "build_hingejoint");
 	SCM_ASSERT(scm_c_generalized_vector_length(s_anchor)==3, s_anchor, SCM_ARG3, "build_hingejoint");
    SCM_ASSERT(scm_is_generalized_vector(s_hinge),  s_hinge,  SCM_ARG4, "build_hingejoint");
	SCM_ASSERT(scm_c_generalized_vector_length(s_hinge)==3, s_hinge, SCM_ARG4, "build_hingejoint");

    int name1=Smob2Prim(s_ob1);
	int name2=Smob2Prim(s_ob2);
	
	float anchor[3];
	flx_floats_from_scm(s_anchor,anchor);
	
	dVector Hinge;
	float temp[3];
	flx_floats_from_scm(s_hinge,temp);
	Hinge.x=temp[0];
	Hinge.y=temp[1];
	Hinge.z=temp[2];

	scm_remember_upto_here_1(s_ob1);
	scm_remember_upto_here_1(s_ob2);

	return scm_from_int(Fluxus->GetPhysics()->CreateJointHinge(name1, name2, dVector(anchor[0],anchor[1],anchor[2]), Hinge));
}

SCM FluxusBinding::build_sliderjoint(SCM s_ob1, SCM s_ob2, SCM s_hinge)
{
    SCM_ASSERT(scm_is_generalized_vector(s_hinge),  s_hinge,  SCM_ARG3, "build_sliderjoint");
	SCM_ASSERT(scm_c_generalized_vector_length(s_hinge)==3, s_hinge, SCM_ARG3, "build_sliderjoint");

    int name1=Smob2Prim(s_ob1);
	int name2=Smob2Prim(s_ob2);
		
	dVector Hinge;
	float temp[3];
	flx_floats_from_scm(s_hinge,temp);
	Hinge.x=temp[0];
	Hinge.y=temp[1];
	Hinge.z=temp[2];

	scm_remember_upto_here_1(s_ob1);
	scm_remember_upto_here_1(s_ob2);

	return scm_from_int(Fluxus->GetPhysics()->CreateJointSlider(name1, name2, Hinge));
}

SCM FluxusBinding::build_hinge2joint(SCM s_ob1, SCM s_ob2, SCM s_anchor, SCM s_hinge1, SCM s_hinge2)
{
    SCM_ASSERT(scm_is_generalized_vector(s_anchor), s_anchor, SCM_ARG3, "build_hinge2joint");
    SCM_ASSERT(scm_c_generalized_vector_length(s_anchor)==3, s_anchor, SCM_ARG3, "build_hinge2joint");
	SCM_ASSERT(scm_is_generalized_vector(s_hinge1), s_hinge1, SCM_ARG4, "build_hinge2joint");
    SCM_ASSERT(scm_c_generalized_vector_length(s_hinge1)==3, s_hinge1, SCM_ARG4, "build_hinge2joint");
	SCM_ASSERT(scm_is_generalized_vector(s_hinge2), s_hinge2, SCM_ARG5, "build_hinge2joint");
	SCM_ASSERT(scm_c_generalized_vector_length(s_hinge2)==3, s_hinge2, SCM_ARG5, "build_hinge2joint");

    int name1=Smob2Prim(s_ob1);
	int name2=Smob2Prim(s_ob2);
	
	float anchor[3];
	flx_floats_from_scm(s_anchor,anchor);
	
	dVector Hinge[2];
	float temp[3];
	flx_floats_from_scm(s_hinge1,temp);
	Hinge[0].x=temp[0];
	Hinge[0].y=temp[1];
	Hinge[0].z=temp[2];
	
	flx_floats_from_scm(s_hinge2,temp);
	Hinge[1].x=temp[0];
	Hinge[1].y=temp[1];
	Hinge[1].z=temp[2];
	
	scm_remember_upto_here_1(s_ob1);
	scm_remember_upto_here_1(s_ob2);

	return scm_from_int(Fluxus->GetPhysics()->CreateJointHinge2(name1, name2, dVector(anchor[0],anchor[1],anchor[2]), Hinge));
}

SCM FluxusBinding::build_amotorjoint(SCM s_ob1, SCM s_ob2, SCM s_axis)
{
    SCM_ASSERT(scm_is_generalized_vector(s_axis),   s_axis,   SCM_ARG3, "build_amotorjoint");
	SCM_ASSERT(scm_c_generalized_vector_length(s_axis)==3, s_axis, SCM_ARG3, "build_amotorjoint");
    int name1=Smob2Prim(s_ob1);
	int name2=Smob2Prim(s_ob2);	
	float axis[3];
	flx_floats_from_scm(s_axis,axis);
	scm_remember_upto_here_1(s_ob1);
	scm_remember_upto_here_1(s_ob2);
	return scm_from_int(Fluxus->GetPhysics()->CreateJointAMotor(name1, name2, dVector(axis[0],axis[1],axis[2])));
}

SCM FluxusBinding::joint_param(SCM s_joint, SCM s_param, SCM s_value)
{
    SCM_ASSERT(scm_is_number(s_joint), s_joint, SCM_ARG1, "joint_param");
    SCM_ASSERT(scm_is_string(s_param), s_param, SCM_ARG1, "joint_param");
    SCM_ASSERT(scm_is_number(s_value), s_value, SCM_ARG2, "joint_param");
    int joint=0;
	joint=scm_to_int(s_joint);
	char *param=0;
	param=scm_to_locale_string(s_param);	
    double v = scm_to_double(s_value);
	Fluxus->GetPhysics()->SetJointParam(joint,param,v);
	free(param);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::joint_angle(SCM s_joint, SCM s_vel, SCM s_angle)
{
    SCM_ASSERT(scm_is_number(s_joint), s_joint, SCM_ARG1, "joint_angle");
    SCM_ASSERT(scm_is_number(s_vel),   s_vel,   SCM_ARG1, "joint_angle");
    SCM_ASSERT(scm_is_number(s_angle), s_angle, SCM_ARG2, "joint_angle");
	Fluxus->GetPhysics()->SetJointAngle(scm_to_int(s_joint),scm_to_double(s_vel),scm_to_double(s_angle));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::set_max_physical(SCM s_value)
{
    SCM_ASSERT(scm_is_number(s_value), s_value, SCM_ARG2, "set_max_physical");
    Fluxus->GetPhysics()->SetMaxObjectCount(scm_to_int(s_value));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::set_mass(SCM s_obj, SCM s_mass)
{
    SCM_ASSERT(scm_is_number(s_mass), s_mass, SCM_ARG2, "set_mass");
    int obj=Smob2Prim(s_obj);
    float mass=scm_to_double(s_mass);
	Fluxus->GetPhysics()->SetMass(obj,mass);
	scm_remember_upto_here_1(s_obj);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::gravity(SCM s_vec)
{
	SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "gravity");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "gravity");
	float vec[3];
	flx_floats_from_scm(s_vec,vec);
	Fluxus->GetPhysics()->SetGravity(dVector(vec[0],vec[1],vec[2]));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::kick(SCM s_obj, SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG2, "kick");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG2, "kick");
    int obj=Smob2Prim(s_obj);
    float vec[3];
    flx_floats_from_scm(s_vec,vec);
	Fluxus->GetPhysics()->Kick(obj,dVector(vec[0],vec[1],vec[2]));
	scm_remember_upto_here_1(s_obj);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::twist(SCM s_obj, SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG2, "twist");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG2, "twist");
    int obj=Smob2Prim(s_obj);
    float vec[3];
    flx_floats_from_scm(s_vec,vec);
	Fluxus->GetPhysics()->Twist(obj,dVector(vec[0],vec[1],vec[2]));
	scm_remember_upto_here_1(s_obj);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::srandom()
{
	return scm_from_double(RandFloat());
}

SCM FluxusBinding::has_collided(SCM s_id)
{
	return scm_from_bool(Fluxus->GetPhysics()->HasCollided(Smob2Prim(s_id)));
}

SCM FluxusBinding::start_audio(SCM s_dev, SCM s_bs, SCM s_sr)
{
	SCM_ASSERT(scm_is_string(s_dev), s_dev, SCM_ARG1, "start-audio");
	SCM_ASSERT(scm_is_number(s_bs), s_bs, SCM_ARG2, "start-audio");
 	SCM_ASSERT(scm_is_number(s_sr), s_sr, SCM_ARG3, "start-audio");
	
	if (Audio==NULL)
	{
		char *name=scm_to_locale_string(s_dev);		
		Audio = new AudioCollector(name,(unsigned int)scm_to_int(s_bs),scm_to_int(s_sr));
		Fluxus->SetAudio(Audio);
		free(name);
	}
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::get_harmonic(SCM s_harm)
{
	SCM_ASSERT(scm_is_number(s_harm), s_harm, SCM_ARG1, "get_harmonic");	
	if (Audio!=NULL)
	{	
    	return scm_from_double(Audio->GetHarmonic(scm_to_int(s_harm)));
	}
	return scm_from_double(0);
}

SCM FluxusBinding::load_texture(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "load-texture");
	char *name=scm_to_locale_string(s_name);	
	int id=Fluxus->GetRenderer()->LoadTexture(name);
    free(name);
    return scm_from_int(id);
}

SCM FluxusBinding::force_load_texture(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "force-load-texture");
	char *name=scm_to_locale_string(s_name);	
	
	int id=Fluxus->GetRenderer()->LoadTexture(name,true);

    free(name);
    return scm_from_int(id);
}

SCM FluxusBinding::texture(SCM s_id)
{
	SCM_ASSERT(scm_is_number(s_id), s_id, SCM_ARG1, "texture");	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
	if (Grabbed) Grabbed->GetState()->Textures[0]=(int)scm_to_int(s_id);
    else Fluxus->GetRenderer()->GetState()->Textures[0]=(int)scm_to_int(s_id);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::multitexture(SCM s_t, SCM s_id)
{
	SCM_ASSERT(scm_is_number(s_t), s_t, SCM_ARG1, "multitexture");	
	SCM_ASSERT(scm_is_number(s_id), s_id, SCM_ARG2, "multitexture");	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
	if (Grabbed) Grabbed->GetState()->Textures[scm_to_int(s_t)]=scm_to_int(s_id);
    else Fluxus->GetRenderer()->GetState()->Textures[scm_to_int(s_t)]=scm_to_int(s_id);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::frame_hook()
{
	return FrameHook;
}

SCM FluxusBinding::frustum(SCM s_u, SCM s_d, SCM s_l, SCM s_r)
{
	SCM_ASSERT(scm_is_number(s_u), s_u, SCM_ARG1, "frustum");
	SCM_ASSERT(scm_is_number(s_d), s_d, SCM_ARG2, "frustum");
	SCM_ASSERT(scm_is_number(s_l), s_l, SCM_ARG3, "frustum");
	SCM_ASSERT(scm_is_number(s_r), s_r, SCM_ARG4, "frustum");
	
	Fluxus->GetRenderer()->SetFrustum(scm_to_double(s_u),scm_to_double(s_d),
									  scm_to_double(s_l),scm_to_double(s_r));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::clip(SCM s_f, SCM s_b)
{
	SCM_ASSERT(scm_is_number(s_f), s_f, SCM_ARG1, "clip");
	SCM_ASSERT(scm_is_number(s_b), s_b, SCM_ARG2, "clip");
	
	Fluxus->GetRenderer()->SetClip(scm_to_double(s_f),scm_to_double(s_b));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::ortho()
{
	//Fluxus->ortho();
	Fluxus->GetRenderer()->SetOrtho(true);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::persp()
{
	Fluxus->GetRenderer()->SetOrtho(false);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::time()
{
	return scm_from_double(Fluxus->GetRenderer()->GetTime());
}

SCM FluxusBinding::delta()
{
	return scm_from_double(Fluxus->GetRenderer()->GetDelta());
}

SCM FluxusBinding::reset_camera()
{
	Fluxus->ResetCamera();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::print_scene_graph()
{
	Fluxus->GetRenderer()->PrintSceneGraph();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::edit(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "edit");
    SCM path = scm_sys_search_load_path(s_name);
    if (scm_is_string(path)) 
	{
        Fluxus->LoadScript(scm_to_locale_string(path));
    } 
	else 
	{
        Fluxus->LoadScript(scm_to_locale_string(s_name));
    }
        
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::save_name(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "save_name");	
	char *name=scm_to_locale_string(s_name);
	Fluxus->SetSaveName(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::source(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "source");	
	char *name=scm_to_locale_string(s_name);
	Fluxus->SourceScript(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::gain(SCM s_gain)
{
	SCM_ASSERT(scm_is_number(s_gain), s_gain, SCM_ARG1, "gain");	
	if (Audio!=NULL)
	{	
		Audio->SetGain(scm_to_double(s_gain));
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::smoothing_bias(SCM s_gain)
{
	SCM_ASSERT(scm_is_number(s_gain), s_gain, SCM_ARG1, "smoothing-bias");	
	if (Audio!=NULL)
	{	
		Audio->SetSmoothingBias(scm_to_double(s_gain));
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::backfacecull(SCM s)
{
	SCM_ASSERT(scm_is_number(s), s, SCM_ARG1, "backfacecull");
	Fluxus->GetRenderer()->SetBackFaceCull(scm_to_double(s));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::desiredfps(SCM s)
{
	SCM_ASSERT(scm_is_number(s), s, SCM_ARG1, "desiredfps");
	Fluxus->GetRenderer()->SetDesiredFPS(scm_to_double(s));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::clear_colour(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "clear_colour");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "clear_colour");
	float col[3];
	flx_floats_from_scm(s_vec,col);
    Fluxus->GetRenderer()->SetBGColour(dColour(col[0],col[1],col[2]));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::clear_frame(SCM s_gain)
{
	SCM_ASSERT(scm_is_number(s_gain), s_gain, SCM_ARG1, "clear_frame");	
	Fluxus->GetRenderer()->SetClearFrame(scm_to_double(s_gain));
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::turtle_prim(SCM type)
{
	SCM_ASSERT(scm_is_number(type), type, SCM_ARG1, "turtle_prim");
	turtle.Prim(scm_to_int(type));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::turtle_vert()
{
	turtle.Vert();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::turtle_build()
{
	return Prim2Smob(turtle.Build(Fluxus->GetRenderer()));
}

SCM FluxusBinding::turtle_move(SCM dist)
{
	SCM_ASSERT(scm_is_number(dist), dist, SCM_ARG1, "turtle_move");
	turtle.Move(scm_to_double(dist));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::turtle_push()
{
	turtle.Push();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::turtle_pop()
{
	turtle.Pop();
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::turtle_turn(SCM s_vec)
{
    SCM_ASSERT(scm_is_generalized_vector(s_vec), s_vec, SCM_ARG1, "turtle_turn");
	SCM_ASSERT(scm_c_generalized_vector_length(s_vec)==3, s_vec, SCM_ARG1, "turtle_turn");
	float rot[3];
	flx_floats_from_scm(s_vec,rot);
	turtle.Turn(dVector(rot[0],rot[1],rot[2]));
	return SCM_UNSPECIFIED;	
}

SCM FluxusBinding::turtle_reset()
{
	turtle.Reset();
	return SCM_UNSPECIFIED;	
}

SCM FluxusBinding::start_framedump(SCM s_name, SCM s_type)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "start-framedump");	
	SCM_ASSERT(scm_is_string(s_type), s_type, SCM_ARG2, "start-framedump");	
	char *name=scm_to_locale_string(s_name);
	char *type=scm_to_locale_string(s_type);
	Fluxus->StartDumpFrames(name,type);
	free(name);
	free(type);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::end_framedump()
{
	Fluxus->EndDumpFrames();
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::process(SCM s_wavname)
{
	SCM_ASSERT(scm_is_string(s_wavname), s_wavname, SCM_ARG1, "process");	
	char *wavname=scm_to_locale_string(s_wavname);
	if (Audio!=NULL)
	{	
		Audio->Process(wavname);
	}
	free(wavname);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::osc_source(SCM s_port)
{
	SCM_ASSERT(scm_is_string(s_port), s_port, SCM_ARG1, "osc_source");	
	char *port=scm_to_locale_string(s_port);
	Fluxus->StartOSC(port);
	free(port);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::osc_msg(SCM s_token)
{
	SCM_ASSERT(scm_is_string(s_token), s_token, SCM_ARG1, "msg_osc");
 	char *name=scm_to_locale_string(s_token);
	bool ret=Fluxus->MsgOSC(name);
	free(name);
	return scm_from_bool(ret);	
}

SCM FluxusBinding::osc(SCM s_index)
{
    SCM_ASSERT(scm_is_number(s_index), s_index, SCM_ARG2, "osc");
 
	unsigned int index=(unsigned int)scm_to_double(s_index);
	char type = Fluxus->TypeFromOSC(index);
	SCM ret;
	if (type=='f') ret=scm_from_double(Fluxus->NumberFromOSC(index));
	else if (type=='i') ret=scm_from_int((int)Fluxus->NumberFromOSC(index));
	else if (type=='s') 
	{
		string value=Fluxus->StringFromOSC(index);
		ret=scm_from_locale_stringn(value.c_str(),value.size());	
	}
	else ret=SCM_UNSPECIFIED;
	return ret;
}

SCM FluxusBinding::osc_destination(SCM s_port)
{
	SCM_ASSERT(scm_is_string(s_port), s_port, SCM_ARG1, "osc_source");	
	char *port=scm_to_locale_string(s_port);
	Fluxus->StartOSCClient(port);
	free(port);
    return SCM_UNSPECIFIED;
}


SCM FluxusBinding::osc_peek()
{
	string value=Fluxus->GetLastMsg();
	return scm_from_locale_stringn(value.c_str(),value.size());	
}

SCM FluxusBinding::osc_send(SCM s_msg, SCM s_types, SCM s_arglist)
{
	SCM_ASSERT(scm_is_string(s_msg), s_msg, SCM_ARG1, "osc-send");	
	SCM_ASSERT(scm_is_string(s_types), s_types, SCM_ARG2, "osc-send");	
	// todo: fix this...
	//SCM_ASSERT(SCM_LISTP(s_arglist), s_arglist, SCM_ARG2, "osc-send");
	char *msg=scm_to_locale_string(s_msg);
	char *types=scm_to_locale_string(s_types);
	
	// vectors seem easier to handle than lists with this api
	SCM argvec = scm_vector(s_arglist);
	
	vector<OSCData*> oscargs;
	for (unsigned int n=0; n<scm_c_generalized_vector_length(argvec); n++)
	{
		SCM arg=scm_vector_ref(argvec, scm_from_int(n));

		if (scm_is_number(arg))// ||  scm_is_true(scm_exact_p(arg)) || scm_is_true(scm_inexact_p(arg)))
		{
			if (n<strlen(types))
			{
				if (types[n]=='f') oscargs.push_back(new OSCFloat(scm_to_double(arg)));
				else if (types[n]=='i') oscargs.push_back(new OSCInt(scm_to_int(arg)));
			}
		}
		else if (scm_is_string(arg))
		{
			char *argstring=scm_to_locale_string(arg);
			oscargs.push_back(new OSCString(argstring));
			free(argstring);
		}
		else
		{
			cerr<<"osc-send has found an argument type it can't send, numbers and strings only"<<endl;
			free(msg);
			free(types);
    		return SCM_UNSPECIFIED;
		}
	}

	Fluxus->SendOSC(msg,oscargs);
	free(msg);
	free(types);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pdata_get(SCM s_t, SCM s_i)
{
	SCM_ASSERT(scm_is_string(s_t), s_t, SCM_ARG1, "pdata-get");
    SCM_ASSERT(scm_is_number(s_i), s_i, SCM_ARG2, "pdata-get");
	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		char *name=scm_to_locale_string(s_t);
		unsigned int index=scm_to_int(s_i);
		unsigned int size;
		char type;
		SCM ret=flx_floats_to_scm(dVector().arr(),3);
		
		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (index<size)
			{
				if (type=='v')	
				{
					ret=flx_floats_to_scm(Grabbed->GetData<dVector>(name,index).arr(),3); 
				}
				else if (type=='c')	
				{
					ret=flx_floats_to_scm(Grabbed->GetData<dColour>(name,index).arr(),4); 
				}
				else if (type=='m')	
				{
					ret=flx_floats_to_scm(Grabbed->GetData<dMatrix>(name,index).arr(),16); 
				}
			}
		}
		free(name); 
		return ret;
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pdata_set(SCM s_t, SCM s_i, SCM s_v)
{
    SCM_ASSERT(scm_is_string(s_t), s_t, SCM_ARG1, "pdata-set");
    SCM_ASSERT(scm_is_number(s_i), s_i, SCM_ARG2, "pdata-set");
	//SCM_ASSERT(scm_is_generalized_vector(s_v), s_v, SCM_ARG3, "pdata-set");	
	//SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3, s_v, SCM_ARG3, "pdata-set");
	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		size_t ssize=0;
		char *name=scm_to_locale_string(s_t);
		unsigned int index=scm_to_int(s_i);
		unsigned int size;
		char type;
		
		if (Grabbed->GetDataInfo(name,type,size))
		{
			if (index<size)
			{
				if (type=='f')	
				{
					Grabbed->SetData<float>(name,index,scm_to_double(s_v));
				}
				else if (type=='v')	
				{
					dVector v;
					flx_floats_from_scm(s_v,v.arr());
					Grabbed->SetData<dVector>(name,index,v);
				}
				else if (type=='c')	
				{
					dColour c;
					flx_floats_from_scm(s_v,c.arr());
					Grabbed->SetData<dColour>(name,index,c);
				}
				else if (type=='m')	
				{
					dMatrix m;
					flx_floats_from_scm(s_v,m.arr());
					Grabbed->SetData<dMatrix>(name,index,m);
				}
			}
		}
		free(name); 
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pdata_add(SCM s_name, SCM s_type)
{
    SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "pdata-add");
    SCM_ASSERT(scm_is_string(s_type), s_type, SCM_ARG2, "pdata-add");
	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		char *names=scm_to_locale_string(s_name);
		char *types=scm_to_locale_string(s_type);
		char type=0;
		unsigned int size=0;
		
		PData *ptr=NULL;
		Grabbed->GetDataInfo("p", type, size);
		
		switch (types[0])
		{
			case 'v': ptr = new TypedPData<dVector>; ((TypedPData<dVector>*)ptr)->m_Data.resize(size); break;
			case 'c': ptr = new TypedPData<dColour>; ((TypedPData<dColour>*)ptr)->m_Data.resize(size); break;
			case 'f': ptr = new TypedPData<float>; ((TypedPData<float>*)ptr)->m_Data.resize(size); break;
			case 'm': ptr = new TypedPData<dMatrix>; ((TypedPData<dMatrix>*)ptr)->m_Data.resize(size); break;
			default : cerr<<"pdata-new: unknown type "<<types[0]<<endl; break;
		}
		
		if (ptr)
		{
			Grabbed->AddData(names,ptr);
		}
		
		free(names);
		free(types);
	}
	
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pdata_op(SCM s_op, SCM s_pd, SCM s_oper)
{
    SCM_ASSERT(scm_is_string(s_op), s_op, SCM_ARG1, "pdata-op");
    SCM_ASSERT(scm_is_string(s_pd), s_pd, SCM_ARG2, "pdata-op");
	PData *ret=NULL;
	
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		char *op=scm_to_locale_string(s_op);
		char *pd=scm_to_locale_string(s_pd);
		
		// find out what the inputs are, and call the corresponding function
		if (scm_is_string(s_oper))
		{
			char *operand=scm_to_locale_string(s_oper);
			
			PData* pd2 = Grabbed->GetDataRaw(operand);
			
			TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(pd2);	
			if (data) ret = Grabbed->DataOp(op, pd, data);
			else
			{
				TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(pd2);
				if (data) ret = Grabbed->DataOp(op, pd, data);
				else 
				{
					TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(pd2);
					if (data) ret = Grabbed->DataOp(op, pd, data);
					else 
					{
						TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(pd2);
						if (data) ret = Grabbed->DataOp(op, pd, data);
					}
				}
			}
			free(operand);
		}
		else if (scm_is_number(s_oper))
		{
			ret = Grabbed->DataOp(op, pd, (float)scm_to_double(s_oper));
		}
		else if (scm_is_generalized_vector(s_oper))
		{
			switch (scm_c_generalized_vector_length(s_oper))
			{
				case 3:
				{
					dVector v;
					flx_floats_from_scm(s_oper,v.arr());
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;
				case 4:
				{
					dColour v;
					flx_floats_from_scm(s_oper,v.arr());
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;
				case 16:
				{
					dMatrix v;
					flx_floats_from_scm(s_oper,v.arr());
					ret = Grabbed->DataOp(op, pd, v);
				}
				break;	
			}
		}
		
		free(op);
		free(pd);
	}
		
	// convert the return data
	if (ret!=NULL)
	{
		TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(ret);	
		if (data) 
		{
			dVector r = data->m_Data[0];
			delete ret;
			return flx_floats_to_scm(r.arr(),3);
		}
		else
		{
			TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(ret);
			if (data) 
			{
				dColour r = data->m_Data[0];
				delete ret;
				return flx_floats_to_scm(r.arr(),4);
			}
			else 
			{
				TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(ret);
				if (data) 
				{		
					float r = data->m_Data[0];
					delete ret;
					return scm_from_double(r);
				}
				else 
				{
					TypedPData<dMatrix> *data = dynamic_cast<TypedPData<dMatrix>*>(ret);
					if (data) 
					{
						dMatrix r = data->m_Data[0];
						delete ret;
						return flx_floats_to_scm(r.arr(),16);
					}
				}
			}
		}
	}
	
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pdata_copy(SCM s_s, SCM s_d)
{
    SCM_ASSERT(scm_is_string(s_s), s_s, SCM_ARG1, "pdata-copy");
    SCM_ASSERT(scm_is_string(s_d), s_d, SCM_ARG2, "pdata-copy");
	
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		char *source=scm_to_locale_string(s_s);
		char *dest=scm_to_locale_string(s_d);
		Grabbed->CopyData(source,dest);
		free(source);
		free(dest);
	}
	
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::pdata_size()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) 
	{
		return scm_from_int(Grabbed->Size());
	}
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::finalise()
{
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::recalc_normals(SCM s_b)
{
    SCM_ASSERT(scm_is_number(s_b), s_b, SCM_ARG1, "recalc-normals");
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) Grabbed->RecalculateNormals(scm_to_double(s_b));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::hide(SCM s_b)
{
    SCM_ASSERT(scm_is_number(s_b), s_b, SCM_ARG1, "hide");
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) Grabbed->Hide(scm_to_double(s_b));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::selectable(SCM s_b)
{
    SCM_ASSERT(scm_is_number(s_b), s_b, SCM_ARG1, "selectable");
	Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();    
	if (Grabbed) Grabbed->Selectable(scm_to_double(s_b));
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::get_transform()
{
    Primitive *Grabbed=Fluxus->GetRenderer()->Grabbed();
    if (Grabbed) 
	{
		return flx_floats_to_scm(Fluxus->GetRenderer()->GetGlobalTransform(GrabbedID).arr(),16);
	}
	return flx_floats_to_scm(Fluxus->GetRenderer()->GetState()->Transform.arr(),16);
}

SCM FluxusBinding::get_camera_transform()
{
	return flx_floats_to_scm(Fluxus->GetRenderer()->GetCamera()->inverse().arr(),16);
}

SCM FluxusBinding::set_camera_transform(SCM s_m)
{
	SCM_ASSERT(scm_is_generalized_vector(s_m),  s_m,  SCM_ARG1, "set-camera-transform");
	SCM_ASSERT(scm_c_generalized_vector_length(s_m)==16,  s_m,  SCM_ARG1, "set-camera-transform");
	dMatrix m;
	flx_floats_from_scm(s_m,m.arr());
	(*Fluxus->GetRenderer()->GetCamera())=m.inverse();
	Fluxus->SetInteractiveCamera(false);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::get_projection_transform()
{
	return flx_floats_to_scm(Fluxus->GetRenderer()->GetProjection().arr(),16);
}

SCM FluxusBinding::get_screen_size()
{
	float res[2];
	int x=0,y=0;
	Fluxus->GetRenderer()->GetResolution(x,y);
	res[0]=x; res[1]=y;
	return flx_floats_to_scm(res,2);
}

SCM FluxusBinding::set_screen_size(SCM s_size)
{
	SCM_ASSERT(scm_is_generalized_vector(s_size), s_size, SCM_ARG1, "set-screen-size");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_size)==2, s_size, SCM_ARG3, "set-screen-size");
	float v[2];
	flx_floats_from_scm(s_size,v);
	// hmmm, seems a bit wrong, but hey...
	glutReshapeWindow((int)v[0],(int)v[1]);
	return SCM_UNSPECIFIED;
}

///////////////////////////////////////////////////////////////////////////////////////

SCM FluxusBinding::vmul(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vmul");
	SCM_ASSERT(scm_is_number(s_b),  s_b,  SCM_ARG2, "vmul");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vmul");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	return flx_floats_to_scm((a*scm_to_double(s_b)).arr(),3);
}

SCM FluxusBinding::vadd(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vadd");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vadd");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "vadd");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_b,  SCM_ARG2, "vadd");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a+b).arr(),3);
}


SCM FluxusBinding::vsub(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vsub");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "vsub");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vsub");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_b,  SCM_ARG2, "vsub");	
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a-b).arr(),3);
}

SCM FluxusBinding::vdiv(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vdiv");
	SCM_ASSERT(scm_is_number(s_b),  s_b,  SCM_ARG2, "vdiv");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vdiv");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	return flx_floats_to_scm((a/scm_to_double(s_b)).arr(),3);
}

SCM FluxusBinding::vtransform(SCM s_v, SCM s_m)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "vtransform");
	SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3,  s_v,  SCM_ARG1, "vtransform");
	SCM_ASSERT(scm_is_generalized_vector(s_m),  s_m,  SCM_ARG2, "vtransform");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_m)==16,  s_m,  SCM_ARG2, "vtransform");
	dVector v;
	flx_floats_from_scm(s_v,v.arr());
	dMatrix m;
	flx_floats_from_scm(s_m,m.arr());
	return flx_floats_to_scm((m.transform(v)).arr(),3);
}

SCM FluxusBinding::vtransform_rot(SCM s_v, SCM s_m)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "vtransform-rot");
	SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3,  s_v,  SCM_ARG1, "vtransform-rot");
	SCM_ASSERT(scm_is_generalized_vector(s_m),  s_m,  SCM_ARG2, "vtransform-rot");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_m)==16,  s_m,  SCM_ARG2, "vtransform-rot");
	dVector v;
	flx_floats_from_scm(s_v,v.arr());
	dMatrix m;
	flx_floats_from_scm(s_m,m.arr());
	return flx_floats_to_scm((m.transform_no_trans(v)).arr(),3);
}

SCM FluxusBinding::vnormalise(SCM s_v)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "vnormalise");
	SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3,  s_v,  SCM_ARG1, "vnormalise");
	dVector v;
	flx_floats_from_scm(s_v,v.arr());
	v.normalise();
	return flx_floats_to_scm(v.arr(),3);
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::vdot(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vdot");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vdot");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "vdot");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_a,  SCM_ARG2, "vdot");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	return scm_from_double(a.dot(b));
}

SCM FluxusBinding::vmag(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vmag");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vmag");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	return scm_from_double(a.mag());
}

SCM FluxusBinding::vdist(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vdist");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vdist");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "vdist");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_a,  SCM_ARG2, "vdist");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	return scm_from_double(a.dist(b));
}

SCM FluxusBinding::vcross(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "vcross");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "vcross");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "vcross");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_b,  SCM_ARG2, "vcross");	
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a.cross(b)).arr(),3);
}

SCM FluxusBinding::mmul(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "mmul");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "mmul");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==16,  s_b,  SCM_ARG2, "mmul");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "mmul");		
	dMatrix a;
	flx_floats_from_scm(s_a,a.arr());
	dMatrix b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a*b).arr(),16);
}

SCM FluxusBinding::madd(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "madd");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "madd");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==16,  s_b,  SCM_ARG2, "madd");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "madd");		
	dMatrix a;
	flx_floats_from_scm(s_a,a.arr());
	dMatrix b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a+b).arr(),16);
}

SCM FluxusBinding::msub(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "msub");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "msub");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==16,  s_b,  SCM_ARG2, "msub");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "msub");		
	dMatrix a;
	flx_floats_from_scm(s_a,a.arr());
	dMatrix b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a-b).arr(),16);
}

SCM FluxusBinding::mdiv(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "mdiv");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "mdiv");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==16,  s_b,  SCM_ARG2, "mdiv");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "mdiv");
	dMatrix a;
	flx_floats_from_scm(s_a,a.arr());
	dMatrix b;
	flx_floats_from_scm(s_b,b.arr());
	return flx_floats_to_scm((a/b).arr(),16);
}

SCM FluxusBinding::mident()
{
	dMatrix m;
	return flx_floats_to_scm(m.arr(),16);	
}

SCM FluxusBinding::mtranslate(SCM s_v)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "mtranslate");
	SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3,  s_v,  SCM_ARG1, "mtranslate");
	dVector a;
	flx_floats_from_scm(s_v,a.arr());
	dMatrix m;
	m.translate(a.x,a.y,a.z);
	return flx_floats_to_scm(m.arr(),16);	
}

SCM FluxusBinding::mrotate(SCM s_v)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "mrotate");	
	
	if (scm_c_generalized_vector_length(s_v)==3)
	{
		// euler angles
		dVector a;
		flx_floats_from_scm(s_v,a.arr());
		dMatrix m;
		m.rotxyz(a.x,a.y,a.z);
		return flx_floats_to_scm(m.arr(),16);	
	}
	else if (scm_c_generalized_vector_length(s_v)==4)
	{
		// quaternion
		dQuat a;
		flx_floats_from_scm(s_v,a.arr());
		dMatrix m=a.toMatrix();
		return flx_floats_to_scm(m.arr(),16);	
	}
	
	cerr<<"mrotate - wrong number of elements in vector"<<endl;
	return SCM_UNSPECIFIED;
}

SCM FluxusBinding::mscale(SCM s_v)
{
	SCM_ASSERT(scm_is_generalized_vector(s_v),  s_v,  SCM_ARG1, "mscale");
	SCM_ASSERT(scm_c_generalized_vector_length(s_v)==3,  s_v,  SCM_ARG1, "mscale");
	dVector a;
	flx_floats_from_scm(s_v,a.arr());
	dMatrix m;
	m.scale(a.x,a.y,a.z);
	return flx_floats_to_scm(m.arr(),16);	
}

SCM FluxusBinding::mtranspose(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "mtranspose");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "mtranspose");
	dMatrix m;
	flx_floats_from_scm(s_a,m.arr());
	m.transpose();
	return flx_floats_to_scm(m.arr(),16);	
}

SCM FluxusBinding::minverse(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "minverse");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==16,  s_a,  SCM_ARG1, "minverse");
	dMatrix m;
	flx_floats_from_scm(s_a,m.arr());
	m=m.inverse();
	return flx_floats_to_scm(m.arr(),16);	
}

SCM FluxusBinding::maim(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a),  s_a,  SCM_ARG1, "maim");
	SCM_ASSERT(scm_is_generalized_vector(s_b),  s_b,  SCM_ARG2, "maim");	
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==3,  s_a,  SCM_ARG1, "maim");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==3,  s_b,  SCM_ARG2, "maim");
	dVector a;
	flx_floats_from_scm(s_a,a.arr());
	dVector b;
	flx_floats_from_scm(s_b,b.arr());
	dMatrix m;
	m.aim(a,b);
	return flx_floats_to_scm(m.arr(),16);
}
	
SCM FluxusBinding::qaxisangle(SCM s_axis, SCM s_angle)
{
	SCM_ASSERT(scm_is_generalized_vector(s_axis), s_axis, SCM_ARG1, "qaxisangle");
	SCM_ASSERT(scm_c_generalized_vector_length(s_axis)==3, s_axis, SCM_ARG1, "qaxisangle");
	SCM_ASSERT(scm_is_number(s_angle),  s_angle,  SCM_ARG1, "qaxisangle");
	
	dVector ax;
	flx_floats_from_scm(s_axis,ax.arr());
	dQuat q;
	q.setaxisangle(ax,scm_to_double(s_angle));
	return flx_floats_to_scm(q.arr(),4);
}

SCM FluxusBinding::qmul(SCM s_a, SCM s_b)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a), s_a, SCM_ARG1, "qmul");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==4, s_a, SCM_ARG1, "qmul");
	SCM_ASSERT(scm_is_generalized_vector(s_b), s_b, SCM_ARG2, "qmul");
	SCM_ASSERT(scm_c_generalized_vector_length(s_b)==4, s_b, SCM_ARG2, "qmul");
	
	dQuat a;
	flx_floats_from_scm(s_a,a.arr());
	dQuat b;
	flx_floats_from_scm(s_b,b.arr());
	dQuat c=a*b;
	return flx_floats_to_scm(c.arr(),4);
}

SCM FluxusBinding::qnormalise(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a), s_a, SCM_ARG1, "qnormalise");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==4, s_a, SCM_ARG1, "qnormalise");
	
	dQuat a;
	flx_floats_from_scm(s_a,a.arr());
	a.renorm();
	return flx_floats_to_scm(a.arr(),4);
}

SCM FluxusBinding::qtomatrix(SCM s_a)
{
	SCM_ASSERT(scm_is_generalized_vector(s_a), s_a, SCM_ARG1, "qnormalise");
	SCM_ASSERT(scm_c_generalized_vector_length(s_a)==4, s_a, SCM_ARG1, "qnormalise");
	
	dQuat a;
	flx_floats_from_scm(s_a,a.arr());
	dMatrix m=a.toMatrix();
	return flx_floats_to_scm(m.arr(),16);
}

SCM FluxusBinding::mouse_over()
{
	int id=Fluxus->GetRenderer()->Select((int)Fluxus->GetMouseX(),(int)Fluxus->GetMouseY(),5);
	if (id > 0)
	{
		return Prim2Smob(id);
	}
	else
	{
		return scm_from_bool(false);
	}
}

SCM FluxusBinding::mouse_button(SCM s_b)
{
	SCM_ASSERT(scm_is_number(s_b),  s_b,  SCM_ARG1, "mouse-button");
	int but=scm_to_int(s_b);		
	return scm_from_bool(Fluxus->GetMouseButton()==but);
}

SCM FluxusBinding::load_recorded_code(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "load-recorded-code");	
	char *name=scm_to_locale_string(s_name);
	Fluxus->LoadRecordedCode(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::save_recorded_code(SCM s_name)
{
	SCM_ASSERT(scm_is_string(s_name), s_name, SCM_ARG1, "save-recorded-code");	
	char *name=scm_to_locale_string(s_name);
	Fluxus->SaveRecordedCode(name);
	free(name);
    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::searchpaths(SCM s_list)
{
	// todo: fix this...
	//SCM_ASSERT(SCM_LISTP(s_list), s_list, SCM_ARG1, "searchpaths");
 	
	// vectors seem easier to handle than lists with this api
	SCM vec = scm_vector(s_list);
	size_t size=0;
	
	for (unsigned int n=0; n<scm_c_generalized_vector_length(vec); n++)
	{
		SCM arg=scm_vector_ref(vec, scm_from_int(n));

		if (scm_is_string(arg))
		{
			char *argstring=scm_to_locale_string(arg);
			SearchPaths::Get()->AddPath(argstring);
			free(argstring);
		}
	}

    return SCM_UNSPECIFIED;
}

SCM FluxusBinding::full_path(SCM s_filename)
{
	SCM_ASSERT(scm_is_string(s_filename), s_filename, SCM_ARG1, "full-path");	
	char *name=scm_to_locale_string(s_filename);
	string fullpath = SearchPaths::Get()->GetFullPath(name);
	free(name);
	return scm_from_locale_stringn(fullpath.c_str(),fullpath.length());
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
	SchemePrim::Init();

	scm_c_define_gsubr("repl-princ", 1,0,0,(SCM (*)(...))repl_princ);
	scm_c_define_gsubr("repl-print", 1,0,0,(SCM (*)(...))repl_print);
	
	// primitives
	scm_c_define_gsubr("build-polygons",2,0,0,(SCM (*)(...))build_polygons);
	scm_c_define_gsubr("build-cube",0,0,0,(SCM (*)(...))build_cube);
    scm_c_define_gsubr("build-sphere",2,0,0, (SCM (*)(...))build_sphere);
    scm_c_define_gsubr("build-plane",0,0,0,(SCM (*)(...)) static_cast<SCM(*)()>(build_plane));
    scm_c_define_gsubr("build-seg-plane",2,0,0,(SCM (*)(...)) static_cast<SCM(*)(SCM,SCM)>(build_plane));
    scm_c_define_gsubr("build-cylinder",2,0,0, (SCM (*)(...))build_cylinder);
	scm_c_define_gsubr("build-line",1,0,0,   (SCM (*)(...))build_line);
	scm_c_define_gsubr("build-text",1,0,0,   (SCM (*)(...))build_text);
	scm_c_define_gsubr("build-nurbs",1,0,0,  (SCM (*)(...))build_nurbs);
	scm_c_define_gsubr("build-nurbs-sphere",2,0,0, (SCM (*)(...))build_nurbs_sphere);
	scm_c_define_gsubr("build-nurbs-plane",2,0,0, (SCM (*)(...))build_nurbs_plane);
	scm_c_define_gsubr("build-particles",1,0,0,(SCM (*)(...)) build_particles);
	scm_c_define_gsubr("build-locator",0,0,0,(SCM (*)(...)) build_locator);
	scm_c_define_gsubr("build-pixels",2,0,0,(SCM (*)(...)) build_pixels);
	scm_c_define_gsubr("upload-pixels",0,0,0,(SCM (*)(...))upload_pixels);
	scm_c_define_gsubr("pixels->texture",1,0,0,(SCM (*)(...))pixels2texture);
	
    scm_c_define_gsubr("rotate", 1,0,0, (SCM (*)(...))rotate);
	
	scm_c_define_gsubr("draw-instance", 1,0,0,(SCM (*)(...))draw_instance);
    scm_c_define_gsubr("draw-cube", 0,0,0,(SCM (*)(...))	   draw_cube);
    scm_c_define_gsubr("draw-plane", 0,0,0,(SCM (*)(...))     draw_plane);
    scm_c_define_gsubr("draw-sphere",0,0,0,(SCM (*)(...))     draw_sphere);
    scm_c_define_gsubr("draw-cylinder",0,0,0,(SCM (*)(...))   draw_cylinder);
	scm_c_define_gsubr("destroy",1,0,0,(SCM (*)(...))  	destroy);
	scm_c_define_gsubr("get-transform",0,0,0,(SCM (*)(...)) get_transform);
	scm_c_define_gsubr("get-camera-transform",0,0,0,(SCM (*)(...))get_camera_transform);
	scm_c_define_gsubr("set-camera-transform",1,0,0,(SCM (*)(...))set_camera_transform);
	scm_c_define_gsubr("get-projection-transform",0,0,0,(SCM (*)(...)) get_projection_transform);
	scm_c_define_gsubr("get-screen-size",0,0,0,(SCM (*)(...)) get_screen_size);
	scm_c_define_gsubr("set-screen-size",1,0,0,(SCM (*)(...)) set_screen_size);

	// renderstate operations
	scm_c_define_gsubr("push",0,0,0,(SCM (*)(...)) 		   push);
	scm_c_define_gsubr("pop",0,0,0,(SCM (*)(...))  		   pop);
	scm_c_define_gsubr("grab",1,0,0,(SCM (*)(...)) 		grab);
    scm_c_define_gsubr("ungrab",0,0,0,(SCM (*)(...))		   ungrab);
    scm_c_define_gsubr("print-scene-graph",0,0,0,(SCM (*)(...))print_scene_graph);
	scm_c_define_gsubr("apply-transform",1,0,0,(SCM (*)(...))  	  apply);
	scm_c_define_gsubr("identity",0,0,0,(SCM (*)(...)) 	   flux_identity);
	scm_c_define_gsubr("concat",1,0,0,(SCM (*)(...))		   concat);
    scm_c_define_gsubr("translate",1,0,0,(SCM (*)(...))	translate);
    scm_c_define_gsubr("scale",1,0,0,(SCM (*)(...))		scale);
    scm_c_define_gsubr("colour",1,0,0,(SCM (*)(...))		colour);
    scm_c_define_gsubr("wire-colour",1,0,0,(SCM (*)(...))  wire_colour);
    scm_c_define_gsubr("opacity",1,0,0,(SCM (*)(...))  	opacity);
    scm_c_define_gsubr("specular",1,0,0,(SCM (*)(...)) 	specular);
    scm_c_define_gsubr("ambient",1,0,0,(SCM (*)(...))     ambient);
    scm_c_define_gsubr("emissive",1,0,0,(SCM (*)(...)) 	emissive);
	scm_c_define_gsubr("shinyness",1,0,0,(SCM (*)(...))	shinyness);
	scm_c_define_gsubr("texture",1,0,0,(SCM (*)(...))  	texture);
	scm_c_define_gsubr("multitexture",2,0,0,(SCM (*)(...)) multitexture);
    scm_c_define_gsubr("hint-solid",0,0,0,(SCM (*)(...))	   hint_solid);
    scm_c_define_gsubr("hint-wire",0,0,0,(SCM (*)(...))	   hint_wire);
    scm_c_define_gsubr("hint-normal",0,0,0,(SCM (*)(...))     hint_normal);
    scm_c_define_gsubr("hint-points",0,0,0,(SCM (*)(...))     hint_points);
    scm_c_define_gsubr("hint-anti-alias",0,0,0,(SCM (*)(...)) hint_anti_alias);
    scm_c_define_gsubr("hint-none",0,0,0,(SCM (*)(...))	   hint_none);
    scm_c_define_gsubr("hint-unlit",0,0,0,(SCM (*)(...))	   hint_unlit);
    scm_c_define_gsubr("hint-vertcols",0,0,0,(SCM (*)(...))   hint_vertcols);
    scm_c_define_gsubr("hint-box",0,0,0,(SCM (*)(...)) 		hint_box);
    scm_c_define_gsubr("hint-multitex",0,0,0,(SCM (*)(...))   hint_multitex);
    scm_c_define_gsubr("hint-origin",0,0,0,(SCM (*)(...))    hint_origin);
	scm_c_define_gsubr("line-width",1,0,0,(SCM (*)(...))	line_width);
	scm_c_define_gsubr("point-width",1,0,0,(SCM (*)(...))  point_width);
	scm_c_define_gsubr("blend-mode",2,0,0,(SCM (*)(...))	blend_mode);
    scm_c_define_gsubr("parent", 1 ,0,0,(SCM (*)(...)) 	parent);
	scm_c_define_gsubr("hide",1,0,0,(SCM (*)(...)) 		hide);
	scm_c_define_gsubr("selectable",1,0,0,(SCM (*)(...))	selectable);
	
	// global state operations
	scm_c_define_gsubr("clear",0,0,0,(SCM (*)(...))		   clear);
	scm_c_define_gsubr("ortho",0,0,0,(SCM (*)(...))		 ortho);
	scm_c_define_gsubr("persp",0,0,0,(SCM (*)(...))		 persp);
	scm_c_define_gsubr("frustum",4,0,0,(SCM (*)(...))  	frustum);
	scm_c_define_gsubr("clip",2,0,0,(SCM (*)(...)) 		clip);
    scm_c_define_gsubr("reset-camera",0,0,0,(SCM (*)(...))    reset_camera);
	scm_c_define_gsubr("lock-camera",1,0,0,(SCM (*)(...))  lock_camera);
	scm_c_define_gsubr("camera-lag",1,0,0,(SCM (*)(...))  camera_lag);
	scm_c_define_gsubr("clear-colour",1,0,0,(SCM (*)(...))    clear_colour);	 
	scm_c_define_gsubr("clear-frame",1,0,0,(SCM (*)(...))     clear_frame);
	scm_c_define_gsubr("blur",1 ,0,0,(SCM (*)(...))		blur);
	scm_c_define_gsubr("feedback",1,0,0,(SCM (*)(...)) 	feedback);
	scm_c_define_gsubr("feedback-transform",1,0,0,(SCM (*)(...)) feedback_transform);
    scm_c_define_gsubr("show-axis",1,0,0,(SCM (*)(...))	show_axis);
    scm_c_define_gsubr("show-fps", 1,0,0,(SCM (*)(...))	show_fps);
	scm_c_define_gsubr("backfacecull",1,0,0,(SCM (*)(...))    backfacecull);
	scm_c_define_gsubr("load-texture",1,0,0,(SCM (*)(...)) load_texture);
	scm_c_define_gsubr("force-load-texture",1,0,0,(SCM (*)(...)) force_load_texture);
	scm_c_define_gsubr("fog",4,0,0,(SCM (*)(...)) fog);

	// lights
    scm_c_define_gsubr("make-light",1,0,0,(SCM (*)(...))		  make_light);
    scm_c_define_gsubr("clear-lights",0 ,0,0,(SCM (*)(...))	  clear_lights);
	scm_c_define_gsubr("light-ambient",2,0,0,(SCM (*)(...))   light_ambient);
	scm_c_define_gsubr("light-diffuse",2,0,0,(SCM (*)(...))   light_diffuse);
	scm_c_define_gsubr("light-specular",2,0,0,(SCM (*)(...))  light_specular);
	scm_c_define_gsubr("light-position",2,0,0,(SCM (*)(...))  light_position);
	
	// interpreter + misc
	scm_c_define_gsubr("edit",1,0,0,(SCM (*)(...)) edit);
	scm_c_define_gsubr("save-name",1,0,0,(SCM (*)(...)) save_name);
	scm_c_define_gsubr("key-pressed",1,0,0,(SCM (*)(...)) key_pressed);
	scm_c_define_gsubr("mouse-over",0,0,0,(SCM (*)(...)) mouse_over);
	scm_c_define_gsubr("mouse-button",1,0,0,(SCM (*)(...)) mouse_button);
    scm_c_define_gsubr("time",0,0,0,(SCM (*)(...)) time);
    scm_c_define_gsubr("delta",0,0,0,(SCM (*)(...)) delta);
    scm_c_define_gsubr("frame-hook",0,0,0,(SCM (*)(...)) frame_hook);
    scm_c_define_gsubr("flxrnd",0,0,0,(SCM (*)(...)) srandom);
	scm_c_define_gsubr("desiredfps",1,0,0,(SCM (*)(...)) desiredfps);
	scm_c_define_gsubr("start-framedump",2,0,0,(SCM (*)(...)) start_framedump);
	scm_c_define_gsubr("end-framedump",0,0,0,(SCM (*)(...)) end_framedump);
	scm_c_define_gsubr("load-code",1,0,0,(SCM (*)(...)) load_recorded_code);
	scm_c_define_gsubr("save-code",1,0,0,(SCM (*)(...)) save_recorded_code);
	
	// audio
	scm_c_define_gsubr("start-audio",3,0,0,(SCM (*)(...))  start_audio);	 
	scm_c_define_gsubr("smoothing-bias",1,0,0,(SCM (*)(...)) smoothing_bias);   
	scm_c_define_gsubr("gain",1,0,0,(SCM (*)(...)) 		gain);   
	scm_c_define_gsubr("get-harmonic",1,0,0,(SCM (*)(...)) get_harmonic);
	scm_c_define_gsubr("gh", 1,0,0,(SCM (*)(...))  		get_harmonic);
	scm_c_define_gsubr("process",1,0,0,(SCM (*)(...))  	 process);
	
	// turtle
	scm_c_define_gsubr("turtle-vert",0,0,0,(SCM (*)(...))  		  turtle_vert);
	scm_c_define_gsubr("turtle-build",0,0,0,(SCM (*)(...)) 		  turtle_build);
	scm_c_define_gsubr("turtle-reset",0,0,0,(SCM (*)(...)) 		  turtle_reset);
	scm_c_define_gsubr("turtle-push",0,0,0,(SCM (*)(...))  		turtle_push);
	scm_c_define_gsubr("turtle-pop",0,0,0,(SCM (*)(...))			turtle_pop);
	scm_c_define_gsubr("turtle-move",1,0,0,(SCM (*)(...))  	   turtle_move);
	scm_c_define_gsubr("turtle-turn",1,0,0,(SCM (*)(...))  	   turtle_turn);
	scm_c_define_gsubr("turtle-prim",1,0,0,(SCM (*)(...))  	   turtle_prim);
	
	// physics
	scm_c_define_gsubr("collisions",1,0,0,(SCM (*)(...)) collisions);
	scm_c_define_gsubr("gravity",1,0,0,(SCM (*)(...)) gravity);
	scm_c_define_gsubr("set-max-physical",1,0,0,(SCM (*)(...)) set_max_physical);
    scm_c_define_gsubr("active-box",1,0,0,(SCM (*)(...))	  active_box);
    scm_c_define_gsubr("active-sphere",1,0,0,(SCM (*)(...))  active_sphere);
    scm_c_define_gsubr("active-cylinder",1,0,0,(SCM (*)(...)) active_cylinder);
    scm_c_define_gsubr("passive-box",1,0,0,(SCM (*)(...))     passive_box);
    scm_c_define_gsubr("passive-sphere",1,0,0,(SCM (*)(...))  passive_sphere);
    scm_c_define_gsubr("passive-cylinder",1,0,0,(SCM (*)(...))passive_cylinder);
    scm_c_define_gsubr("ground-plane",2,0,0,(SCM (*)(...)) ground_plane);
    scm_c_define_gsubr("build-fixedjoint",1,0,0,(SCM (*)(...)) build_fixedjoint);
    scm_c_define_gsubr("build-hingejoint",4,0,0,(SCM (*)(...)) build_hingejoint);
    scm_c_define_gsubr("build-balljoint",3,0,0,(SCM (*)(...)) build_balljoint);
    scm_c_define_gsubr("build-sliderjoint",3,0,0,(SCM (*)(...)) build_sliderjoint);
    scm_c_define_gsubr("build-hinge2joint",5,0,0,(SCM (*)(...)) build_hinge2joint);
    scm_c_define_gsubr("build-amotorjoint",3,0,0,(SCM (*)(...)) build_amotorjoint);
    scm_c_define_gsubr("surface-params",4,0,0,(SCM (*)(...)) surface_params);
    scm_c_define_gsubr("joint-param",3,0,0,(SCM (*)(...))  joint_param);
    scm_c_define_gsubr("joint-angle",3,0,0,(SCM (*)(...))  joint_angle);
    scm_c_define_gsubr("set-mass",2,0,0,(SCM (*)(...)) 	set_mass);
    scm_c_define_gsubr("kick",2 ,0,0,(SCM (*)(...))		kick);
    scm_c_define_gsubr("twist",2,0,0,(SCM (*)(...))		twist);
	scm_c_define_gsubr("has-collided",1,0,0,(SCM (*)(...)) has_collided);
	
	scm_c_define_gsubr("osc-source",1,0,0,(SCM (*)(...))	osc_source);
	scm_c_define_gsubr("osc",1,0,0,(SCM (*)(...))  		osc);
	scm_c_define_gsubr("osc-peek",0 ,0,0,(SCM (*)(...))	   osc_peek);
	scm_c_define_gsubr("osc-msg",1,0,0,(SCM (*)(...))  	osc_msg);
	scm_c_define_gsubr("osc-destination",1 ,0,0,(SCM (*)(...))   osc_destination);
	scm_c_define_gsubr("osc-send",3,0,0,(SCM (*)(...)) 	osc_send);
	
	// advanced prim editing
	scm_c_define_gsubr("pdata-set",3,0,0,(SCM (*)(...)) pdata_set);
	scm_c_define_gsubr("pdata-get",2,0,0,(SCM (*)(...)) pdata_get);
	scm_c_define_gsubr("pdata-size",0,0,0,(SCM (*)(...)) pdata_size);
	scm_c_define_gsubr("pdata-add",2,0,0,(SCM (*)(...)) pdata_add);
	scm_c_define_gsubr("pdata-copy",2,0,0,(SCM (*)(...)) pdata_copy);
	scm_c_define_gsubr("pdata-op",3,0,0,(SCM (*)(...)) pdata_op);   
	scm_c_define_gsubr("finalise",0,0,0,(SCM (*)(...)) finalise);
	scm_c_define_gsubr("recalc-normals",1,0,0,(SCM (*)(...)) recalc_normals);
	
	// maths
	scm_c_define_gsubr("vmul",2,0,0,(SCM (*)(...)) vmul);
	scm_c_define_gsubr("vadd",2,0,0,(SCM (*)(...)) vadd);
	scm_c_define_gsubr("vsub",2,0,0,(SCM (*)(...)) vsub);
	scm_c_define_gsubr("vdiv",2,0,0,(SCM (*)(...)) vdiv);
	scm_c_define_gsubr("vtransform",2,0,0,(SCM (*)(...)) vtransform);   
	scm_c_define_gsubr("vtransform-rot",2,0,0,(SCM (*)(...)) vtransform_rot);   
	scm_c_define_gsubr("vnormalise",1,0,0,(SCM (*)(...)) vnormalise);   
	scm_c_define_gsubr("vdot",2,0,0,(SCM (*)(...)) vdot);   
	scm_c_define_gsubr("vmag",1,0,0,(SCM (*)(...)) vmag);   
	scm_c_define_gsubr("vdist",2,0,0,(SCM (*)(...)) vdist); 
	scm_c_define_gsubr("vcross",2,0,0,(SCM (*)(...)) vcross);   
	scm_c_define_gsubr("mmul",2,0,0,(SCM (*)(...)) mmul);
	scm_c_define_gsubr("madd",2,0,0,(SCM (*)(...)) madd);
	scm_c_define_gsubr("msub",2,0,0,(SCM (*)(...)) msub);
	scm_c_define_gsubr("mdiv",2,0,0,(SCM (*)(...)) mdiv);
	scm_c_define_gsubr("mident",0,0,0,(SCM (*)(...))  mident);
	scm_c_define_gsubr("mtranslate",1,0,0,(SCM (*)(...)) mtranslate);   
	scm_c_define_gsubr("mrotate",1,0,0,(SCM (*)(...)) mrotate); 
	scm_c_define_gsubr("mscale",1,0,0,(SCM (*)(...)) mscale);
	scm_c_define_gsubr("mtranspose",1,0,0,(SCM (*)(...)) mtranspose);
	scm_c_define_gsubr("minverse",1,0,0,(SCM (*)(...)) minverse);
	scm_c_define_gsubr("maim",2,0,0,(SCM (*)(...)) maim);
	scm_c_define_gsubr("qaxisangle",2,0,0,(SCM (*)(...))qaxisangle);
	scm_c_define_gsubr("qmul",2,0,0,(SCM (*)(...)) qmul);
	scm_c_define_gsubr("qnormalise",1,0,0,(SCM (*)(...))qnormalise);
	scm_c_define_gsubr("qtomatrix",1,0,0,(SCM (*)(...))qtomatrix);

	scm_c_define_gsubr("searchpaths",1,0,0,(SCM (*)(...)) searchpaths);
	scm_c_define_gsubr("full-path",1,0,0,(SCM (*)(...)) full_path);
	
	
}
