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

#include "FluxusPrimitiveBinding.h"
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

void FluxusPrimitiveBinding::RegisterProcs()
{
	SchemePrim::Init();
	
	scm_c_define_gsubr("build-polygons",2,0,0,(CALLBACK_CAST)build_polygons);
	scm_c_define_gsubr("build-cube",0,0,0,(CALLBACK_CAST)build_cube);
    scm_c_define_gsubr("build-sphere",2,0,0, (CALLBACK_CAST)build_sphere);
    scm_c_define_gsubr("build-plane",0,0,0,(CALLBACK_CAST) static_cast<SCM(*)()>(build_plane));
    scm_c_define_gsubr("build-seg-plane",2,0,0,(CALLBACK_CAST) static_cast<SCM(*)(SCM,SCM)>(build_plane));
    scm_c_define_gsubr("build-cylinder",2,0,0, (CALLBACK_CAST)build_cylinder);
	scm_c_define_gsubr("build-line",1,0,0,   (CALLBACK_CAST)build_line);
	scm_c_define_gsubr("build-text",1,0,0,   (CALLBACK_CAST)build_text);
	scm_c_define_gsubr("build-nurbs",1,0,0,  (CALLBACK_CAST)build_nurbs);
	scm_c_define_gsubr("build-nurbs-sphere",2,0,0, (CALLBACK_CAST)build_nurbs_sphere);
	scm_c_define_gsubr("build-nurbs-plane",2,0,0, (CALLBACK_CAST)build_nurbs_plane);
	scm_c_define_gsubr("build-particles",1,0,0,(CALLBACK_CAST) build_particles);
	scm_c_define_gsubr("build-locator",0,0,0,(CALLBACK_CAST) build_locator);
	scm_c_define_gsubr("build-pixels",2,0,0,(CALLBACK_CAST) build_pixels);
	scm_c_define_gsubr("upload-pixels",0,0,0,(CALLBACK_CAST)upload_pixels);
	scm_c_define_gsubr("pixels->texture",1,0,0,(CALLBACK_CAST)pixels2texture);
	
	scm_c_define_gsubr("draw-instance", 1,0,0,(CALLBACK_CAST)draw_instance);
    scm_c_define_gsubr("draw-cube", 0,0,0,(CALLBACK_CAST)	   draw_cube);
    scm_c_define_gsubr("draw-plane", 0,0,0,(CALLBACK_CAST)     draw_plane);
    scm_c_define_gsubr("draw-sphere",0,0,0,(CALLBACK_CAST)     draw_sphere);
    scm_c_define_gsubr("draw-cylinder",0,0,0,(CALLBACK_CAST)   draw_cylinder);
	scm_c_define_gsubr("destroy",1,0,0,(CALLBACK_CAST)  	destroy);
}

SCM FluxusPrimitiveBinding::build_polygons(SCM s_size, SCM s_type)
{
	SCM_ASSERT(scm_is_number(s_size), s_size, SCM_ARG1, "build-polygons");
	SCM_ASSERT(scm_is_number(s_type), s_type, SCM_ARG2, "build-polygons");
	
	PolyPrimitive *Prim = new PolyPrimitive((PolyPrimitive::Type)scm_to_int(s_type));
	Prim->Resize(scm_to_int(s_size));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusPrimitiveBinding::build_nurbs(SCM s_size)
{
	SCM_ASSERT(scm_is_number(s_size), s_size, SCM_ARG1, "build-nurbs");
	
	NURBSPrimitive *Prim = new NURBSPrimitive();
	Prim->Resize(scm_to_int(s_size));
	return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusPrimitiveBinding::build_cube()
{
	PolyPrimitive *BoxPrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakeCube(BoxPrim);
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(BoxPrim));    
}

SCM FluxusPrimitiveBinding::build_sphere(SCM s_hsegments, SCM s_rsegments)
{
	SCM_ASSERT(scm_is_number(s_hsegments), s_hsegments, SCM_ARG1, "build_sphere");
	SCM_ASSERT(scm_is_number(s_rsegments), s_rsegments, SCM_ARG2, "build_sphere");
	int hsegments=scm_to_int(s_hsegments);
    int rsegments=scm_to_int(s_rsegments);	
	
	PolyPrimitive *SphPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeSphere(SphPrim, 1, hsegments, rsegments);
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(SphPrim));
}

SCM FluxusPrimitiveBinding::build_plane()
{
	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(PlanePrim);
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(PlanePrim));
}

SCM FluxusPrimitiveBinding::build_plane(SCM s_xsegments, SCM s_ysegments)
{
	SCM_ASSERT(scm_is_number(s_xsegments), s_xsegments, SCM_ARG1, "build-plane");
	SCM_ASSERT(scm_is_number(s_ysegments), s_ysegments, SCM_ARG2, "build-plane");

	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(PlanePrim,scm_to_int(s_xsegments),scm_to_int(s_ysegments));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(PlanePrim));
}

SCM FluxusPrimitiveBinding::build_cylinder(SCM s_hsegments, SCM s_rsegments)
{
	SCM_ASSERT(scm_is_number(s_hsegments), s_hsegments, SCM_ARG1, "build_cylinder");
	SCM_ASSERT(scm_is_number(s_rsegments), s_rsegments, SCM_ARG2, "build_cylinder");
	
	PolyPrimitive *CylPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeCylinder(CylPrim, 1, 1, scm_to_int(s_hsegments), scm_to_int(s_rsegments));

    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(CylPrim));
}

SCM FluxusPrimitiveBinding::build_line(SCM s_numpoints)
{
	SCM_ASSERT(scm_is_number(s_numpoints), s_numpoints, SCM_ARG1, "build-line");
	
	LinePrimitive *Prim = new LinePrimitive();
	Prim->Resize(scm_to_int(s_numpoints));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusPrimitiveBinding::build_text(SCM s_text)
{
	SCM_ASSERT(scm_is_string(s_text), s_text, SCM_ARG1, "build_text");
	
	char *text=scm_to_locale_string(s_text);
	
	// 16*16 grid of letters
	TextPrimitive *TextPrim = new TextPrimitive(16/256.0f,16/256.0f,16,0);
	TextPrim->SetText(text,20,-20,0.018);
	
	return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(TextPrim));
}
	
SCM FluxusPrimitiveBinding::build_nurbs_sphere(SCM s_hsegments, SCM s_rsegments)
{
	SCM_ASSERT(scm_is_number(s_hsegments), s_hsegments, SCM_ARG1, "build_nurbs_sphere");
	SCM_ASSERT(scm_is_number(s_rsegments), s_rsegments, SCM_ARG2, "build_nurbs_sphere");
	
	NURBSPrimitive *SphPrim = new NURBSPrimitive;
    MakeNURBSSphere(SphPrim, 1, scm_to_int(s_hsegments), scm_to_int(s_rsegments));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(SphPrim));
}

SCM FluxusPrimitiveBinding::build_nurbs_plane(SCM s_usegments, SCM s_vsegments)
{
	SCM_ASSERT(scm_is_number(s_usegments), s_usegments, SCM_ARG1, "build_nurbs_sphere");
	SCM_ASSERT(scm_is_number(s_vsegments), s_vsegments, SCM_ARG2, "build_nurbs_sphere");
		
	NURBSPrimitive *Prim = new NURBSPrimitive;
    MakeNURBSPlane(Prim, scm_to_int(s_usegments), scm_to_int(s_vsegments));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusPrimitiveBinding::build_particles(SCM s_count)
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

SCM FluxusPrimitiveBinding::build_locator()
{
	LocatorPrimitive *Prim = new LocatorPrimitive();
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusPrimitiveBinding::build_pixels(SCM s_w, SCM s_h)
{
	SCM_ASSERT(scm_is_number(s_w), s_w, SCM_ARG1, "build-pixels");
	SCM_ASSERT(scm_is_number(s_h), s_h, SCM_ARG1, "build-pixels");
	
	PixelPrimitive *Prim = new PixelPrimitive((int)scm_to_double(s_w), (int)scm_to_double(s_h));
    return Prim2Smob(Fluxus->GetRenderer()->AddPrimitive(Prim));
}

SCM FluxusPrimitiveBinding::upload_pixels()
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

SCM FluxusPrimitiveBinding::pixels2texture(SCM s_ob)
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

SCM FluxusPrimitiveBinding::draw_instance(SCM s_ob)
{    	
    Fluxus->GetRenderer()->RenderPrimitive(Fluxus->GetRenderer()->GetPrimitive(Smob2Prim(s_ob)));
    return SCM_UNSPECIFIED;
}

SCM FluxusPrimitiveBinding::draw_cube()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticCube);
    return SCM_UNSPECIFIED;
}

SCM FluxusPrimitiveBinding::draw_plane()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticPlane);
    return SCM_UNSPECIFIED;
}

SCM FluxusPrimitiveBinding::draw_sphere()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticSphere);
    return SCM_UNSPECIFIED;
}

SCM FluxusPrimitiveBinding::draw_cylinder()
{    	
    Fluxus->GetRenderer()->RenderPrimitive(StaticCylinder);
    return SCM_UNSPECIFIED;
}

SCM FluxusPrimitiveBinding::destroy(SCM s_name)
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
