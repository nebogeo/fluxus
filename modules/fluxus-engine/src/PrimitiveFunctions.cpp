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

#include <assert.h>
#include "Engine.h"
#include "SchemeHelper.h"
#include "PrimitiveFunctions.h"
#include "dada.h"
#include "GraphicsUtils.h"
#include "LinePrimitive.h"
#include "TextPrimitive.h"
#include "ParticlePrimitive.h"
#include "LocatorPrimitive.h"
#include "PixelPrimitive.h"
#include "BlobbyPrimitive.h"

using namespace PrimitiveFunctions;
using namespace fluxus;
using namespace SchemeHelper;

// StartSectionDoc-en
// Primitives
// Primitives are objects that you can render. There isn't really much else in a fluxus scene, 
// except lights, a camera and lots of primitives.
// Example:
// EndSectionDoc 

// StartFunctionDoc-en
// build-cube
// Returns: primitiveid-number
// Description:
// A simple cube, texture mapped placement per face
// @image{images/cube}
// Example:
// (define mynewcube (build-cube))
// EndFunctionDoc

Scheme_Object *build_cube(int argc, Scheme_Object **argv)
{
	PolyPrimitive *BoxPrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakeCube(BoxPrim);
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(BoxPrim));    
}

Scheme_Object *build_nurbs(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-nurbs", "i", argc, argv);
	NURBSPrimitive *Prim = new NURBSPrimitive();
	Prim->Resize(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// build-polygins verts-number type-number
// Returns: primitiveid-number
// Description:
// Builds a raw polygon primitive with size vertices (everything is initially set to zero). 
// Type is a number that refers to the way the vertices are interpreted to build polygons, 
// and can be one of the following: 0=TRISTRIP, 1=QUADS, 2=TRILIST, 3=TRIFAN, 4=POLYGON
// Example:
// (define mynewshape (build-polygons 100 0))
// EndFunctionDoc

Scheme_Object *build_polygons(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-polygons", "ii", argc, argv);
	PolyPrimitive *Prim = new PolyPrimitive((PolyPrimitive::Type)IntFromScheme(argv[1]));
	Prim->Resize(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// build-sphere slices-number stacks-number
// Returns: primitiveid-number
// Description:
// A sphere with the resolution specified, texture mapped in normal "world map" style.
// @image{images/sphere}
// Example:
// (define mynewshape (build-sphere 10 10))
// EndFunctionDoc

Scheme_Object *build_sphere(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-sphere", "ii", argc, argv);
	PolyPrimitive *SphPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeSphere(SphPrim, 1, IntFromScheme(argv[0]), IntFromScheme(argv[1]));
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(SphPrim));
}

// StartFunctionDoc-en
// build-plane
// Returns: primitiveid-number
// Description:
// A single quad plane, texture mapped from 0->1 in both dimensions.
// @image{images/plane}
// Example:
// (define mynewshape (build-plane))
// EndFunctionDoc

Scheme_Object *build_plane(int argc, Scheme_Object **argv)
{
	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);
	MakePlane(PlanePrim);
   	return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(PlanePrim));
}

// StartFunctionDoc-en
// build-seg-plane vertsx-number vertsy-number
// Returns: primitiveid-number
// Description:
// A tesselated poly plane, texture mapped from 0->1 in both dimensions.
// Example:
// (define mynewshape (build-plane))
// EndFunctionDoc

Scheme_Object *build_seg_plane(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-seg-plane", "ii", argc, argv);
	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);	
    MakePlane(PlanePrim,IntFromScheme(argv[0]),IntFromScheme(argv[1]));
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(PlanePrim));
}

// StartFunctionDoc-en
// build-cylinder hsegments rsegments
// Returns: primitiveid-number
// Description:
// A capped cylinder, texture map wrapped around, and badly wrapped around the ends.
// @image{images/cylinder}
// Example:
// (define mynewshape (build-cylinder 10 10))
// EndFunctionDoc

Scheme_Object *build_cylinder(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-cylinder", "ii", argc, argv);
	PolyPrimitive *CylPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeCylinder(CylPrim, 1, 1, IntFromScheme(argv[0]), IntFromScheme(argv[1]));
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(CylPrim));
}

// StartFunctionDoc-en
// build-line numpoints-number
// Returns: primitiveid-number
// Description:
// Builds a line consisting of numpoints points. The geometry is constantly camera facing and 
// is texture mapped so the texture is stretched along the line from start to finish. You use 
// the pdata functions to edit the postions and widths of the lines. If used lit, the normals 
// are faked to approximate a circular cross section. Additionally, if solid rendering is 
// cleared with (hint-none) and (hint-wire) is activated, a faster constant width line will be 
// drawn - width specified by the (line-width) command.
// @image{images/line}
// Example:
// (define mynewshape (build-line 10))
// EndFunctionDoc

Scheme_Object *build_line(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-line", "i", argc, argv);
	LinePrimitive *Prim = new LinePrimitive();
	Prim->Resize(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// build-text text-string
// Returns: primitiveid-number
// Description:
// Builds a sequence of planes, texture mapped so that a font texture can be used to display 
// text. Might also be useful for more abstract things. The font assumed to be non proportional 
// - there is an example font shipped with fluxus
// @image{images/text}
// Ok, so this isn't a very good font texture :)
// Example:
// (texture (texture-load "font.png"))
// (define mynewshape (build-text "hello"))
// EndFunctionDoc

Scheme_Object *build_text(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-text", "s", argc, argv);
		
	// 16*16 grid of letters
	TextPrimitive *TextPrim = new TextPrimitive(16/256.0f,16/256.0f,16,0);
	TextPrim->SetText(StringFromScheme(argv[0]),20,-20,0.018);
	MZ_GC_UNREG(); 
	
	return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(TextPrim));
}

// StartFunctionDoc-en
// build-nurbs-sphere hsegments rsegments
// Returns: primitiveid-number
// Description:
// Builds a tesselated nurbs sphere, texture mapped in the same fashion as the poly sphere.
// @image{images/nurbs-sphere}
// Example:
// (define mynewshape (build-nurbs-sphere 10 10))
// EndFunctionDoc
	
Scheme_Object *build_nurbs_sphere(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-nurbs-sphere", "ii", argc, argv);
	NURBSPrimitive *SphPrim = new NURBSPrimitive;
    MakeNURBSSphere(SphPrim, 1, IntFromScheme(argv[0]), IntFromScheme(argv[1]));
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(SphPrim));
}

// StartFunctionDoc-en
// build-nurbs-plane hsegments rsegments
// Returns: primitiveid-number
// Description:
// Builds a tesselated nurbs plane, texture mapped in uv direction.
// @image{images/nurbs-plane}
// Example:
// (define mynewshape (build-nurbs-plane 10 10))
// EndFunctionDoc

Scheme_Object *build_nurbs_plane(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-nurbs-plane", "ii", argc, argv);
	NURBSPrimitive *Prim = new NURBSPrimitive;
    MakeNURBSPlane(Prim, IntFromScheme(argv[0]), IntFromScheme(argv[1]));
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// build-particles count-number
// Returns: primitiveid-number
// Description:
// Builds a particles primitive containing num points, all initially set to the  origin. You 
// use the pdata functions to edit the postions, colours and sizes. Particles come in two 
// flavors, camera facing sprites, which are the default, can be textured and individually 
// scaled; and points (when hint-points is set), which cannot be textured but are much faster 
// to render, as they are hardware supported gl points. By default these point particles are 
// square, turn on hint-anti-alias to make them circular.
// @image{images/sprites}
// Example:
// (define mynewshape (build-particles 100))
// EndFunctionDoc


Scheme_Object *build_particles(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-particles", "i", argc, argv);
	ParticlePrimitive *Prim = new ParticlePrimitive;
	int count=IntFromScheme(argv[0]);
	for (int i=0; i<count; i++)
	{
		Prim->AddParticle(dVector(0,0,0),dColour(0,0,0),dVector(0.1,0.1,0.1));
	}
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// build-locator 
// Returns: primitiveid-number
// Description:
// A locator is an empty primitive, useful for parenting to (when you don't want to have the 
// parent object visible). This primitive can only be visualised with (hint-origin) to display 
// it's local transform origin.
// Example:
// (define mynewshape (build-locator))
// EndFunctionDoc

Scheme_Object *build_locator(int argc, Scheme_Object **argv)
{
	LocatorPrimitive *Prim = new LocatorPrimitive();
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// build-pixels width-number height-number 
// Returns: primitiveid-number
// Description:
// Makes a new pixel primitive. A pixel primitive is used for making procedural textures, which 
// can then be applied to other primitives. For this reason, pixel primitives probably wont be 
// rendered much, but you can render them to preview the texture on a flat plane.
// Example:
// (define mynewshape (build-pixels 100 100))
// EndFunctionDoc

Scheme_Object *build_pixels(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-pixels", "ii", argc, argv);
	PixelPrimitive *Prim = new PixelPrimitive(IntFromScheme(argv[0]), IntFromScheme(argv[1]));
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// upload-pixels pixelprimitiveid-number
// Returns: void
// Description:
// Uploads the texture data, you need to call this when you've finished writing to the 
// pixelprim, and while it's grabbed.
// Example:
// (define mynewshape (build-pixels 100 100))
// (upload-pixels mynewshape)
// EndFunctionDoc

Scheme_Object *upload_pixels(int argc, Scheme_Object **argv)
{	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed) 
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Grabbed);
		if (pp)
		{
			pp->Upload();
		    return scheme_void;
		}
	}
	
	cerr<<"upload-pixels can only be called while a pixelprimitive is grabbed"<<endl;
    return scheme_void;
}

// StartFunctionDoc-en
// pixels->texture pixelprimitiveid-number
// Returns: textureid-number
// Description:
// Returns a texture you can use exactly like a normal loaded one.
// Example:
// (define mynewshape (build-pixels 100 100))
// (upload-pixels mynewshape)
// (texture (pixels->texture mynewshape))
// EndFunctionDoc

Scheme_Object *pixels2texture(int argc, Scheme_Object **argv)
{		
	DECL_ARGV();
	ArgCheck("pixels->texture", "i", argc, argv);
	Primitive *Prim=Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]));
	if (Prim) 
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Prim);
		if (pp)
		{
			MZ_GC_UNREG(); 
		    return scheme_make_integer_value(pp->GetTexture());
		}
	}
	
	cerr<<"pixels->texture can only be called on a pixelprimitive"<<endl;
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// build-blobby numinfluences subdivisionsvec boundingvec
// Returns: primitiveid-number
// Description:
// Blobby primitives are a higher level implicit surface representation in fluxus which is 
// defined using influences in 3 dimesional space. These infuences are then summed together, 
// and a particular value is "meshed" (using the marching cubes algorithm) to form a smooth 
// surface. The influences can be animated, and the smooth surface moves and deforms to adapt, 
// giving the primitive it's blobby name.
// 
// build-blobby returns a new blobby primitive. Numinfluences is the number of "blobs". 
// Subdivisions allows you to control the resolution of the surface in each dimension, while 
// boundingvec sets the bounding area of the primitive in local object space. The mesh will not 
// be calculated outside of this area. Influence positions and colours need to be set using 
// pdata-set.
// Example:
// (define mynewshape (build-blobby 7 (vector 30 30 30) (vector 3 3 3)))
// EndFunctionDoc

Scheme_Object *build_blobby(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-blobby", "ivv", argc, argv);
	
	dVector dim;
	FloatsFromScheme(argv[1],dim.arr(),3);
	dVector size;
	FloatsFromScheme(argv[2],size.arr(),3);
	BlobbyPrimitive *Prim = new BlobbyPrimitive((int)dim.x,(int)dim.y,(int)dim.z,size);
	int count=IntFromScheme(argv[0]);
	for (int i=0; i<count; i++)
	{
		Prim->AddInfluence(dVector(0,0,0),0);
	}
	
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// draw-instance primitiveid-number
// Returns: void
// Description:
// Copies a retained mode primitive and draws it in the current state as an immediate mode
// primitive.
// Example:
// (define mynewshape (build-cube))
// (colour (vector 1 0 0))
// (draw-instance mynewshape) ; draws a copy of mynewshape
// EndFunctionDoc

Scheme_Object *draw_instance(int argc, Scheme_Object **argv)
{    	
	DECL_ARGV();
	ArgCheck("draw-instance", "i", argc, argv);
    Engine::Get()->Renderer()->RenderPrimitive(Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0])));
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// draw-cube 
// Returns: void
// Description:
// Draws a cube in the current state in immediate mode
// primitive.
// Example:
// (define (render)
//     (draw-cube))
// (every-frame (render))
// EndFunctionDoc

Scheme_Object *draw_cube(int argc, Scheme_Object **argv)
{    	
    Engine::Get()->Renderer()->RenderPrimitive(Engine::StaticCube);
    return scheme_void;
}

// StartFunctionDoc-en
// draw-plane
// Returns: void
// Description:
// Draws a plane in the current state in immediate mode
// primitive.
// Example:
// (define (render)
//     (draw-plane))
// (every-frame (render))
// EndFunctionDoc

Scheme_Object *draw_plane(int argc, Scheme_Object **argv)
{    	
    Engine::Get()->Renderer()->RenderPrimitive(Engine::StaticPlane);
    return scheme_void;
}

// StartFunctionDoc-en
// draw-sphere
// Returns: void
// Description:
// Draws a sphere in the current state in immediate mode
// primitive.
// Example:
// (define (render)
//     (draw-sphere))
// (every-frame (render))
// EndFunctionDoc

Scheme_Object *draw_sphere(int argc, Scheme_Object **argv)
{    	
    Engine::Get()->Renderer()->RenderPrimitive(Engine::StaticSphere);
    return scheme_void;
}

// StartFunctionDoc-en
// draw-cylinder
// Returns: void
// Description:
// Draws a cylinder in the current state in immediate mode
// primitive.
// Example:
// (define (render)
//     (draw-cylinder))
// (every-frame (render))
// EndFunctionDoc

Scheme_Object *draw_cylinder(int argc, Scheme_Object **argv)
{    	
    Engine::Get()->Renderer()->RenderPrimitive(Engine::StaticCylinder);
    return scheme_void;
}

// StartFunctionDoc-en
// destroy primitiveid-number
// Returns: void
// Description:
// Deletes a built primitive from the renderer.
// primitive.
// Example:
// (define mynewshape (build-sphere 10 10))
// (destroy mynewshape)
// EndFunctionDoc

Scheme_Object *destroy(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("destroy", "i", argc, argv);
	int name=0;
	name=IntFromScheme(argv[0]);	
	
	Primitive *p=Engine::Get()->Renderer()->GetPrimitive(name);
	if (p)
	{
    	if (p->IsPhysicalHint())
    	{
    		Engine::Get()->Physics()->Free(name);
    	}
    	Engine::Get()->Renderer()->RemovePrimitive(name);
    }

	MZ_GC_UNREG(); 
    return scheme_void;
}

void PrimitiveFunctions::AddGlobals(Scheme_Env *env)
{	
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();
	scheme_add_global("draw-cube", scheme_make_prim_w_arity(draw_cube, "draw-cube", 0, 0), env);
	scheme_add_global("build-cube", scheme_make_prim_w_arity(build_cube, "build-cube", 0, 0), env);
	scheme_add_global("build-polygons", scheme_make_prim_w_arity(build_polygons, "build-polygons", 2, 2), env);
	scheme_add_global("build-nurbs", scheme_make_prim_w_arity(build_nurbs, "build-nurbs", 1, 1), env);
	scheme_add_global("build-sphere", scheme_make_prim_w_arity(build_sphere, "build-sphere", 2, 2), env);
	scheme_add_global("build-plane", scheme_make_prim_w_arity(build_plane, "build-plane", 0, 0), env);
	scheme_add_global("build-seg-plane", scheme_make_prim_w_arity(build_seg_plane, "build-seg-plane", 2, 2), env);
	scheme_add_global("build-cylinder", scheme_make_prim_w_arity(build_cylinder, "build-cylinder", 2, 2), env);
	scheme_add_global("build-line", scheme_make_prim_w_arity(build_line, "build-line", 1, 1), env);
	scheme_add_global("build-text", scheme_make_prim_w_arity(build_text, "build-text", 1, 1), env);
	scheme_add_global("build-nurbs-sphere", scheme_make_prim_w_arity(build_nurbs_sphere, "build-nurbs-sphere", 2, 2), env);
	scheme_add_global("build-nurbs-plane", scheme_make_prim_w_arity(build_nurbs_plane, "build-nurbs-sphere", 2, 2), env);
	scheme_add_global("build-particles", scheme_make_prim_w_arity(build_particles, "build-particles", 1, 1), env);
	scheme_add_global("build-locator", scheme_make_prim_w_arity(build_locator, "build-locator", 0, 0), env);
	scheme_add_global("build-pixels", scheme_make_prim_w_arity(build_pixels, "build-pixels", 2, 2), env);
	scheme_add_global("pload-pixels", scheme_make_prim_w_arity(upload_pixels, "upload-pixels", 0, 0), env);
	scheme_add_global("pixels->texture", scheme_make_prim_w_arity(pixels2texture, "pixels->texture", 1, 1), env);
	scheme_add_global("build-blobby", scheme_make_prim_w_arity(build_blobby, "build-blobby", 3, 3), env);
	scheme_add_global("draw-instance", scheme_make_prim_w_arity(draw_instance, "draw-instance", 1, 1), env);
	scheme_add_global("draw-cube", scheme_make_prim_w_arity(draw_cube, "draw-cube", 0, 0), env);
	scheme_add_global("draw-plane", scheme_make_prim_w_arity(draw_plane, "draw-plane", 0, 0), env);
	scheme_add_global("draw-sphere", scheme_make_prim_w_arity(draw_sphere, "draw-sphere", 0, 0), env);
	scheme_add_global("draw-cylinder", scheme_make_prim_w_arity(draw_cylinder, "draw-cylinder", 0, 0), env);
	scheme_add_global("destroy", scheme_make_prim_w_arity(destroy, "destroy", 1, 1), env);
 	MZ_GC_UNREG(); 
}
