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
#include "RibbonPrimitive.h"
#include "TextPrimitive.h"
#include "ParticlePrimitive.h"
#include "LocatorPrimitive.h"
#include "PixelPrimitive.h"
#include "BlobbyPrimitive.h"
#include "TypePrimitive.h"
#include "ImagePrimitive.h"
#include "VoxelPrimitive.h"
#include "PrimitiveIO.h"
#include "SearchPaths.h"
#include "Evaluator.h"

///\todo remove all the dynamic casts and replace with a homespun version to do with grabbed

using namespace PrimitiveFunctions;
using namespace Fluxus;
using namespace SchemeHelper;

// StartSectionDoc-en
// primitives
// Primitives are objects that you can render. There isn't really much else in a fluxus scene,
// except lights, a camera and lots of primitives.
// Example:
// EndSectionDoc

// StartSectionDoc-pt
// primitivas
// Primitivas são objetos que você pode renderizar. Não há muito mais coisas
// numa cena do fluxus, exceto luzes, uma camera e muitas primitivas.
// Exemplo:
// EndSectionDoc

// StartFunctionDoc-en
// build-cube
// Returns: primitiveid-number
// Description:
// A simple cube, texture mapped placement per face.
// Example:
// (define mynewcube (build-cube))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-cube
// Retorna: número-de-id-da-primitiva
// Descrição:
// Um simples cubo, mapeamento de textura por face.
// Exemplo:
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
	int size=IntFromScheme(argv[0]);
	if (size<1)
	{
		Trace::Stream<<"build-nurbs: size less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	NURBSPrimitive *Prim = new NURBSPrimitive();
	Prim->Resize(size);
	MZ_GC_UNREG();
	return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// build-polygons verts-number type-symbol
// Returns: primitiveid-number
// Description:
// Builds a raw polygon primitive with size vertices (everything is initially set to zero).
// Type is a symbol that refers to the way the vertices are interpreted to build polygons,
// and can be one of the following: triangle-strip quad-list triangle-list triangle-fan polygon
// Example:
// (define mynewshape (build-polygons 100 'triangle-strip))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-polygons número-de-vértices tipo-de-número
// Retorna: número-de-id-primitiva
// Descrição:
// Cónstroi uma primitiva de polígono cru com tamanho de vértices
// (tudo é inicialmente tido como zero).
// Tipo é um número que referencia a forma as quais os vértices são
// interpretados para construir os poligonos, e podem ser os
// seguintes:
// 0=TRISTRIP, 1=QUADS, 2=TRILIST, 3=TRIFAN, 4=POLYGON.
// Exemplo:
// (define mynewshape (build-polygons 100 'triangle-strip))
// EndFunctionDoc

Scheme_Object *build_polygons(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-polygons", "iS", argc, argv);
	int size=IntFromScheme(argv[0]);
	string t = SymbolName(argv[1]);
	PolyPrimitive::Type type=PolyPrimitive::TRISTRIP;
	if (t=="triangle-strip") type=PolyPrimitive::TRISTRIP;
	else if (t=="quad-list") type=PolyPrimitive::QUADS;
	else if (t=="triangle-list") type=PolyPrimitive::TRILIST;
	else if (t=="triangle-fan") type=PolyPrimitive::TRIFAN;
	else if (t=="polygon") type=PolyPrimitive::POLYGON;
	else
	{
		Trace::Stream<<"build-polygons: unknown poly type: "<<t<<endl;
	}
	if (size<1)
	{
		Trace::Stream<<"build-polygons: size less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	PolyPrimitive *Prim = new PolyPrimitive(type);
	Prim->Resize(size);
	MZ_GC_UNREG();
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// build-sphere slices-number stacks-number
// Returns: primitiveid-number
// Description:
// A sphere with the resolution specified, texture mapped in normal "world map" style.
// Example:
// (define mynewshape (build-sphere 10 10))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-sphere número-de-cortes-horizontais número-de-cortes-verticais
// Retorna: número-id-primitiva
// Descrição:
// Uma esfera com a resolução especificada, a textura mapeada no
// estilo normal "world map".
// Exemplo:
// (define mynewshape (build-sphere 10 10))
// EndFunctionDoc

Scheme_Object *build_sphere(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-sphere", "ii", argc, argv);
	int x=IntFromScheme(argv[0]);
	int y=IntFromScheme(argv[1]);
	if (x<1 || y<1)
	{
		Trace::Stream<<"build-sphere: resolution in x or y less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	PolyPrimitive *SphPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeSphere(SphPrim, 1, x, y);
	MZ_GC_UNREG();
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(SphPrim));
}

// StartFunctionDoc-en
// build-icosphere level-number
// Returns: primitiveid-number
// Description:
// Build sphere by recursively subdividing an icosahedron.
// Example:
// (define mynewshape (build-icosphere 1))
// EndFunctionDoc

Scheme_Object *build_icosphere(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-icosphere", "i", argc, argv);
	int l=IntFromScheme(argv[0]);
	if (l<1)
	{
		Trace::Stream<<"build-icosphere: level is less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	PolyPrimitive *SphPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeIcosphere(SphPrim, l);
	MZ_GC_UNREG();
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(SphPrim));
}

// StartFunctionDoc-en
// build-torus inner-radius-number outer-radius-number slices-number stacks-number
// Returns: primitiveid-number
// Description:
// A torus with the resolution specified, control the size and thickness of the
// donut with the inner and outer radius.
// Example:
// (define mynewshape (build-torus 0.5 1 12 12))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-torus número-raio-interior número-raio-exterior número-cortes número-fatias
// Retorna: número-id-primitiva
// Descrição:
// Um torus com a resolução especificada, controle o tamanho e a
// grossura da "rosquinha" com o raio inferior e exterior.
// Exemplo:
// (define mynewshape (build-torus 0.5 1 12 12))
// EndFunctionDoc

Scheme_Object *build_torus(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-torus", "ffii", argc, argv);
	int x=IntFromScheme(argv[2]);
	int y=IntFromScheme(argv[3]);
	if (x<1 || y<1)
	{
		Trace::Stream<<"build-torus: resolution in x or y less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	PolyPrimitive *Prim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakeTorus(Prim, FloatFromScheme(argv[0]), FloatFromScheme(argv[1]), x, y);
	MZ_GC_UNREG();
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// build-plane
// Returns: primitiveid-number
// Description:
// A single quad plane, texture mapped from 0->1 in both dimensions.
// Example:
// (define mynewshape (build-plane))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-plane
// Retorna: número-id-primitiva
// Descrição:
// Um único plano quadrado, mapeado de 0->1 em ambas as dimensões.
// Exemplo:
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

// StartFunctionDoc-pt
// build-seg-plane número-de-vértices-x número-de-vértices-y
// Retorna: número-id-primitiva
// Descrição:
// Um plano poligonal tesselado, mapeado de 0->1 em ambas dimensões.
// Exemplo:
// (define mynewshape (build-plane))
// EndFunctionDoc

Scheme_Object *build_seg_plane(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-seg-plane", "ii", argc, argv);
	int x=IntFromScheme(argv[0]);
	int y=IntFromScheme(argv[1]);
	if (x<1 || y<1)
	{
		Trace::Stream<<"build-plane: resolution in x or y less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	PolyPrimitive *PlanePrim = new PolyPrimitive(PolyPrimitive::QUADS);
    MakePlane(PlanePrim,x,y);
	MZ_GC_UNREG();
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(PlanePrim));
}

// StartFunctionDoc-en
// build-cylinder hsegments rsegments
// Returns: primitiveid-number
// Description:
// A capped cylinder, texture map wrapped around, and badly wrapped around the ends.
// Example:
// (define mynewshape (build-cylinder 10 10))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-cylinder h-segmentos r-segmentos
// Retorna: número-id-primitiva
// Descrição:
// Um cilindro coberto, textura mapeada em volta, e mal mapeada em
// volta do fim.
// Exemplo:
// (define mynewshape (build-cylinder 10 10))
// EndFunctionDoc

Scheme_Object *build_cylinder(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-cylinder", "ii", argc, argv);
	int x=IntFromScheme(argv[0]);
	int y=IntFromScheme(argv[1]);
	if (x<1 || y<1)
	{
		Trace::Stream<<"build-cylinder: resolution in x or y less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	PolyPrimitive *CylPrim = new PolyPrimitive(PolyPrimitive::TRILIST);
    MakeCylinder(CylPrim, 1, 1, x, y);
	MZ_GC_UNREG();
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(CylPrim));
}

// StartFunctionDoc-en
// build-ribbon numpoints-number
// Returns: primitiveid-number
// Description:
// Builds a ribbon consisting of numpoints points. The geometry is constantly camera facing and
// is texture mapped so the texture is stretched along the ribbon from start to finish. You use
// the pdata functions to edit the postions and widths of the segments. If used lit, the normals
// are faked to approximate a circular cross section. Additionally, if solid rendering is
// cleared with (hint-none) and (hint-wire) is activated, a faster constant width line will be
// drawn - width specified by the (line-width) command.
// Example:
// (define mynewshape (build-ribbon 10))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-ribbon numpoints-número
// Retorna: número-id-primitiva
// Descrição:
// Cónstroi uma linha consistindo de numpoints pontos. A geometria
// aponta para a câmera constantemente e a textura é mapeada de forma
// que se alonga na linha do início ao fim. Você usa as funções pdata
// para editar as posições e largura das linhas. Se usado iluminado,
// as normais são falseadas para aproximar uma seção circular
// cruzada. Adicionalmente, se renderização sólida for limpa com
// (hint-none) e (hint-wire) ativado, uma rápida linha constante vai
// ser desenhada - largura específicada pelo comando (line-width).
// Exemplo:
// (define mynewshape (build-ribbon 10))
// EndFunctionDoc

Scheme_Object *build_ribbon(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-ribbon", "i", argc, argv);
	int size=IntFromScheme(argv[0]);
	if (size<1)
	{
		Trace::Stream<<"build-ribbon: size is less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	RibbonPrimitive *Prim = new RibbonPrimitive();
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
// Ok, so this isn't a very good font texture :)
// Example:
// (texture (load-texture "font.png"))
// (define mynewshape (build-text "hello"))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-text string-texto
// Retorna: número-id-primitiva
// Descrição:
// Constrói uma sequencia de planos, mapeados de forma que uma textura
// de fonte possa ser usada para visualização. Pode vir a ser útil para
// coisas mais abstratas. A fonte é assumida como não proporcional -
// tem um exemplo de fonte acompanhando o fluxus.
// Ok, so this isn't a very good font texture :)
// Exemplo:
// (texture (load-texture "font.png"))
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
// build-type ttf-filename text-string
// Returns: primitiveid-number
// Description:
// Builds a geometric type primitive from a ttf font and some text.
// Example:
// (clear)
// (wire-colour (vector 0 0 1))
// ; this font should be on the default fluxus path (as it's the editor font)
// (define t (build-type "Bitstream-Vera-Sans-Mono.ttf" "fluxus rocks!!"))
//
// ; make a poly primitive from the type
// (define p (with-state
//     (translate (vector 0 4 0))
//     (type->poly t)))
//
// ; set some texture coords on the poly prim and load a texture onto it
// (with-primitive p
//     (pdata-map!
//         (lambda (t p)
//             (vmul p 0.5))
//         "t" "p")
//     (texture (load-texture "refmap.png")))
// EndFunctionDoc

Scheme_Object *build_type(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-type", "ss", argc, argv);

	TypePrimitive *TypePrim = new TypePrimitive();
	if (TypePrim->LoadTTF(StringFromScheme(argv[0])))
	{
		TypePrim->SetText(StringFromScheme(argv[1]));
		MZ_GC_UNREG();
		return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(TypePrim));
	}
	else
	{
		MZ_GC_UNREG();
		delete TypePrim;
		return scheme_void;
	}
}

// StartFunctionDoc-en
// build-extruded-type ttf-filename text-string extrude-depth
// Returns: primitiveid-number
// Description:
// Builds an extruded geometric type primitive from a ttf font and some text.
// Example:
// (clear)
// (wire-colour (vector 0 0 1))
// ; this font should be on the default fluxus path (as it's the editor font)
// (define t (build-extruded-type "Bitstream-Vera-Sans-Mono.ttf" "fluxus rocks!!" 1))
//
// ; make a poly primitive from the type
// (define p (with-state
//     (translate (vector 0 4 0))
//     (type->poly t)))
//
// ; set some texture coords on the poly prim and load a texture onto it
// (with-primitive p
//     (pdata-map!
//         (lambda (t p)
//             (vmul p 0.5))
//         "t" "p")
//     (texture (load-texture "refmap.png")))
// EndFunctionDoc

Scheme_Object *build_extruded_type(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-extruded-type", "ssf", argc, argv);

	TypePrimitive *TypePrim = new TypePrimitive();
	if(TypePrim->LoadTTF(StringFromScheme(argv[0])))
	{
		TypePrim->SetTextExtruded(StringFromScheme(argv[1]),FloatFromScheme(argv[2]));
		MZ_GC_UNREG();
		return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(TypePrim));
	}
	else
	{
		MZ_GC_UNREG();
		delete TypePrim;
		return scheme_void;
	}
}

// StartFunctionDoc-en
// type->poly typeprimitiveid-number
// Returns: polyprimid-number
// Description:
// Converts the mesh of a type primitive into a triangle list polygon primitive. The poly primitive
// will be a bit slower to render, but you'll be able to do everything you can do with a poly primitive
// with it, such as add textures and deform it.
// Example:
// (clear)
// (wire-colour (vector 0 0 1))
// ; this font should be on the default fluxus path (as it's the editor font)
// (define t (build-extruded-type "Bitstream-Vera-Sans-Mono.ttf" "fluxus rocks!!" 1))
//
// ; make a poly primitive from the type
// (define p (with-state
//     (translate (vector 0 4 0))
//     (type->poly t)))
//
// ; set some texture coords on the poly prim and load a texture onto it
// (with-primitive p
//     (pdata-map!
//         (lambda (t p)
//             (vmul p 0.5))
//         "t" "p")
//     (texture (load-texture "refmap.png")))
// EndFunctionDoc

Scheme_Object *type2poly(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("type->poly", "i", argc, argv);
	Primitive *Prim=Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]));
	if (Prim)
	{
		// only if this is a pixel primitive
		TypePrimitive *tp = dynamic_cast<TypePrimitive *>(Prim);
		if (tp)
		{
			PolyPrimitive *np = new PolyPrimitive(PolyPrimitive::TRILIST);
			tp->ConvertToPoly(*np);
			MZ_GC_UNREG();
			return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(np));
		}
	}

	Trace::Stream<<"type->poly can only be called on a typeprimitive"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}



// StartFunctionDoc-en
// text-params width-number height-number stride-number wrap-number
// Returns: primitiveid-number
// Description:
// Sets parameters for making fonts from texture maps. Defaults: 16/256 16/256 16 0
// Example:
// ; don't use me!
// EndFunctionDoc

Scheme_Object *text_params(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("text-params", "sffffffffff", argc, argv);

	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		TextPrimitive *tp = dynamic_cast<TextPrimitive *>(Grabbed);
		if (tp)
		{
			tp->SetTextParams(FloatFromScheme(argv[1]),FloatFromScheme(argv[2]),
							  IntFromScheme(argv[3]),IntFromScheme(argv[4]),
							  FloatFromScheme(argv[5]),FloatFromScheme(argv[6]),
							  FloatFromScheme(argv[10]));
			tp->SetText(StringFromScheme(argv[0]),FloatFromScheme(argv[7]),
						FloatFromScheme(argv[8]),FloatFromScheme(argv[9]));
		}
	}
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// ribbon-inverse-normals 1/0
// Returns: void
// Description:
// Inverts the automatically generated ribbon normals
// Example:
// (define mynewshape (build-ribbon 10))
// (with-primitive mynewshape
//     (ribbon-inverse-normals 1))
// EndFunctionDoc

Scheme_Object *ribbon_inverse_normals(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("ribbon-inverse-normals", "i", argc, argv);

	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		RibbonPrimitive *rp = dynamic_cast<RibbonPrimitive *>(Grabbed);
		if (rp)
		{
			rp->SetInverseNormals(IntFromScheme(argv[0]));
		}
	}
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// build-nurbs-sphere hsegments rsegments
// Returns: primitiveid-number
// Description:
// Builds a tesselated nurbs sphere, texture mapped in the same fashion as the poly sphere.
// Since the nurbs sphere uses additional control points the primitive data count can be
// calculated by (hsegments + 3) * rsegments.
// Example:
// (define mynewshape (build-nurbs-sphere 10 10))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-nurbs-sphere h-segmentos r-segmentos
// Retorna: número-id-primitiva
// Descrição:
// Constrói uma esfera nurbs tesselada, mapead da mesma forma que a
// esfera poligonal.
// Exemplo:
// (define mynewshape (build-nurbs-sphere 10 10))
// EndFunctionDoc

Scheme_Object *build_nurbs_sphere(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-nurbs-sphere", "ii", argc, argv);
	int x=IntFromScheme(argv[0]);
	int y=IntFromScheme(argv[1]);
	if (x<1 || y<1)
	{
		Trace::Stream<<"build-nurbs-sphere: resolution in x or y less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	NURBSPrimitive *SphPrim = new NURBSPrimitive;
    MakeNURBSSphere(SphPrim, 1, x, y);
	MZ_GC_UNREG();
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(SphPrim));
}

// StartFunctionDoc-en
// build-nurbs-plane xsegments ysegments
// Returns: primitiveid-number
// Description:
// Builds a tesselated nurbs plane, texture mapped in uv direction.
// Since the nurbs plane uses additional control points the primitive data count can be
// calculated by (xsegments + 1) * (ysegments + 1)
// Example:
// (define mynewshape (build-nurbs-plane 10 10))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-nurbs-plane h-segmento r-segmento
// Retorna: número-id-primitiva
// Descrição:
// Constrói um plano nurbs tesselado, mapeado na direção uv.
// Exemplo:
// (define mynewshape (build-nurbs-plane 10 10))
// EndFunctionDoc

Scheme_Object *build_nurbs_plane(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-nurbs-plane", "ii", argc, argv);
	int x=IntFromScheme(argv[0]);
	int y=IntFromScheme(argv[1]);
	if (x<1 || y<1)
	{
		Trace::Stream<<"build-nurbs-plane: resolution in x or y less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	NURBSPrimitive *Prim = new NURBSPrimitive;
    MakeNURBSPlane(Prim, x, y);
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
// Example:
// (define mynewshape (build-particles 100))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-particles número-contagem
// Retorna: número-id-primitiva
// Descrição:
// Cónstroi uma primitiva de partículas contendo num pontos, tudo
// inicialmente aplicado à origem. Você usa as funções pdata para
// editar as posições, cores e tamanhos. Partículas vêm em dois tipos,
// sprites apontando pra câmera, que são o padrão, podem ser
// texturizadas e escaladas individualmente; e pontuais (quando
// hint-points está aplicado), que não podem ser texturizadas mas são
// muito mais rápidas de renderizar, já que elas são pontos gl
// suportados pelo hardware. Por defeito essas partículas pontuais são
// quadradas, ligue hint-anti-alias para faze-las circulares.
// Exemplo:
// (define mynewshape (build-particles 100))
// EndFunctionDoc

Scheme_Object *build_particles(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-particles", "i", argc, argv);
	int size=IntFromScheme(argv[0]);
	if (size<1)
	{
		Trace::Stream<<"build-particles: size less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	ParticlePrimitive *Prim = new ParticlePrimitive;
	for (int i=0; i<size; i++)
	{
		Prim->AddParticle(dVector(0,0,0),dColour(0,0,0),dVector(0.1,0.1,0.1));
	}
	MZ_GC_UNREG();
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// build-image texture-number coordinate-vector size-vector
// Returns: primitiveid-number
// Description:
// Builds an image, which is displayed on screen using 2D orthographic projection.
// Coordinate defines the location of the image from the upper-left corner. Size
// specifies the image display resolution in pixels.
// Example:
// (define img (build-image (load-texture "test.png") (vector 0 0) (get-screen-size)))
// EndFunctionDoc

Scheme_Object *build_image(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-image", "i??", argc, argv);
	for (int i = 1; i <= 2; i++)
	{
		if (!SCHEME_VECTORP(argv[i]))
		{
			MZ_GC_UNREG();
			scheme_wrong_type("build-image", "vector", i, argc, argv);
		}
		if (SCHEME_VEC_SIZE(argv[i]) != 2)
		{
			MZ_GC_UNREG();
			scheme_wrong_type("build-image", "vector size 2", i, argc, argv);
		}
	}

	float topleft[2], size[2];
	FloatsFromScheme(argv[1], topleft, 2);
	FloatsFromScheme(argv[2], size, 2);

	ImagePrimitive *ImagePrim = new ImagePrimitive(Engine::Get()->Renderer(),
				IntFromScheme(argv[0]),
				topleft[0], topleft[1],
				size[0], size[1]);
	MZ_GC_UNREG();

	return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(ImagePrim));
}

// StartFunctionDoc-en
// build-voxels width-number height-number depth-number 
// Returns: primitiveid-number
// Description:
// Builds voxels primitive, similar to pixel primitives, except include a 3rd dimension.
// Example:
// (define vox (build-voxels 10 10 10))
// EndFunctionDoc
Scheme_Object *build_voxels(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-voxels", "iii", argc, argv);

	VoxelPrimitive *VoxPrim = new VoxelPrimitive(IntFromScheme(argv[0]),
												IntFromScheme(argv[1]),
												IntFromScheme(argv[2]));
	MZ_GC_UNREG();

	return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(VoxPrim));
}

// StartFunctionDoc-en
// voxels->blobby voxelsprimitiveid-number
// Returns: blobbyprimid-number
// Description:
// Converts the voxels from a voxels primitive to those of a blobby primitive, this is only
// really of use for then converting the voxels to a polygon primitive. The blobby is just the
// intermediate step.
// Example:
//
// EndFunctionDoc
Scheme_Object *voxels2blobby(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("voxels->blobby", "i", argc, argv);
	Primitive *Prim=Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]));
	if (Prim)
	{
		// only if this is a voxel primitive
		VoxelPrimitive *vp = dynamic_cast<VoxelPrimitive *>(Prim);
		if (vp)
		{
			BlobbyPrimitive *bp = vp->ConvertToBlobby();
			MZ_GC_UNREG();
			return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(bp));
		}
	}

	Trace::Stream<<"voxels->blobby can only be called on a voxelsprimitive"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// voxels->poly voxelsprimitiveid-number [isolevel-threshold-number]
// Returns: polyprimid-number
// Description:
// Converts the voxels from a voxels primitive into a triangle list polygon primitive.
// Example:
// (clear)
// (define vx (build-voxels 16 16 16))
// (with-primitive vx
//    (voxels-sphere-influence (vector 0 0 0) (vector 1 1 1 .1) .5)
//    (voxels-calc-gradient)
//    (voxels-point-light (vector .5 .5 .5) (vector 1 0 1)))
// (define pb (voxels->poly vx))
// (with-primitive pb
//    (recalc-normals 0)
//    (translate #(1.1 0 0)))
// EndFunctionDoc

Scheme_Object *voxels2poly(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	float thres = 1.0;
	if (argc == 1)
	{
		ArgCheck("voxels->poly", "i", argc, argv);
	}
	else
	{
		ArgCheck("voxels->poly", "if", argc, argv);
		thres = FloatFromScheme(argv[1]);
	}
	Primitive *Prim=Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]));
	if (Prim)
	{
		// only if this is a voxel primitive
		VoxelPrimitive *vp = dynamic_cast<VoxelPrimitive *>(Prim);
		if (vp)
		{
			BlobbyPrimitive *bp = vp->ConvertToBlobby();

			PolyPrimitive *np = new PolyPrimitive(PolyPrimitive::TRILIST);
			bp->ConvertToPoly(*np, thres);
			delete bp;
			MZ_GC_UNREG();
			return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(np));
		}
	}

	Trace::Stream<<"voxels->poly can only be called on a voxelsprimitive"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// voxels-width
// Returns: width-number
// Description:
// Returns the width of the current voxels primitive.
// Example:
// (define mynewshape (build-voxels 10 10 10))
// (with-primitive mynewshape
//     (display (vector (voxels-width) (voxels-height) (voxels-depth)))(newline))
// EndFunctionDoc
Scheme_Object *voxels_width(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		VoxelPrimitive *pp = dynamic_cast<VoxelPrimitive *>(Grabbed);
		if (pp)
		{
			MZ_GC_UNREG();
		    return scheme_make_integer_value(pp->GetWidth());
		}
	}

	Trace::Stream<<"voxels-width can only be called on a voxelsprimitive"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// voxels-height
// Returns: height-number
// Description:
// Returns the height of the current voxels primitive.
// Example:
// (define mynewshape (build-voxels 10 10 10))
// (with-primitive mynewshape
//     (display (vector (voxels-width) (voxels-height) (voxels-depth)))(newline))
// EndFunctionDoc
Scheme_Object *voxels_height(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		VoxelPrimitive *pp = dynamic_cast<VoxelPrimitive *>(Grabbed);
		if (pp)
		{
			MZ_GC_UNREG();
		    return scheme_make_integer_value(pp->GetHeight());
		}
	}

	Trace::Stream<<"voxels-height can only be called on a voxelsprimitive"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// voxels-depth
// Returns: depth-number
// Description:
// Returns the depth of the current voxels primitive.
// Example:
// (define mynewshape (build-voxels 10 10 10))
// (with-primitive mynewshape
//     (display (vector (voxels-width) (voxels-height) (voxels-depth)))(newline))
// EndFunctionDoc
Scheme_Object *voxels_depth(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		VoxelPrimitive *pp = dynamic_cast<VoxelPrimitive *>(Grabbed);
		if (pp)
		{
			MZ_GC_UNREG();
		    return scheme_make_integer_value(pp->GetDepth());
		}
	}

	Trace::Stream<<"voxels-height can only be called on a pixelprimitive"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// voxels-calc-gradient
// Returns: void
// Description:
// Fills out the "g" pdata array with the gradient of the voxel intensities. 
// This is the first derivative, the change in intensity in x,y and z directions
// across the voxels. This can be used to approximate normals, is mainly of 
// use for lighting the voxels.
// Example:
// (clear)
// (blend-mode 'src-alpha 'one)
// (hint-nozwrite)
// (texture (load-texture "splat.png"))
// (define p (build-voxels 30 30 30))
// (define t 0)
//
// (every-frame (with-primitive p 
//     (set! t (+ t 0.02))
//     (for ((i (in-range 0 10)))
//         (voxels-sphere-solid (vector (+ 0.5 (* 0.5 (sin (+ (* i 0.1) t)))) 0.5 0.5) 
//             (if (odd? i) (vector 0 0 0) (vmul (vector 1 0.1 0.1) (* i 0.05)))
//             (- 1 (/ i 10))))
//
//     (voxels-box-solid (vector 0.5 0.5 -0.1) (vector 1.1 1.1 1.1) (vector 0 0 0))
//     (voxels-calc-gradient)
//     (voxels-point-light (vector 0 50 0) (vmul (vector 1 1 1) 0.05))))
// EndFunctionDoc
Scheme_Object *voxels_calc_gradient(int argc, Scheme_Object **argv)
{		
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed) 
	{
		// only if this is a pixel primitive
		VoxelPrimitive *pp = dynamic_cast<VoxelPrimitive *>(Grabbed);
		if (pp)
		{
			pp->CalcGradient();
		    return scheme_void;
		}
	}

	Trace::Stream<<"voxels-calc-gradient can only be called while a voxels primitive is grabbed"<<endl;
    return scheme_void;
}

// StartFunctionDoc-en
// voxels-sphere-influence pos-vector colour-vector strength
// Returns: void
// Description:
// Adds a coloured sperical influence into the voxel values from the centre position.
// Example:
// (clear)
// (blend-mode 'src-alpha 'one)
// (hint-nozwrite)
// (texture (load-texture "splat.png"))
// (define p (build-voxels 30 30 30))
// (define t 0)
//
// (every-frame (with-primitive p
//     (set! t (+ t 0.02))
//     (for ((i (in-range 0 10)))
//         (voxels-sphere-solid (vector (+ 0.5 (* 0.5 (sin (+ (* i 0.1) t)))) 0.5 0.5)
//             (if (odd? i) (vector 0 0 0) (vmul (vector 1 0.1 0.1) (* i 0.05)))
//             (- 1 (/ i 10))))
//
//     (voxels-box-solid (vector 0.5 0.5 -0.1) (vector 1.1 1.1 1.1) (vector 0 0 0))
//     (voxels-calc-gradient)
//     (voxels-point-light (vector 0 50 0) (vmul (vector 1 1 1) 0.05))))
// EndFunctionDoc
Scheme_Object *voxels_sphere_influence(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("voxels-sphere-influence", "vcf", argc, argv);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		VoxelPrimitive *pp = dynamic_cast<VoxelPrimitive *>(Grabbed);
		if (pp)
		{
			dVector pos;
			FloatsFromScheme(argv[0],pos.arr(),3);
			dColour col = ColourFromScheme(argv[1], Engine::Get()->State()->ColourMode);
			pp->SphereInfluence(pos,col,FloatFromScheme(argv[2]));
			MZ_GC_UNREG();
		    return scheme_void;
		}
	}
	MZ_GC_UNREG();

	Trace::Stream<<"voxels-sphere-influence can only be called while a voxels primitive is grabbed"<<endl;
    return scheme_void;
}

// StartFunctionDoc-en
// voxels-sphere-solid pos-vector colour-vector radius
// Returns: void
// Description:
// Sets a solid sphere of coloured voxel values.
// Example:
// (clear)
// (blend-mode 'src-alpha 'one)
// (hint-nozwrite)
// (texture (load-texture "splat.png"))
// (define p (build-voxels 30 30 30))
// (define t 0)
//
// (every-frame (with-primitive p
//     (set! t (+ t 0.02))
//     (for ((i (in-range 0 10)))
//         (voxels-sphere-solid (vector (+ 0.5 (* 0.5 (sin (+ (* i 0.1) t)))) 0.5 0.5)
//             (if (odd? i) (vector 0 0 0) (vmul (vector 1 0.1 0.1) (* i 0.05)))
//             (- 1 (/ i 10))))
//
//     (voxels-box-solid (vector 0.5 0.5 -0.1) (vector 1.1 1.1 1.1) (vector 0 0 0))
//     (voxels-calc-gradient)
//     (voxels-point-light (vector 0 50 0) (vmul (vector 1 1 1) 0.05))))
// EndFunctionDoc
Scheme_Object *voxels_sphere_solid(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("voxels-sphere-solid", "vcf", argc, argv);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		VoxelPrimitive *pp = dynamic_cast<VoxelPrimitive *>(Grabbed);
		if (pp)
		{
			dVector pos;
			FloatsFromScheme(argv[0],pos.arr(),3);
			dColour col = ColourFromScheme(argv[1], Engine::Get()->State()->ColourMode);
			pp->SphereSolid(pos,col,FloatFromScheme(argv[2]));
			MZ_GC_UNREG();
		    return scheme_void;
		}
	}
	MZ_GC_UNREG();

	Trace::Stream<<"voxels-sphere-solid can only be called while a voxels primitive is grabbed"<<endl;
    return scheme_void;
}

// StartFunctionDoc-en
// voxels-sphere-cube topleft-vector botright-vector colour
// Returns: void
// Description:
// Sets a solid cube of coloured voxel values.
// Example:
// (clear)
// (blend-mode 'src-alpha 'one)
// (hint-nozwrite)
// (texture (load-texture "splat.png"))
// (define p (build-voxels 30 30 30))
// (define t 0)
//
// (every-frame (with-primitive p
//     (set! t (+ t 0.02))
//     (for ((i (in-range 0 10)))
//         (voxels-sphere-solid (vector (+ 0.5 (* 0.5 (sin (+ (* i 0.1) t)))) 0.5 0.5)
//             (if (odd? i) (vector 0 0 0) (vmul (vector 1 0.1 0.1) (* i 0.05)))
//             (- 1 (/ i 10))))
//
//     (voxels-box-solid (vector 0.5 0.5 -0.1) (vector 1.1 1.1 1.1) (vector 0 0 0))
//     (voxels-calc-gradient)
//     (voxels-point-light (vector 0 50 0) (vmul (vector 1 1 1) 0.05))))
// EndFunctionDoc
Scheme_Object *voxels_box_solid(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("voxels-box-solid", "vvc", argc, argv);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		VoxelPrimitive *pp = dynamic_cast<VoxelPrimitive *>(Grabbed);
		if (pp)
		{
			dVector top;
			FloatsFromScheme(argv[0],top.arr(),3);
			dVector bot;
			FloatsFromScheme(argv[1],bot.arr(),3);
			dColour col = ColourFromScheme(argv[2], Engine::Get()->State()->ColourMode);
			pp->BoxSolid(top,bot,col);
			MZ_GC_UNREG();
		    return scheme_void;
		}
	}
	MZ_GC_UNREG();

	Trace::Stream<<"voxels-box-solid can only be called while a voxels primitive is grabbed"<<endl;
    return scheme_void;
}

// StartFunctionDoc-en
// voxels-threshold thresh-number
// Returns: void
// Description:
// Thresholds the voxels and colours those above the threshold white and those
// below the threshold black.
// Example:
// (clear)
// (blend-mode 'src-alpha 'one)
// (hint-nozwrite)
// (texture (load-texture "splat.png"))
// (define p (build-voxels 30 30 30))
// (define t 0)
//
// (every-frame (with-primitive p
//     (set! t (+ t 0.02))
//     (for ((i (in-range 0 10)))
//         (voxels-sphere-solid (vector (+ 0.5 (* 0.5 (sin (+ (* i 0.1) t)))) 0.5 0.5) 
//             (if (odd? i) (vector 0 0 0) (vmul (vector 1 0.1 0.1) (* i 0.05)))
//             (- 1 (/ i 10))))
//
//     (voxels-box-solid (vector 0.5 0.5 -0.1) (vector 1.1 1.1 1.1) (vector 0 0 0))
//     (voxels-calc-gradient)
//     (voxels-point-light (vector 0 50 0) (vmul (vector 1 1 1) 0.05))))
// EndFunctionDoc
Scheme_Object *voxels_threshold(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("voxels-threshold", "f", argc, argv);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		VoxelPrimitive *pp = dynamic_cast<VoxelPrimitive *>(Grabbed);
		if (pp)
		{
			pp->Threshold(FloatFromScheme(argv[0]));
			MZ_GC_UNREG();
		    return scheme_void;
		}
	}
	MZ_GC_UNREG();

	Trace::Stream<<"voxels-threshold can only be called while a voxels primitive is grabbed"<<endl;
    return scheme_void;
}

// StartFunctionDoc-en
// voxels-point-light pos-vector colour-vector
// Returns: void
// Description:
// Uses the gradient pdata array to light the voxels from a point light source
// Example:
// (clear)
// (blend-mode 'src-alpha 'one)
// (hint-nozwrite)
// (texture (load-texture "splat.png"))
// (define p (build-voxels 30 30 30))
// (define t 0)
//
// (every-frame (with-primitive p
//     (set! t (+ t 0.02))
//     (for ((i (in-range 0 10)))
//         (voxels-sphere-solid (vector (+ 0.5 (* 0.5 (sin (+ (* i 0.1) t)))) 0.5 0.5)
//             (if (odd? i) (vector 0 0 0) (vmul (vector 1 0.1 0.1) (* i 0.05)))
//             (- 1 (/ i 10))))
//
//     (voxels-box-solid (vector 0.5 0.5 -0.1) (vector 1.1 1.1 1.1) (vector 0 0 0))
//     (voxels-calc-gradient)
//     (voxels-point-light (vector 0 50 0) (vmul (vector 1 1 1) 0.05))))
// EndFunctionDoc
Scheme_Object *voxels_point_light(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("voxels-point-light", "vc", argc, argv);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		VoxelPrimitive *pp = dynamic_cast<VoxelPrimitive *>(Grabbed);
		if (pp)
		{
			dVector pos;
			FloatsFromScheme(argv[0],pos.arr(),3);
			dColour col = ColourFromScheme(argv[1], Engine::Get()->State()->ColourMode);
			pp->PointLight(pos,col);
			MZ_GC_UNREG();
		    return scheme_void;
		}
	}
	MZ_GC_UNREG();

	Trace::Stream<<"voxels-point-light can only be called while a voxels primitive is grabbed"<<endl;
    return scheme_void;
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

// StartFunctionDoc-pt
// build-locator
// Retorna: número-id-primitiva
// Descrição:
// Um locator é uma primitiva vazia, útil para parentesco (quando você
// não quer ter o objeto pai visivel). Essa primitiva só pode ser
// visualizada com (hint-origin) para mostrar sua origem de
// transformação local.
// Exemplo:
// (define mynewshape (build-locator))
// EndFunctionDoc

Scheme_Object *build_locator(int argc, Scheme_Object **argv)
{
	LocatorPrimitive *Prim = new LocatorPrimitive();
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// locator-bounding-radius
// locator-bounding-radius size-number
// Returns: void
// Description:
// Sets the bounding box radius for the locator
// Example:
// (define mylocator (build-locator))
// (with-primitive mylocator
//     (locator-bounding-radius 23.4))
// EndFunctionDoc

Scheme_Object *locator_bounding_radius(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("locator-bounding-radius", "f", argc, argv);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a locator primitive
		LocatorPrimitive *lp = dynamic_cast<LocatorPrimitive *>(Grabbed);
		if (lp)
		{
			lp->SetBoundingBoxRadius(FloatFromScheme(argv[0]));
			MZ_GC_UNREG();
			return scheme_void;
		}
	}

	Trace::Stream<<"locator-bounding-radius can only be called while a locator primitive is grabbed"<<endl;
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// load-primitive
// Returns: primitiveid-number
// Description:
// Loads a primitive from disk
// Example:
// (define mynewshape (load-primitive "octopus.obj"))
// EndFunctionDoc

// StartFunctionDoc-pt
// load-primitive
// Retorna: número-id-primitiva
// Descrição:
// Exemplo:
// (define mynewshape (load-primitive "octopus.obj"))
// EndFunctionDoc

Scheme_Object *load_primitive(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("load-primitive", "p", argc, argv);
	string filename=PathFromScheme(argv[0]);
	Primitive *Prim = PrimitiveIO::Read(SearchPaths::Get()->GetFullPath(filename));
	if (Prim!=NULL)
	{
		MZ_GC_UNREG();
		return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
	}
	return scheme_make_integer_value(0);
}

// StartFunctionDoc-en
// clear-geometry-cache
// Returns: void
// Description:
// Clears cached geometry, so subsequent loads with come from the disk.
// Example:
// (clear-geometry-cache)
// EndFunctionDoc

// StartFunctionDoc-pt
// clear-geometry-cache
// Retorna: número-id-primitiva
// Descrição:
// Exemplo:
// (clear-geometry-cache)
// EndFunctionDoc

Scheme_Object *clear_geometry_cache(int argc, Scheme_Object **argv)
{
	PrimitiveIO::ClearGeometryCache();
	return scheme_void;
}

// StartFunctionDoc-en
// save-primitive
// Returns: void
// Description:
// Saves the current primitive to disk. Pixel primitives are saved as PNG
// files.
// Polygon primitives are saved as Wavefront OBJ files with material
// information. The children of the primitives are also saved as different
// objects in the file. New groups are generated for each locator in the
// hierarchy.
// Example:
// (define l (build-locator))
// (with-primitive (build-sphere 10 10)
//    (colour #(1 0 0))
//    (parent l))
// (translate (vector 3 2 0))
// (rotate (vector 15 60 70))
// (with-primitive (build-torus 1 2 10 10)
//    (colour #(0 1 0))
//    (parent l))
// (with-primitive l
//    (save-primitive "p.obj"))
// EndFunctionDoc

// StartFunctionDoc-pt
// save-primitive
// Retorna: número-id-primitiva
// Descrição:
// Exemplo:
// (with-primitive (build-sphere 10 10)
//     (save-primitive "mymesh.obj"))
// EndFunctionDoc

Scheme_Object *save_primitive(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("save-primitive", "s", argc, argv);
	string filename = StringFromScheme(argv[0]);
	Primitive *Grabbed = Engine::Get()->Renderer()->Grabbed();
	unsigned id = Engine::Get()->GrabbedID();
	if (Grabbed)
	{
		PrimitiveIO::Write(filename, Grabbed, id,
						Engine::Get()->Renderer()->GetSceneGraph());
	}
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// build-pixels width-number height-number renderer-active-boolean
// Returns: primitiveid-number
// Description:
// Makes a new pixel primitive. A pixel primitive is used for making procedural textures, which
// can then be applied to other primitives. For this reason, pixel primitives probably wont be
// rendered much, but you can render them to preview the texture on a flat plane.
// Objects can be rendered into pixels primitives. The pixels primitive renderer is disabled by
// default. To enable it, use the renderer-activate optional boolean parameter.
// Example:
// (clear)
// (define p1 (build-pixels 16 16))
// (with-primitive p1
//    (pdata-map!
//        (lambda (c)
//            (rndvec))
//        "c")
//    (pixels-upload)) ; call pixels upload to see the results
//
// (translate (vector 1.5 0 0))
// (define p2 (build-pixels 256 256 #t)) ; render target
// (with-pixels-renderer p2 ; render a cube into the pixels primitive
//    (clear-colour (vector .37 .5 .59))
//    (scale 5)
//    (rotate (vector -5 60 140))
//    (build-cube))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-pixels número-largura número-altura
// Retorna: número-id-primitiva
// Descrição:
// Faz uma nova primitiva pixel. Uma primitiva pixel é usada pra fazer
// texturas procedurais, que podem então ser aplicadas em outras
// primitivas. Por essa razão, primitivas pixel não vão ser
// renderizadas muito, mas você pode renderizar elas para visualizar a
// texturas em um plano.
// Exemplo:
// (define mynewshape (build-pixels 100 100))
// (with-primitive mynewshape
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "c")
//     (pixels-upload)) ; call pixels upload to see the results
// EndFunctionDoc

Scheme_Object *build_pixels(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	bool rend = false;
	unsigned txts = 1;
	if (argc == 2)
	{
		ArgCheck("build-pixels", "ii", argc, argv);
	}
	else
	if (argc == 3)
	{
		ArgCheck("build-pixels", "iib", argc, argv);
		rend = BoolFromScheme(argv[2]);
	}
	else
	{
		ArgCheck("build-pixels", "iibi", argc, argv);
		rend = BoolFromScheme(argv[2]);
		txts = IntFromScheme(argv[3]);
	}

	int x=IntFromScheme(argv[0]);
	int y=IntFromScheme(argv[1]);
	if (x<1 || y<1)
	{
		Trace::Stream<<"build-pixels: resolution in x or y less than 1!"<<endl;
		MZ_GC_UNREG();
		return scheme_void;
	}
	PixelPrimitive *Prim = new PixelPrimitive(x, y, rend, txts);
	MZ_GC_UNREG();
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// pixels-upload
// Returns: void
// Description:
// Uploads the texture data, you need to call this when you've finished writing to the
// pixelprim, and while it's grabbed.
// Example:
// (define mynewshape (build-pixels 100 100))
// (with-primitive mynewshape
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "c")
//     (pixels-upload)) ; call pixels upload to see the results
// EndFunctionDoc

// StartFunctionDoc-pt
// pixels-upload
// Retorna: void
// Descrição:
// Traz os dados da textura, você precisa chamar isto quando você
// finalizou escrever ao pixelprim, e enquanto ele está "grabbed".
// Exemplo:
// (define mynewshape (build-pixels 100 100))
// (with-primitive mynewshape
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "c")
//     (pixels-upload)) ; call pixels upload to see the results
// EndFunctionDoc

Scheme_Object *pixels_upload(int argc, Scheme_Object **argv)
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

	Trace::Stream<<"pixels-upload can only be called while a pixelprimitive is grabbed"<<endl;
    return scheme_void;
}

// StartFunctionDoc-en
// pixels-download [texture-id]
// Returns: void
// Description:
// Downloads the texture data from the GPU to the PData array.
// Optional texture id can be supplied to specify the pixel primitive
// texture the data is downloaded from.
// Example:
// (clear)
//
// (define p (build-pixels 256 256 #t))
//
// (define q (with-pixels-renderer p
//         (clear-colour (vector 1 1 1))
//         (scale 3)
//         (colour (vector 0 0 1))
//         (build-torus 0.2 2 10 20)))
//
// (define i (with-state
//         (translate (vector 0.5 0.5 0))
//         (scale 0.2)
//         (build-cube)))
//
// (every-frame
//     (begin
//     (with-primitive p
//         (pixels-download)
//         ; paint the cube with the colour of the pixel underneath it
//         (let ((c (pdata-ref "c" (pixels-index (vector 0.5 0.5 0)))))
//             (with-primitive i (colour c))))
//     (with-pixels-renderer p
//         (with-primitive q
//             (rotate (vector 1 0.2 0))))))
// EndFunctionDoc
Scheme_Object *pixels_download(int argc, Scheme_Object **argv)
{
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Grabbed);
		int handle = 0;
		if (argc == 1)
		{
			ArgCheck("pixels-download", "i", argc, argv);
			handle = IntFromScheme(argv[0]);
		}

		if (pp)
		{
			pp->Download(handle);
		    return scheme_void;
		}
	}

	Trace::Stream<<"pixels-download can only be called while a pixelprimitive is grabbed"<<endl;
    return scheme_void;
}

Scheme_Object *pixels_load(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("pixels-load", "s", argc, argv);

	Trace::Stream<<"pixels-load is deprecated! use load-primitive"<<endl;

	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Grabbed);
		if (pp)
		{
			pp->Load(StringFromScheme(argv[0]));
		    MZ_GC_UNREG();
			return scheme_void;
		}
	}

	Trace::Stream<<"pixels-load can only be called while a pixelprimitive is grabbed"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// pixels->texture pixelprimitiveid-number [textureindex-number]
// Returns: textureid-number
// Description:
// Returns a texture you can use exactly like a normal loaded one.
// Example:
// (define mypixels (build-pixels 100 100))
// (with-primitive mypixels
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "c")
//     (pixels-upload))
//
// (with-state
//     (texture (pixels->texture mypixels))
//     (build-torus 1 2 10 10))
// EndFunctionDoc

// StartFunctionDoc-pt
// pixels->texture número-id-pixelprim
// Retorna: número-id-textura
// Descrição:
// Retorna uma textura que você pode usar exatamente igual uma que foi
// carregada normalmente.
// Exemplo:
// (define mypixels (build-pixels 100 100))
// (with-primitive mypixels
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "c")
//     (pixels-upload))
//
// (with-state
//     (texture (pixels->texture mypixels))
//     (build-torus 1 2 10 10))
// EndFunctionDoc

Scheme_Object *pixels2texture(int argc, Scheme_Object **argv)
{
	DECL_ARGV();

	unsigned txt = 0;
	if (argc == 1)
	{
		ArgCheck("pixels->texture", "i", argc, argv);
	}
	else
	{
		ArgCheck("pixels->texture", "ii", argc, argv);
		txt = IntFromScheme(argv[1]);
	}
	Primitive *Prim=Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]));
	if (Prim)
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Prim);
		if (pp)
		{
			MZ_GC_UNREG();
		    return scheme_make_integer_value(pp->GetTexture(txt));
		}
	}

	Trace::Stream<<"pixels->texture can only be called on a pixelprimitive"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// pixels-width
// Returns: width-number
// Description:
// Returns the width of the current pixel primitive.
// Example:
// (define mynewshape (build-pixels 100 100))
// (with-primitive mynewshape
//     (display (vector (pixels-width) (pixels-height)))(newline))
// EndFunctionDoc

Scheme_Object *pixels_width(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Grabbed);
		if (pp)
		{
			MZ_GC_UNREG();
		    return scheme_make_integer_value(pp->GetWidth());
		}
	}

	Trace::Stream<<"pixels-width can only be called on a pixelprimitive"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// pixels-height
// Returns: width-number
// Description:
// Returns the height of the current pixel primitive.
// Example:
// (define mynewshape (build-pixels 100 100))
// (with-primitive mynewshape
//     (display (vector (pixels-width) (pixels-height)))(newline))
// EndFunctionDoc

Scheme_Object *pixels_height(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Grabbed);
		if (pp)
		{
			MZ_GC_UNREG();
		    return scheme_make_integer_value(pp->GetHeight());
		}
	}

	Trace::Stream<<"pixels-height can only be called on a pixelprimitive"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

Scheme_Object *pixels_render_to(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("pixels-render-to", "i", argc, argv);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Grabbed);
		if (pp)
		{
			pp->SetRenderTexture(IntFromScheme(argv[0]));
			MZ_GC_UNREG();
		    return scheme_void;
		}
	}

	Trace::Stream<<"pixels-render-to can only be called on a pixelprimitive"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

Scheme_Object *pixels_display(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("pixels-display", "i", argc, argv);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Grabbed);
		if (pp)
		{
			pp->SetDisplayTexture(IntFromScheme(argv[0]));
			MZ_GC_UNREG();
		    return scheme_void;
		}
	}

	Trace::Stream<<"pixels-display can only be called on a pixelprimitive"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// pixels-renderer-activate boolean
// Returns: void
// Description:
// Activates/deactivates the pixel primitive renderer.
// Example:
// (clear)
// (define p (build-pixels 256 256))
//
// (with-primitive p
//     (pixels-renderer-activate #t))
//
// (define cube (with-pixels-renderer p
//                    (clear-colour (vector .2 .4 .8))
//                    (rotate #(30 50 80))
//                    (scale 5)
//                    (build-cube)))
// EndFunctionDoc

Scheme_Object *pixels_renderer_activate(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("pixels-renderer-activate", "b", argc, argv);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a pixel primitive
		PixelPrimitive *pp = dynamic_cast<PixelPrimitive *>(Grabbed);
		if (pp)
		{
			pp->ActivateRenderer(BoolFromScheme(argv[0]));
			MZ_GC_UNREG();
		    return scheme_void;
		}
	}

	Trace::Stream<<"pixels-renderer-activate can only be called on a pixelprimitive"<<endl;
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
// (clear)
// (define b (build-blobby 5 (vector 30 30 30) (vector 1 1 1)))
// 
// (with-primitive b
//     (shinyness 100)
//     (specular (vector 1 1 1))
//     (hint-vertcols)
//     (pdata-set "p" 0 (vector 0.75 0.25 0.5))
//     (pdata-set "c" 0 (vector 0.01 0 0))
//     (pdata-set "s" 0 0.01)
//     (pdata-set "p" 1 (vector 0.25 0.75 0.5))
//     (pdata-set "c" 1 (vector 0 0.01 0))
//     (pdata-set "s" 1 0.01)
//     (pdata-set "p" 2 (vector 0.75 0.75 0.5))
//     (pdata-set "c" 2 (vector 0 0 0.01))
//     (pdata-set "s" 2 0.01)
//     (pdata-set "p" 3 (vector 0.25 0.25 0.5))
//     (pdata-set "c" 3 (vector 0.01 0.01 0))
//     (pdata-set "s" 3 0.01)
//     (pdata-set "p" 4 (vector 0.5 0.5 0.5))
//     (pdata-set "c" 4 (vector 0.01 0.01 0.01))
//     (pdata-set "s" 4 0.025))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-blobby número-influencias vecsubdvision vecfronteira
// Retorna: número-id-primitiva
// Descrição:
// Primitivas blobby no fluxus são uma representação de superfície
// implicita de alto nível que é definida usando influências no espaço
// em 3 dimensões. Estas influências são então somadas, e um valor
// particular é "malheado" (usando o algorítmo dos cubos marchando)
// para formar uma superfície macia. Estas influências podem ser
// animadas, e a superfície macia mexe e deforma para adaptar, dando a
// primitiva seu nome blobby.
// 
// build-blobby retorna uma nova primitiva blobby. número-influências
// é o número de "blobs", Subdivisão permite a você controlar a
// resolução da superfície em cada dimensão, enquanto vecfronteira
// ajusta a área fronteriça da primitiva em espaço de objeto local. A
// malha não vai ser calculada fora desta área limite. Influências de
// cores e posições precisam ser ajustadas usando pdata-set.
// Exemplo:
// (clear)
// (define b (build-blobby 5 (vector 30 30 30) (vector 1 1 1)))
// 
// (with-primitive b
//     (shinyness 100)
//     (specular (vector 1 1 1))
//     (hint-vertcols)
//     (pdata-set "p" 0 (vector 0.75 0.25 0.5))
//     (pdata-set "c" 0 (vector 0.01 0 0))
//     (pdata-set "s" 0 0.01)
//     (pdata-set "p" 1 (vector 0.25 0.75 0.5))
//     (pdata-set "c" 1 (vector 0 0.01 0))
//     (pdata-set "s" 1 0.01)
//     (pdata-set "p" 2 (vector 0.75 0.75 0.5))
//     (pdata-set "c" 2 (vector 0 0 0.01))
//     (pdata-set "s" 2 0.01)
//     (pdata-set "p" 3 (vector 0.25 0.25 0.5))
//     (pdata-set "c" 3 (vector 0.01 0.01 0))
//     (pdata-set "s" 3 0.01)
//     (pdata-set "p" 4 (vector 0.5 0.5 0.5))
//     (pdata-set "c" 4 (vector 0.01 0.01 0.01))
//     (pdata-set "s" 4 0.025))
// EndFunctionDoc

Scheme_Object *build_blobby(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-blobby", "ivv", argc, argv);
	int count=IntFromScheme(argv[0]);
	if (count<1)
	{
		Trace::Stream<<"build-blobby: size less than 1!"<<endl;
		MZ_GC_UNREG(); 
		return scheme_void;
	}	
	
	dVector dim;
	FloatsFromScheme(argv[1],dim.arr(),3);
	dVector size;
	FloatsFromScheme(argv[2],size.arr(),3);
	BlobbyPrimitive *Prim = new BlobbyPrimitive((int)dim.x,(int)dim.y,(int)dim.z,size);
	for (int i=0; i<count; i++)
	{
		Prim->AddInfluence(dVector(0,0,0),0);
	}
	
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(Prim));
}

// StartFunctionDoc-en
// blobby->poly blobbyprimitiveid-number
// Returns: polyprimid-number
// Description:
// Converts the mesh of a blobby primitive into a triangle list polygon primitive. This is useful as
// the polygon primitive will be much much faster to render, but can't deform in the blobby way.
// Doesn't convert vertex colours over yet unfortunately.
// Example:
// (clear)
// (define b (build-blobby 5 (vector 30 30 30) (vector 1 1 1)))
// 
// (with-primitive b
//     (shinyness 100)
//     (specular (vector 1 1 1))
//     (hint-vertcols)
//     (pdata-set "p" 0 (vector 0.75 0.25 0.5))
//     (pdata-set "c" 0 (vector 0.01 0 0))
//     (pdata-set "s" 0 0.01)
//     (pdata-set "p" 1 (vector 0.25 0.75 0.5))
//     (pdata-set "c" 1 (vector 0 0.01 0))
//     (pdata-set "s" 1 0.01)
//     (pdata-set "p" 2 (vector 0.75 0.75 0.5))
//     (pdata-set "c" 2 (vector 0 0 0.01))
//     (pdata-set "s" 2 0.01)
//     (pdata-set "p" 3 (vector 0.25 0.25 0.5))
//     (pdata-set "c" 3 (vector 0.01 0.01 0))
//     (pdata-set "s" 3 0.01)
//     (pdata-set "p" 4 (vector 0.5 0.5 0.5))
//     (pdata-set "c" 4 (vector 0.01 0.01 0.01))
//     (pdata-set "s" 4 0.025))
// 
// (define p (with-state
//     (translate (vector 1 0 0))
//     (blobby->poly b)))
// EndFunctionDoc

// StartFunctionDoc-pt
// blobby->poly número-id-blobbyprimitiva
// Retorna: número-id-primitivapoly
// Descrição:
// Converte a malha de uma primitiva blobby em uma primitiva poligonal
// de lista de triângulos.
// Exemplo:
// (clear)
// (define b (build-blobby 5 (vector 30 30 30) (vector 1 1 1)))
// 
// (with-primitive b
//     (shinyness 100)
//     (specular (vector 1 1 1))
//     (hint-vertcols)
//     (pdata-set "p" 0 (vector 0.75 0.25 0.5))
//     (pdata-set "c" 0 (vector 0.01 0 0))
//     (pdata-set "s" 0 0.01)
//     (pdata-set "p" 1 (vector 0.25 0.75 0.5))
//     (pdata-set "c" 1 (vector 0 0.01 0))
//     (pdata-set "s" 1 0.01)
//     (pdata-set "p" 2 (vector 0.75 0.75 0.5))
//     (pdata-set "c" 2 (vector 0 0 0.01))
//     (pdata-set "s" 2 0.01)
//     (pdata-set "p" 3 (vector 0.25 0.25 0.5))
//     (pdata-set "c" 3 (vector 0.01 0.01 0))
//     (pdata-set "s" 3 0.01)
//     (pdata-set "p" 4 (vector 0.5 0.5 0.5))
//     (pdata-set "c" 4 (vector 0.01 0.01 0.01))
//     (pdata-set "s" 4 0.025))
// 
// (define p (with-state
//     (translate (vector 1 0 0))
//     (blobby->poly b)))
// EndFunctionDoc

Scheme_Object *blobby2poly(int argc, Scheme_Object **argv)
{		
	DECL_ARGV();
	ArgCheck("blobby->poly", "i", argc, argv);
	Primitive *Prim=Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]));
	if (Prim) 
	{
		// only if this is a pixel primitive
		BlobbyPrimitive *bp = dynamic_cast<BlobbyPrimitive *>(Prim);
		if (bp)
		{
			PolyPrimitive *np = new PolyPrimitive(PolyPrimitive::TRILIST);
			bp->ConvertToPoly(*np);
			MZ_GC_UNREG();
    		return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(np));
		}
	}
	
	Trace::Stream<<"blobby->poly can only be called on a blobbyprimitive"<<endl;
	MZ_GC_UNREG(); 
    return scheme_void;
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

// StartFunctionDoc-pt
// draw-instance número-id-primitiva
// Retorna: void
// Descrição:
// Copia um modo retido da primitiva e desenha ela no estado corrente
// como um no modo imediato.
// Exemplo:
// (define mynewshape (build-cube))
// (colour (vector 1 0 0))
// (draw-instance mynewshape) ; draws a copy of mynewshape
// EndFunctionDoc

Scheme_Object *draw_instance(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("draw-instance", "i", argc, argv);
	Primitive *p = Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]));
	if (p)
	{
		Engine::Get()->Renderer()->RenderPrimitive(p);
	}
	else
	{
		Trace::Stream<<"draw-instance can only be called with an existing object id"<<endl;
	}

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

// StartFunctionDoc-pt
// draw-cube
// Retorna: void
// Descrição:
// Desenha um cubo no estado imediato corrente.
// Exemplo:
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

// StartFunctionDoc-pt
// draw-plane
// Retorna: void
// Descrição:
// Desenha um plano no estado corrente em modo imediato
// Exemplo:
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

// StartFunctionDoc-pt
// draw-sphere
// Retorna: void
// Descrição:
// Desenha uma esfera no estado corrente em modo imediato.
// Exemplo:
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

// StartFunctionDoc-pt
// draw-cylinder
// Retorna: void
// Descrição:
// Desenha um cilindro no estado corrente em modo imediato.
// Exemplo:
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
// draw-torus 
// Returns: void
// Description:
// Draws a torus in the current state in immediate mode
// primitive.
// Example:
// (define (render)
//     (draw-torus))
// (every-frame (render))
// EndFunctionDoc

// StartFunctionDoc-pt
// draw-torus
// Retorna: void
// Descrição:
// Desenha um torus no estado imediato corrente.
// Exemplo:
// (define (render)
//     (draw-torus))
// (every-frame (render))
// EndFunctionDoc

Scheme_Object *draw_torus(int argc, Scheme_Object **argv)
{
    Engine::Get()->Renderer()->RenderPrimitive(Engine::StaticTorus);
    return scheme_void;
}

// StartFunctionDoc-en
// draw-line point-vector point-vector
// Returns: void
// Description:
// Draws a line in the current state in immediate mode.
// primitive.
// Example:
// (define (render)
//     (draw-line (vector 0 0 0) (vector 2 1 0)))
// (every-frame (render))
// EndFunctionDoc

Scheme_Object *draw_line(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("draw-line", "vv", argc, argv);
	dVector p0, p1;
	FloatsFromScheme(argv[0], p0.arr(), 3);
	FloatsFromScheme(argv[1], p1.arr(), 3);
	RibbonPrimitive *p = new RibbonPrimitive();
	p->Resize(2);
	p->SetData<dVector>("p", 0, p0);
	p->SetData<dVector>("p", 1, p1);
	// force a hint-wire state
	Engine::Get()->PushGrab(0);
	Engine::Get()->Renderer()->PushState();
	Engine::Get()->State()->Hints |= HINT_WIRE;
	Engine::Get()->Renderer()->RenderPrimitive(p, true);
	Engine::Get()->PopGrab();
	Engine::Get()->Renderer()->PopState();
	MZ_GC_UNREG();
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

// StartFunctionDoc-pt
// destroy número-id-primitive
// Retorna: void
// Descrição:
// Deleta uma primitiva construída do renderizador.
// Exemplo:
// (define mynewshape (build-sphere 10 10))
// (destroy mynewshape)
// EndFunctionDoc

Scheme_Object *destroy(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("destroy", "i", argc, argv);
	int name=0;
	name=IntFromScheme(argv[0]);
	Engine::Get()->Physics()->Free(name);
	Engine::Get()->Renderer()->RemovePrimitive(name);
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// primitive-type-name
// Returns: void
// Description:
// Returns the name of the primtive, as a string.
// Example:
// (define p (build-cube))
// (display (with-primitive p (primitive-type-name)))(newline)
// EndFunctionDoc

Scheme_Object *primitive_type_name(int argc, Scheme_Object **argv)
{	
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();
	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed) 
	{
        ret = scheme_make_utf8_string(Grabbed->GetTypeName().c_str());
        MZ_GC_UNREG(); 
        return ret;
	}
    Trace::Stream<<"primitive-type-name called without a current primitive"<<endl;
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// poly-indices
// Returns: void
// Description:
// Gets the vertex indices from this primitive.
// Example:
// (define p (build-cube))
//
// (with-primitive p
//     (poly-convert-to-indexed)
//     (display (poly-indices))(newline))
// EndFunctionDoc

Scheme_Object *poly_indices(int argc, Scheme_Object **argv)
{
	Scheme_Object *l = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, l);
	MZ_GC_REG();

	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		// only if this is a poly primitive
		PolyPrimitive *pp = dynamic_cast<PolyPrimitive *>(Grabbed);
		if (pp)
		{
			l = scheme_null;

			for (int n=(int)pp->GetIndex().size()-1; n>=0; n--)
			{
				l=scheme_make_pair(scheme_make_integer(pp->GetIndex()[n]),l);
			}
			MZ_GC_UNREG();
		    return l;
		}
	}

	Trace::Stream<<"poly-indices can only be called while a polyprimitive is grabbed"<<endl;
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// poly-type-enum
// Returns: void
// Description:
// Returns the enum value representing the type of the current polygon primitive.
// This is needed as I can't get my scheme scripts to recognise symbols returned from here.
// Use (poly-type) instead of this directly.
// primitive.
// Example:
// (define (poly-type)	
//   (let ((t (poly-type-enum)))
//     (cond
//       ((eq? t 0) 'triangle-strip)
//       ((eq? t 1) 'quad-list)
//       ((eq? t 2) 'triangle-list)
//       ((eq? t 3) 'triangle-fan)
//       ((eq? t 4) 'polygon))))
// EndFunctionDoc

Scheme_Object *poly_type_enum(int argc, Scheme_Object **argv)
{	
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();
	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed) 
	{
		// only if this is a pixel primitive
		PolyPrimitive *pp = dynamic_cast<PolyPrimitive *>(Grabbed);
		if (pp)
		{
			ret = scheme_make_integer_value((int)pp->GetType());
			MZ_GC_UNREG(); 
		    return ret;
		}
	}
	
	Trace::Stream<<"poly-type can only be called while a polyprimitive is grabbed"<<endl;
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// poly-indexed?
// Returns: void
// Description:
// Returns true if the current polygon primitive is in indexed mode.
// primitive.
// Example:
// (define p (build-polygons 3 'triangle-strip))
// (with-primitive p
//     (poly-convert-to-indexed)
//     (display (poly-indexed?))(newline))
// EndFunctionDoc

Scheme_Object *poly_indexed(int argc, Scheme_Object **argv)
{	
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();
	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed) 
	{
		// only if this is a pixel primitive
		PolyPrimitive *pp = dynamic_cast<PolyPrimitive *>(Grabbed);
		if (pp)
		{
			ret=scheme_false;
			if (pp->IsIndexed()) ret=scheme_true;
			MZ_GC_UNREG(); 
		    return ret;
		}
	}
	
	Trace::Stream<<"poly-type can only be called while a polyprimitive is grabbed"<<endl;
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// poly-set-index index-list
// Returns: void
// Description:
// Switches the primitive to indexed mode, and uses the 
// list as the index values for this primitive.
// primitive.
// Example:
// (clear)
// ; lets build our own cube primitive...
// (define p (build-polygons 8 'quad-list))
// 
// (with-primitive p
//     ; setup the vertex data
//     (pdata-set "p" 0 (vector -1 -1 -1))
//     (pdata-set "p" 1 (vector  1 -1 -1))
//     (pdata-set "p" 2 (vector  1 -1  1))
//     (pdata-set "p" 3 (vector -1 -1  1))
//     (pdata-set "p" 4 (vector -1  1 -1))
//     (pdata-set "p" 5 (vector  1  1 -1))
//     (pdata-set "p" 6 (vector  1  1  1))
//     (pdata-set "p" 7 (vector -1  1  1))
//     (pdata-set "c" 0 (vector  0  0  0))
//     (pdata-set "c" 1 (vector  0  0  1))
//     (pdata-set "c" 2 (vector  0  1  0))
//     (pdata-set "c" 3 (vector  0  1  1))
//     (pdata-set "c" 4 (vector  1  0  0))
//     (pdata-set "c" 5 (vector  1  0  1))
//     (pdata-set "c" 6 (vector  1  1  0))
//     (pdata-set "c" 7 (vector  1  1  1))
//     
//     (hint-wire)
//     (hint-unlit)
//     (hint-vertcols)
//     
//     ; connect the verts together into faces
//     (poly-set-index (list 7 6 5 4  5 6 2 1 
//             4 5 1 0  1 2 3 0
//             3 7 4 0  6 7 3 2)))
// EndFunctionDoc

// StartFunctionDoc-pt
// poly-set-index lista-indexada
// Retorna: void
// Descrição:
// Troca a primitiva por modo indexado,e usa a lista como valores de
// index para essa primitiva.
// Exemplo:
// (clear)
// ; lets build our own cube primitive...
// (define p (build-polygons 8 'quad-list))
// 
// (with-primitive p
//     ; setup the vertex data
//     (pdata-set "p" 0 (vector -1 -1 -1))
//     (pdata-set "p" 1 (vector  1 -1 -1))
//     (pdata-set "p" 2 (vector  1 -1  1))
//     (pdata-set "p" 3 (vector -1 -1  1))
//     (pdata-set "p" 4 (vector -1  1 -1))
//     (pdata-set "p" 5 (vector  1  1 -1))
//     (pdata-set "p" 6 (vector  1  1  1))
//     (pdata-set "p" 7 (vector -1  1  1))
//     (pdata-set "c" 0 (vector  0  0  0))
//     (pdata-set "c" 1 (vector  0  0  1))
//     (pdata-set "c" 2 (vector  0  1  0))
//     (pdata-set "c" 3 (vector  0  1  1))
//     (pdata-set "c" 4 (vector  1  0  0))
//     (pdata-set "c" 5 (vector  1  0  1))
//     (pdata-set "c" 6 (vector  1  1  0))
//     (pdata-set "c" 7 (vector  1  1  1))
//     
//     (hint-wire)
//     (hint-unlit)
//     (hint-vertcols)
//     
//     ; connect the verts together into faces
//     (poly-set-index (list 7 6 5 4  5 6 2 1 
//             4 5 1 0  1 2 3 0
//             3 7 4 0  6 7 3 2)))
// EndFunctionDoc

Scheme_Object *poly_set_index(int argc, Scheme_Object **argv)
{	
	Scheme_Object *indexvec = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, indexvec);
	MZ_GC_REG();

	ArgCheck("poly-set-index", "l", argc, argv);
	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed) 
	{
		// only if this is a pixel primitive
		PolyPrimitive *pp = dynamic_cast<PolyPrimitive *>(Grabbed);
		if (pp)
		{
			indexvec = scheme_list_to_vector(argv[0]);
			pp->GetIndex().resize(SCHEME_VEC_SIZE(indexvec));
			for (int n=0; n<SCHEME_VEC_SIZE(indexvec); n++)
			{
				if (SCHEME_EXACT_INTEGERP(SCHEME_VEC_ELS(indexvec)[n])) 
				{					
					pp->GetIndex()[n]=IntFromScheme(SCHEME_VEC_ELS(indexvec)[n]);
				}
			}
			pp->SetIndexMode(true);
			MZ_GC_UNREG(); 
		    return scheme_void;
		}
	}
	
	Trace::Stream<<"poly-set-index! can only be called while a polyprimitive is grabbed"<<endl;
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// poly-convert-to-indexed
// Returns: void
// Description:
// Converts the currently grabbed polygon primitive from
// raw vertex arrays to indexed arrays. This removes duplicate
// vertices from the polygon, making the pdata arrays shorter, 
// which speeds up processing time.
// Example:
// (define mynewshape (build-sphere 10 10))
// (grab mynewshape)
// (poly-convert-to-indexed)
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-pt
// poly-convert-to-indexed
// Retorna: void
// Descrição:
// Converte o atual poligono primitivo que está "grabbed" de arrays de
// vértices crus para arrays indexadas. Isto remove vértices
// duplicados do polígono, fazendo a array de pdata menor, o que
// aumenta a velocidade do processo.
// Exemplo:
// (define mynewshape (build-sphere 10 10))
// (grab mynewshape)
// (poly-convert-to-indexed)
// (ungrab)
// EndFunctionDoc

Scheme_Object *poly_convert_to_indexed(int argc, Scheme_Object **argv)
{		
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed) 
	{
		// only if this is a pixel primitive
		PolyPrimitive *pp = dynamic_cast<PolyPrimitive *>(Grabbed);
		if (pp)
		{
			pp->ConvertToIndexed();
		    return scheme_void;
		}
	}
	
	Trace::Stream<<"poly-convert-to-indexed can only be called while a polyprimitive is grabbed"<<endl;
    return scheme_void;
}

// StartFunctionDoc-en
// build-copy src-primitive-number
// Returns: primitiveid-number
// Description:
// Returns a copy of a primitive
// Example:
// (define mynewshape (build-sphere 10 10))
// (define myothernewshape (build-copy mynewshape))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-copy número-primitiva-fonte
// Retorna: número-id-primitiva
// Descrição:
// Retorna uma cópia da primitiva
// Exemplo:
// (define mynewshape (build-sphere 10 10))
// (define myothernewshape (build-copy mynewshape))
// EndFunctionDoc
	
Scheme_Object *build_copy(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-copy", "i", argc, argv);
	Primitive *src = Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]));
	if (src==NULL)
	{
		MZ_GC_UNREG(); 
		return scheme_make_integer_value(0);
	}
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(src->Clone()));
}

// StartFunctionDoc-en
// make-pfunc name-string
// Returns: pfuncid-number
// Description:
// Makes a new primitive function. pfuncs range from general purpose to complex and specialised operations 
// which you can run on primitives. All pfuncs share the same interface for controlling and setting them up - 
// pfunc-set! All pfunc types and arguments are as follows:
//
// arithmetic 
//     For applying general arithmetic to any pdata array
//
//     operator string : one of add sub mul div
//     src string : pdata array name
//     other string : pdata array name (optional)
//     constant float : constant value (optional)
//     dst string : pdata array name
//
// genskinweights 
//     Generates skinweights - adds float pdata called "s1" -> "sn" 
//     where n is the number of nodes in the skeleton - 1 
//
//     skeleton-root primid-number : the root of the bindpose skeleton for skinning
//     sharpness float : a control of how sharp the creasing will be when skinned 
//
// skinweights->vertcols
//     A utility for visualising skinweights for debugging. 
//     no arguments
//
// skinning 
//     Skins a primitive - deforms it to follow a skeleton's movements. Primitives we want to run
//     this on have to contain extra pdata - copies of the starting vert positions called "pref" and
//     the same for normals, if normals are being skinned, called "nref". 
//    
//     skeleton-root primid-number : the root primitive of the animating skeleton
//     bindpose-root primid-number : the root primitive of the bindpose skeleton
//     skin-normals number : whether to skin the normals as well as the positions
//     
// Example:
// (define mypfunc (make-pfunc 'arithmetic))
// EndFunctionDoc

// StartFunctionDoc-pt
// make-pfunc string-nome
// Retorna: número-pfuncid
// Descrição:
// Faz uma nova primitiva funcional. Pfuncs variam de propósito geral
// até operações complexas e especializadas que você pode rodar em
// primitivas. Todas as pfuncs dividem a mesma interface para
// controlar e ajustar - pfunc-set! Todos os tipos e argumentos pfunc
// são como os seguintes:
//
// arithmetic 
//     Para aplicar aritmética geral para qualquer array pdata.
//
//     operator string : um dos add sub mul div
//     src string : nome pdata array 
//     other string : nome pdata array (opcional)
//     constant float : valor constante (opcional)
//     dst string : nome pdata array 
//
// genskinweights 
//     Gera pesos de skinning - adiciona float pdata chamada "s1" -> "sn" 
//     aonde n é o número de nós no esqueleto - 1 
//
//     skeleton-root primid-number : a raiz da posebind do esqueleto para skinning
//     sharpness float : um controle de quão afiado o vinco vai ser
//     quando "skineado". 
//
// skinweights->vertcols
//     Uma utilidade para visualizar pesos de skin para debugar. 
//     sem argumentos.
//
// skinning 
//     Skin uma primitiva - deforma ela para seguir os movimentos de
//     um esqueleto. Primitivas que a gente quer usar isto deve conter
//     extra pdata - cópias das posições iniciais das posições dos
//     vértices chamadas "pref" e o mesmo para normais, se as normais
//     estão sendo skineadas , chamada "nref". 
//    
//     skeleton-root primid-number : a primitiva raiz do esqueleto animado
//     bindpose-root primid-number : a raiz primitiva da pose bind do esqueleto
//     skin-normals number : se devemos usar skin nas normais como nas posições
// 
// Exemplo:
// (define mypfunc (make-pfunc 'arithmetic))
// EndFunctionDoc
	
Scheme_Object *make_pfunc(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("make-pfunc", "S", argc, argv);
	int id = Engine::Get()->GetPFuncContainer()->Make(SymbolName(argv[0]));
	MZ_GC_UNREG(); 
    return scheme_make_integer_value(id);
}

// StartFunctionDoc-en
// pfunc-set! pfuncid-number argument-list
// Returns: void
// Description:
// Sets arguments on a primitive function. See docs for make-pfunc for all the arguments.
// Example:
// (define mypfunc (make-pfunc 'arithmetic))
// (pfunc-set! mypfunc (list 'operator "add"
//                           'src "p"
//                           'const 0.4
//                           'dst "p"))
// EndFunctionDoc

// StartFunctionDoc-pt
// pfunc-set! número-id-pfunc lista-argumento
// Retorna: void
// Descrição:
// Ajusta argumentos na função primitiva. Veja a documentação de
// make-pfunc para todos os argumentos.
// Exemplo:
// (define mypfunc (make-pfunc 'arithmetic))
// (pfunc-set! mypfunc (list 'operator "add"
//                           'src "p"
//                           'const 0.4
//                           'dst "p"))
// EndFunctionDoc
	
Scheme_Object *pfunc_set(int argc, Scheme_Object **argv)
{
	Scheme_Object *paramvec = NULL;
	Scheme_Object *innerlist = NULL;
	MZ_GC_DECL_REG(3);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, paramvec);
	MZ_GC_VAR_IN_REG(2, innerlist);
	MZ_GC_REG();
			
   	ArgCheck("pfunc-set!", "il", argc, argv);

	// vectors seem easier to handle than lists with this api
	paramvec = scheme_list_to_vector(argv[1]);
	int id = IntFromScheme(argv[0]);
	for (int n=0; n<SCHEME_VEC_SIZE(paramvec); n+=2)
	{
		if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(paramvec)[n]))
		{
			// get the parameter name
			string param = SymbolName(SCHEME_VEC_ELS(paramvec)[n]);
	
			if (SCHEME_CHAR_STRINGP(SCHEME_VEC_ELS(paramvec)[n+1]))
			{
				Engine::Get()->GetPFuncContainer()->SetArg<string>(id,param,StringFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]));
			}
			else if (SCHEME_NUMBERP(SCHEME_VEC_ELS(paramvec)[n+1]))
			{
				if (SCHEME_EXACT_INTEGERP(SCHEME_VEC_ELS(paramvec)[n+1])) 
				{
					Engine::Get()->GetPFuncContainer()->SetArg<int>(id,param,IntFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]));
				}
				else 
				{
					Engine::Get()->GetPFuncContainer()->SetArg<float>(id,param,FloatFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]));
				}
			}
			else if (SCHEME_VECTORP(SCHEME_VEC_ELS(paramvec)[n+1]))
			{
				if (SCHEME_VEC_SIZE(SCHEME_VEC_ELS(paramvec)[n+1]) == 3)
				{
					dVector vec;
					FloatsFromScheme(SCHEME_VEC_ELS(paramvec)[n+1],vec.arr(),3);
					Engine::Get()->GetPFuncContainer()->SetArg<dVector>(id,param,vec);
				}
				else if (SCHEME_VEC_SIZE(SCHEME_VEC_ELS(paramvec)[n+1]) == 4)
				{
					dColour vec;
					FloatsFromScheme(SCHEME_VEC_ELS(paramvec)[n+1],vec.arr(),4);
					Engine::Get()->GetPFuncContainer()->SetArg<dColour>(id,param,vec);
				}
				else
				{	
					Trace::Stream<<"pfunc-set! has found a strange sized vector"<<endl;
				}
			}
			else if (SCHEME_LISTP(SCHEME_VEC_ELS(paramvec)[n+1]))
			{
				innerlist = scheme_list_to_vector(SCHEME_VEC_ELS(paramvec)[n+1]);
				if (SCHEME_NUMBERP(SCHEME_VEC_ELS(innerlist)[0])) 
				{
					if (SCHEME_EXACT_INTEGERP(SCHEME_VEC_ELS(innerlist)[0])) 
					{
						Engine::Get()->GetPFuncContainer()->SetArg<vector<int> >(id,param,
							IntVectorFromScheme(innerlist));
					}
					else
					{
						Engine::Get()->GetPFuncContainer()->SetArg<vector<float> >(id,param,
							FloatVectorFromScheme(innerlist));
					}
				}
				else
				{	
					Trace::Stream<<"pfunc-set! only takes lists of numbers"<<endl;
				}
				
			}
			else
			{
				Trace::Stream<<"pfunc-set! has found an argument type it can't send, numbers, vectors or lists only"<<endl;
			}
		}
		else
		{
			Trace::Stream<<"pfunc-set! has found a mal-formed parameter list"<<endl;
		}
	}
	
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// pfunc-run id-number
// Returns: void
// Description:
// Runs a primitive function on the currently grabbed primitive.
// Example:
// (define mypfunc (make-pfunc 'arithmetic))
// EndFunctionDoc


// StartFunctionDoc-pt
// pfunc-run número-id
// Retorna: void
// Descrição:
// Roda uma função primitiva na primitiva atualmente pega.
// Exemplo:
// (define mypfunc (make-pfunc 'arithmetic))
// EndFunctionDoc
	
Scheme_Object *pfunc_run(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("pfunc-run", "i", argc, argv);
	if (Engine::Get()->Grabbed()) 
	{
		Engine::Get()->GetPFuncContainer()->Run(IntFromScheme(argv[0]),
						Engine::Get()->Grabbed(),
						&Engine::Get()->Renderer()->GetSceneGraph());
	}
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// geo/line-intersect start-vec end-vec
// Returns: void
// Description:
// Returns a list of pdata values at each intersection point of 
// the specified line. The line is in primitive local space, to 
// check with a point in global space, you need to transform the 
// point with the inverse of the primitive transform.
// Example:
// (clear)
// (define s (with-state
//         (build-torus 1 2 10 10)))
// 
// (define l (with-state
//         (hint-none)
//         (hint-unlit)
//         (hint-wire)
//         (build-line 2)))
// 
// (define (check a b)
//     (with-primitive s
//         (for-each
//             (lambda (intersection)                    
//                 (with-state ; draw a sphere at the intersection point
//                     (translate (cdr (assoc "p" intersection)))
//                     (colour (vector 0 1 0))
//                     (scale (vector 0.3 0.3 0.3))
//                     (draw-sphere)))
//         (geo/line-intersect a b))))
// 
// (every-frame
//     (with-primitive l
//         (pdata-set "p" 0 (vector 0 -5 0))
//         (pdata-set "p" 1 (vector (* 5 (sin (time))) 5 0))
//         (check (pdata-ref "p" 0) (pdata-ref "p" 1))))
// EndFunctionDoc


// StartFunctionDoc-pt
// geo/line-intersect número-id
// Retorna: void
// Descrição:
// Roda uma função primitiva na primitiva atualmente pega.
// Exemplo:
// (clear)
// (define s (with-state
//         (build-torus 1 2 10 10)))
// 
// (define l (with-state
//         (hint-none)
//         (hint-unlit)
//         (hint-wire)
//         (build-line 2)))
// 
// (define (check a b)
//     (with-primitive s
//         (for-each
//             (lambda (intersection)                    
//                 (with-state ; draw a sphere at the intersection point
//                     (translate (cdr (assoc "p" intersection)))
//                     (colour (vector 0 1 0))
//                     (scale (vector 0.3 0.3 0.3))
//                     (draw-sphere)))
//         (geo/line-intersect a b))))
// 
// (every-frame
//     (with-primitive l
//         (pdata-set "p" 0 (vector 0 -5 0))
//         (pdata-set "p" 1 (vector (* 5 (sin (time))) 5 0))
//         (check (pdata-ref "p" 0) (pdata-ref "p" 1))))
// EndFunctionDoc

Scheme_Object *geo_line_intersect(int argc, Scheme_Object **argv)
{
	Scheme_Object *name = NULL;
	Scheme_Object *value = NULL;
	Scheme_Object *p = NULL;
	Scheme_Object *l = NULL;
	Scheme_Object *pl = NULL;

	MZ_GC_DECL_REG(6);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, name);
	MZ_GC_VAR_IN_REG(2, value);
	MZ_GC_VAR_IN_REG(3, p);
	MZ_GC_VAR_IN_REG(4, l);
	MZ_GC_VAR_IN_REG(5, pl);
	MZ_GC_REG();
	ArgCheck("geo/line-intersect", "vv", argc, argv);
	
	l = scheme_null;
	
	if (Engine::Get()->Grabbed()) 
	{
		Evaluator *eval = Engine::Get()->Grabbed()->MakeEvaluator();
		if (eval)
		{
			vector<Evaluator::Point> points;
			eval->IntersectLine(VectorFromScheme(argv[0]), VectorFromScheme(argv[1]), points);

			for (vector<Evaluator::Point>::iterator i=points.begin(); i!=points.end(); ++i)
			{
				pl = scheme_null;
                // jam the parametric position on the ray to the end of the list
                // (so as not to break compatibility :/)
				pl = scheme_make_pair(scheme_make_double(i->m_T),pl);
				for (vector<Evaluator::Blend*>::iterator b=i->m_Blends.begin(); b!=i->m_Blends.end(); ++b)
				{
					name = scheme_make_utf8_string((*b)->m_Name.c_str());
					
					switch((*b)->m_Type)
					{
						case 'f': value = scheme_make_double(static_cast<Evaluator::TypedBlend<float>*>(*b)->m_Blend); break;
						case 'v': value = FloatsToScheme(static_cast<Evaluator::TypedBlend<dVector>*>(*b)->m_Blend.arr(),4); break;
						case 'c': value = FloatsToScheme(static_cast<Evaluator::TypedBlend<dColour>*>(*b)->m_Blend.arr(),4); break;
						case 'm': value = FloatsToScheme(static_cast<Evaluator::TypedBlend<dMatrix>*>(*b)->m_Blend.arr(),16); break;
						default: assert(0); break;
					}

					p = scheme_make_pair(name,value);					
					pl = scheme_make_pair(p,pl);
				}
				l = scheme_make_pair(pl,l);
			}

			delete eval;
		}
	}
	MZ_GC_UNREG(); 
    return l;
}

// StartFunctionDoc-en
// recalc-bb
// Returns: void
// Description:
// This call regenerates the primitives bounding box. 
// As this call can be expensive, it's up to you when to call it - only if the pdata 
// has changed and just before operations that use the bounding box 
// (such as bb/bb-intersect) is the fastest policy.
// Example:
// (define myprim (build-cube))
// (with-primitive myprim
//   (hint-box)
//   (pdata-set! "p" 0 (vector -10 0 0))
//   (recalc-bb))
// EndFunctionDoc

Scheme_Object *recalc_bb(int argc, Scheme_Object **argv)
{
	if (Engine::Get()->Grabbed()) 
	{
		SceneNode *node=(SceneNode*)(Engine::Get()->Renderer()->GetSceneGraph().FindNode(Engine::Get()->GrabbedID()));
		if (node) Engine::Get()->Renderer()->GetSceneGraph().RecalcAABB(node);
	}
	return scheme_void;
}


// StartFunctionDoc-en
// bb/bb-intersect? prim thresh
// Returns: void
// Description:
// Returns #t if the current primitive bounding box intersects with the supplied one, 
// with an additional expanding threshold.
// Example:
// (clear)
// 
// (define a (with-state
//      (build-sphere 10 10)))
// 
// (define b (with-state
//      (translate (vector 2 0 0))
//      (build-sphere 10 10)))
// 
// (every-frame
//     (begin
//         (with-primitive b
//             (translate (vector (* -0.1 (sin (time))) 0 0))
//             (recalc-bb))
//         (with-primitive a
//             (when (bb/bb-intersect? b 0)
//                 (colour (rndvec))))))
// EndFunctionDoc

Scheme_Object *bb_bb_intersect(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("bb-intersect?", "if", argc, argv);
	if (Engine::Get()->Grabbed()) 
	{
		SceneNode *a=(SceneNode*)(Engine::Get()->Renderer()->GetSceneGraph().FindNode(Engine::Get()->GrabbedID()));
		SceneNode *b=(SceneNode*)(Engine::Get()->Renderer()->GetSceneGraph().FindNode(IntFromScheme(argv[0])));
		if (a && b)
		{			
			if (Engine::Get()->Renderer()->GetSceneGraph().Intersect(a,b,FloatFromScheme(argv[1])))
			{
				MZ_GC_UNREG(); 
				return scheme_true;
			}
		}
	}
 	MZ_GC_UNREG(); 
	return scheme_false;
}

// StartFunctionDoc-en
// bb/point-intersect? point thresh
// Returns: void
// Description:
// Returns #t if the current primitive bounding box intersects with point supplied, 
// with an additional expanding threshold (so you can do check intersections with spheres). 
// The point is in world space. You need to 
// call (recalc-bb) before using this function, if the primitive has been moved or
// had it's pdata changed.
// Example:
// (clear)
// 
// (define a (with-state
//      (build-sphere 10 10)))
// 
// (define b (with-state
//      (translate (vector 2 0 0))
//      (build-sphere 10 10)))
// 
// (every-frame
//     (begin
//         (with-primitive b
//             (translate (vector (* -0.1 (sin (time))) 0 0))
//			   (recalc-bb))
//         (with-primitive a
//             ; check the centre point and give the radius, this sphere
//             ; check is faster than a bb/bb one
//             (when (bb/point-intersect? (vtransform (vector 0 0 0)
//                     (with-primitive b (get-transform))) 1)
//                 (colour (rndvec))))))
// EndFunctionDoc

Scheme_Object *bb_point_intersect(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("bb/point-intersect?", "vf", argc, argv);
	if (Engine::Get()->Grabbed()) 
	{
		SceneNode *node=(SceneNode*)(Engine::Get()->Renderer()->GetSceneGraph().FindNode(Engine::Get()->GrabbedID()));
		if (node)
		{			
			if (Engine::Get()->Renderer()->GetSceneGraph().Intersect
				(VectorFromScheme(argv[0]),node,FloatFromScheme(argv[1])))
			{
				MZ_GC_UNREG(); 
				return scheme_true;
			}
		}
	}
 	MZ_GC_UNREG(); 
	return scheme_false;
}

// StartFunctionDoc-en
// get-children 
// Returns: void
// Description:
// Gets a list of primitives parented to this one.
// Example:
// ; build a random heirachical structure
// (define (build-heir depth)
//     (with-state
//         (let ((p (with-state
//                         (translate (vector 2 0 0))
//                         (scale 0.9)
//                         (build-cube))))
//             (when (> depth 0)
//                 (parent p)            
//                 (for ((i (in-range 0 5)))
//                     (when (zero? (random 3))
//                         (rotate (vector 0 0 (* 45 (crndf))))
//                         (build-heir (- depth 1))))))))
// 
// ; navigate the scene graph and print it out
// (define (print-heir children)
//     (for-each
//         (lambda (child)
//             (with-primitive child
//                 (printf "id: ~a parent: ~a children: ~a~n" child (get-parent) (get-children))
//                 (print-heir (get-children))))
//         children))
// 
// (clear)
// (build-heir 5)
// (print-heir (get-children))
// EndFunctionDoc

Scheme_Object *get_children(int argc, Scheme_Object **argv)
{	
	Scheme_Object *l = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, l);
	MZ_GC_REG();
	l = scheme_null;
	
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed) 
	{
		SceneNode *a=(SceneNode*)(Engine::Get()->Renderer()->GetSceneGraph().FindNode(Engine::Get()->GrabbedID()));
				
		for (vector<Node*>::iterator n=a->Children.begin(); 
			n<a->Children.end(); n++)
		{
			l=scheme_make_pair(scheme_make_integer((*n)->ID),l);
		}
	}
	else // return the root node's children
	{
		SceneNode *a=(SceneNode*)(Engine::Get()->Renderer()->GetSceneGraph().Root());
				
		for (vector<Node*>::iterator n=a->Children.begin(); 
			n<a->Children.end(); n++)
		{
			l=scheme_make_pair(scheme_make_integer((*n)->ID),l);
		}
	}
	
	MZ_GC_UNREG(); 
    return l;
}

// StartFunctionDoc-en
// get-parent 
// Returns: void
// Description:
// Gets the parent of this node. 1 is the root node.
// Example:
// ; build a random heirachical structure
// (define (build-heir depth)
//     (with-state
//         (let ((p (with-state
//                         (translate (vector 2 0 0))
//                         (scale 0.9)
//                         (build-cube))))
//             (when (> depth 0)
//                 (parent p)            
//                 (for ((i (in-range 0 5)))
//                     (when (zero? (random 3))
//                         (rotate (vector 0 0 (* 45 (crndf))))
//                         (build-heir (- depth 1))))))))
// 
// ; navigate the scene graph and print it out
// (define (print-heir children)
//     (for-each
//         (lambda (child)
//             (with-primitive child
//                 (printf "id: ~a parent: ~a children: ~a~n" child (get-parent) (get-children))
//                 (print-heir (get-children))))
//         children))
// 
// (clear)
// (build-heir 5)
// (print-heir (get-children))
// EndFunctionDoc

Scheme_Object *get_parent(int argc, Scheme_Object **argv)
{
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
		SceneNode *a=(SceneNode*)(Engine::Get()->Renderer()->GetSceneGraph().FindNode(Engine::Get()->GrabbedID()));
		return scheme_make_integer(a->Parent->ID);
	}
	Trace::Stream<<"get-parent: no primitive current"<<endl;
	return scheme_void;
}

// StartFunctionDoc-en
// get-bb
// Returns: bounding-box
// Description:
// Gets the bounding box this primitive in object space. A bounding box is
// returned as a list of two vectors, the minimum (closest to the origin)
// and maximum (furthest) corner.
// Example:
// (with-primitive (build-sphere 10 10)
//     (scale (vector 3 1 0.3))
//     (rotate (vector 30 25 45))
//     (apply-transform)
//     (display (get-bb))(newline))
// EndFunctionDoc

Scheme_Object *get_bb(int argc, Scheme_Object **argv)
{
	Scheme_Object *l = NULL;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, l);
	MZ_GC_REG();

	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed)
	{
        dMatrix space;
        space.init();
        dBoundingBox bb = Grabbed->GetBoundingBox(space);
        l = scheme_null;
		l=scheme_make_pair(FloatsToScheme(bb.min.arr(),3),l);
		l=scheme_make_pair(FloatsToScheme(bb.max.arr(),3),l);
		MZ_GC_UNREG();
		return l;
	}
	Trace::Stream<<"get-bb: no primitive current"<<endl;
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
	scheme_add_global("build-icosphere", scheme_make_prim_w_arity(build_icosphere, "build-icosphere", 1, 1), env);
	scheme_add_global("build-torus", scheme_make_prim_w_arity(build_torus, "build-torus", 4, 4), env);
	scheme_add_global("build-plane", scheme_make_prim_w_arity(build_plane, "build-plane", 0, 0), env);
	scheme_add_global("build-seg-plane", scheme_make_prim_w_arity(build_seg_plane, "build-seg-plane", 2, 2), env);
	scheme_add_global("build-cylinder", scheme_make_prim_w_arity(build_cylinder, "build-cylinder", 2, 2), env);
	scheme_add_global("build-ribbon", scheme_make_prim_w_arity(build_ribbon, "build-ribbon", 1, 1), env);
	scheme_add_global("build-text", scheme_make_prim_w_arity(build_text, "build-text", 1, 1), env);
	scheme_add_global("build-nurbs-sphere", scheme_make_prim_w_arity(build_nurbs_sphere, "build-nurbs-sphere", 2, 2), env);
	scheme_add_global("build-nurbs-plane", scheme_make_prim_w_arity(build_nurbs_plane, "build-nurbs-sphere", 2, 2), env);
	scheme_add_global("build-particles", scheme_make_prim_w_arity(build_particles, "build-particles", 1, 1), env);
	scheme_add_global("build-image", scheme_make_prim_w_arity(build_image, "build-image", 3, 3), env);
	scheme_add_global("build-locator", scheme_make_prim_w_arity(build_locator, "build-locator", 0, 0), env);
	scheme_add_global("build-voxels", scheme_make_prim_w_arity(build_voxels, "build-voxels", 3, 3), env);
	scheme_add_global("locator-bounding-radius", scheme_make_prim_w_arity(locator_bounding_radius, "locator-bounding-radius", 1, 1), env);
	scheme_add_global("build-pixels", scheme_make_prim_w_arity(build_pixels, "build-pixels", 2, 4), env);
	scheme_add_global("build-type", scheme_make_prim_w_arity(build_type, "build-type", 2, 2), env);
	scheme_add_global("build-extruded-type", scheme_make_prim_w_arity(build_extruded_type, "build-extruded-type", 3, 3), env);
	scheme_add_global("load-primitive", scheme_make_prim_w_arity(load_primitive, "load-primitive", 1, 1), env);
	scheme_add_global("save-primitive", scheme_make_prim_w_arity(save_primitive, "save-primitive", 1, 1), env);
	scheme_add_global("clear-geometry-cache", scheme_make_prim_w_arity(clear_geometry_cache, "clear-geometry-cache", 0, 0), env);
	scheme_add_global("pixels-upload", scheme_make_prim_w_arity(pixels_upload, "pixels-upload", 0, 0), env);
	scheme_add_global("pixels-download", scheme_make_prim_w_arity(pixels_download, "pixels-download", 0, 1), env);
	scheme_add_global("pixels-load", scheme_make_prim_w_arity(pixels_load, "pixels-load", 1, 1), env);
	scheme_add_global("pixels-width", scheme_make_prim_w_arity(pixels_width, "pixels-width", 0, 0), env);
	scheme_add_global("pixels-height", scheme_make_prim_w_arity(pixels_height, "pixels-height", 0, 0), env);
	scheme_add_global("pixels-render-to", scheme_make_prim_w_arity(pixels_render_to, "pixels-render-to", 1, 1), env);
	scheme_add_global("pixels-display", scheme_make_prim_w_arity(pixels_display, "pixels-display", 1, 1), env);
	scheme_add_global("pixels->texture", scheme_make_prim_w_arity(pixels2texture, "pixels->texture", 1, 2), env);
	scheme_add_global("pixels-renderer-activate", scheme_make_prim_w_arity(pixels_renderer_activate, "pixels-renderer-activate", 1, 1), env);
	scheme_add_global("voxels->blobby", scheme_make_prim_w_arity(voxels2blobby, "voxels->blobby", 1, 1), env);
	scheme_add_global("voxels->poly", scheme_make_prim_w_arity(voxels2poly, "voxels->poly", 1, 2), env);
	scheme_add_global("voxels-width", scheme_make_prim_w_arity(voxels_width, "voxels-width", 0, 0), env);
	scheme_add_global("voxels-height", scheme_make_prim_w_arity(voxels_height, "voxels-height", 0, 0), env);
	scheme_add_global("voxels-depth", scheme_make_prim_w_arity(voxels_depth, "voxels-depth", 0, 0), env);
	scheme_add_global("voxels-calc-gradient", scheme_make_prim_w_arity(voxels_calc_gradient, "voxels-calc-gradient", 0, 0), env);
	scheme_add_global("voxels-sphere-influence", scheme_make_prim_w_arity(voxels_sphere_influence, "voxels-sphere-influence", 3, 3), env);
	scheme_add_global("voxels-sphere-solid", scheme_make_prim_w_arity(voxels_sphere_solid, "voxels-sphere-solid", 3, 3), env);
	scheme_add_global("voxels-box-solid", scheme_make_prim_w_arity(voxels_box_solid, "voxels-box-solid", 3, 3), env);
	scheme_add_global("voxels-threshold", scheme_make_prim_w_arity(voxels_threshold, "voxels-threshold", 1, 1), env);
	scheme_add_global("voxels-point-light", scheme_make_prim_w_arity(voxels_point_light, "voxels-point-light", 2, 2), env);
	scheme_add_global("text-params", scheme_make_prim_w_arity(text_params, "text-params", 11, 11), env);
	scheme_add_global("ribbon-inverse-normals", scheme_make_prim_w_arity(ribbon_inverse_normals, "ribbon-inverse-normals", 1, 1), env);
	scheme_add_global("build-blobby", scheme_make_prim_w_arity(build_blobby, "build-blobby", 3, 3), env);
	scheme_add_global("blobby->poly", scheme_make_prim_w_arity(blobby2poly, "blobby->poly", 1, 1), env);
	scheme_add_global("type->poly", scheme_make_prim_w_arity(type2poly, "type->poly", 1, 1), env);
	scheme_add_global("draw-instance", scheme_make_prim_w_arity(draw_instance, "draw-instance", 1, 1), env);
	scheme_add_global("draw-cube", scheme_make_prim_w_arity(draw_cube, "draw-cube", 0, 0), env);
	scheme_add_global("draw-plane", scheme_make_prim_w_arity(draw_plane, "draw-plane", 0, 0), env);
	scheme_add_global("draw-sphere", scheme_make_prim_w_arity(draw_sphere, "draw-sphere", 0, 0), env);
	scheme_add_global("draw-cylinder", scheme_make_prim_w_arity(draw_cylinder, "draw-cylinder", 0, 0), env);
	scheme_add_global("draw-torus", scheme_make_prim_w_arity(draw_torus, "draw-torus", 0, 0), env);
	scheme_add_global("draw-line", scheme_make_prim_w_arity(draw_line, "draw-line", 2, 2), env);
	scheme_add_global("destroy", scheme_make_prim_w_arity(destroy, "destroy", 1, 1), env);
	scheme_add_global("primitive-type-name", scheme_make_prim_w_arity(primitive_type_name, "primitive-type-name", 0, 0), env);
	scheme_add_global("poly-set-index", scheme_make_prim_w_arity(poly_set_index, "poly-set-index", 1, 1), env);
	scheme_add_global("poly-indices", scheme_make_prim_w_arity(poly_indices, "poly-indices", 0, 0), env);
	scheme_add_global("poly-type-enum", scheme_make_prim_w_arity(poly_type_enum, "poly-type-enum", 0, 0), env);
	scheme_add_global("poly-indexed?", scheme_make_prim_w_arity(poly_indexed, "poly-indexed?", 0, 0), env);
	scheme_add_global("poly-convert-to-indexed", scheme_make_prim_w_arity(poly_convert_to_indexed, "poly-convert-to-indexed", 0, 0), env);
	scheme_add_global("build-copy", scheme_make_prim_w_arity(build_copy, "build-copy", 1, 1), env);
	scheme_add_global("make-pfunc", scheme_make_prim_w_arity(make_pfunc, "make-pfunc", 1, 1), env);
	scheme_add_global("pfunc-set!", scheme_make_prim_w_arity(pfunc_set, "pfunc-set!", 2, 2), env);
	scheme_add_global("pfunc-run", scheme_make_prim_w_arity(pfunc_run, "pfunc-run", 1, 1), env);
	scheme_add_global("geo/line-intersect", scheme_make_prim_w_arity(geo_line_intersect, "geo/line-intersect", 2, 2), env);
	scheme_add_global("recalc-bb", scheme_make_prim_w_arity(recalc_bb, "recalc-bb", 0, 0), env);
	scheme_add_global("bb/bb-intersect?", scheme_make_prim_w_arity(bb_bb_intersect, "bb/bb-intersect?", 2, 2), env);
	scheme_add_global("bb/point-intersect?", scheme_make_prim_w_arity(bb_point_intersect, "bb/point-intersect?", 2, 2), env);
	scheme_add_global("get-children", scheme_make_prim_w_arity(get_children, "get-children", 0, 0), env);
	scheme_add_global("get-parent", scheme_make_prim_w_arity(get_parent, "get-parent", 0, 0), env);
	scheme_add_global("get-bb", scheme_make_prim_w_arity(get_bb, "get-bb", 0, 0), env);
	MZ_GC_UNREG();
}

