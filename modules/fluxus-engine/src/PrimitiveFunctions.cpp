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
#include "PrimitiveIO.h"
#include "SearchPaths.h"
#include "Evaluator.h"

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
	NURBSPrimitive *Prim = new NURBSPrimitive();
	if (size<1) 
	{
		Trace::Stream<<"build-nurbs: size less than 1!"<<endl;
		MZ_GC_UNREG(); 
		return scheme_void;
	}
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
		Trace::Stream<<"build-nurbs: size less than 1!"<<endl;
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
		
	// 16*16 grid of letters
	TypePrimitive *TypePrim = new TypePrimitive();
	if (TypePrim->LoadTTF(StringFromScheme(argv[0])))
	{
		TypePrim->SetText(StringFromScheme(argv[1]));
	}
	MZ_GC_UNREG(); 
	
	return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(TypePrim));
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
		
	// 16*16 grid of letters
	TypePrimitive *TypePrim = new TypePrimitive();
	if(TypePrim->LoadTTF(StringFromScheme(argv[0])))
	{
		TypePrim->SetTextExtruded(StringFromScheme(argv[1]),FloatFromScheme(argv[2]));
	}
	MZ_GC_UNREG(); 
	
	return scheme_make_integer_value(Engine::Get()->Renderer()->AddPrimitive(TypePrim));
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
	ArgCheck("text=params", "sffffffffff", argc, argv);
	
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
// build-nurbs-sphere hsegments rsegments
// Returns: primitiveid-number
// Description:
// Builds a tesselated nurbs sphere, texture mapped in the same fashion as the poly sphere.
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
// build-nurbs-plane hsegments rsegments
// Returns: primitiveid-number
// Description:
// Builds a tesselated nurbs plane, texture mapped in uv direction.
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
	ArgCheck("locator-bounding-radius", "i", argc, argv);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed) 
	{
		// only if this is a pixel primitive
		LocatorPrimitive *lp = dynamic_cast<LocatorPrimitive *>(Grabbed);
		if (lp)
		{
			lp->SetBoundingBoxRadius(FloatFromScheme(argv[0]));
    		MZ_GC_UNREG();
		    return scheme_void;
		}
	}
	
	Trace::Stream<<"pixels-upload can only be called while a pixelprimitive is grabbed"<<endl;
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
	ArgCheck("load-primitive", "s", argc, argv);
	string filename=StringFromScheme(argv[0]);
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
// Saves the current primitive to disk
// Example:
// (with-primitive (build-sphere 10 10) 
//     (save-primitive "mymesh.obj"))
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
	string filename=StringFromScheme(argv[0]);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed) 
	{
		PrimitiveIO::Write(SearchPaths::Get()->GetFullPath(filename),Grabbed);
	}	    	
	MZ_GC_UNREG();
    return scheme_void;
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
// (with-primitive mynewshape
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "c")
//     (pixels-upload)) ; call pixels upload to see the results
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
	ArgCheck("build-pixels", "ii", argc, argv);
	int x=IntFromScheme(argv[0]);
	int y=IntFromScheme(argv[1]);
	if (x<1 || y<1)
	{
		Trace::Stream<<"build-pixels: resolution in x or y less than 1!"<<endl;
		MZ_GC_UNREG(); 
		return scheme_void;
	}	
	PixelPrimitive *Prim = new PixelPrimitive(x,y);
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

// TODO:document 

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
// pixels->texture pixelprimitiveid-number
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

// StartFunctionDoc-en
// poly-indices 
// Returns: void
// Description:
// Gets the vertex indices from this primitive
// primitive.
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
		// only if this is a pixel primitive
		PolyPrimitive *pp = dynamic_cast<PolyPrimitive *>(Grabbed);
		if (pp)
		{
			l = scheme_null;
			for (unsigned int n=0; n<pp->GetIndex().size(); n++)
			{
				l=scheme_make_pair(scheme_make_double(pp->GetIndex()[n]),l);
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
// line-intersect start-vec end-vec
// Returns: void
// Description:
// Returns a list of pdata values at each intersection point of 
// the specified line.
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
//         (line-intersect a b))))
// 
// (every-frame
//     (with-primitive l
//         (pdata-set "p" 0 (vector 0 -5 0))
//         (pdata-set "p" 1 (vector (* 5 (sin (time))) 5 0))
//         (check (pdata-ref "p" 0) (pdata-ref "p" 1))))
// EndFunctionDoc


// StartFunctionDoc-pt
// line-intersect número-id
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
//         (line-intersect a b))))
// 
// (every-frame
//     (with-primitive l
//         (pdata-set "p" 0 (vector 0 -5 0))
//         (pdata-set "p" 1 (vector (* 5 (sin (time))) 5 0))
//         (check (pdata-ref "p" 0) (pdata-ref "p" 1))))
// EndFunctionDoc

Scheme_Object *line_intersect(int argc, Scheme_Object **argv)
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
	ArgCheck("line-intersect", "vv", argc, argv);
	
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
// bb-intersect prim thresh
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
//             (translate (vector (* -0.1 (sin (time))) 0 0)))
//         (with-primitive a
//             (when (bb-intersect b 0)
//                 (colour (rndvec))))))
// EndFunctionDoc

Scheme_Object *bb_intersect(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("bb-intersect", "if", argc, argv);
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
	scheme_add_global("build-torus", scheme_make_prim_w_arity(build_torus, "build-torus", 4, 4), env);
	scheme_add_global("build-plane", scheme_make_prim_w_arity(build_plane, "build-plane", 0, 0), env);
	scheme_add_global("build-seg-plane", scheme_make_prim_w_arity(build_seg_plane, "build-seg-plane", 2, 2), env);
	scheme_add_global("build-cylinder", scheme_make_prim_w_arity(build_cylinder, "build-cylinder", 2, 2), env);
	scheme_add_global("build-ribbon", scheme_make_prim_w_arity(build_ribbon, "build-ribbon", 1, 1), env);
	scheme_add_global("build-text", scheme_make_prim_w_arity(build_text, "build-text", 1, 1), env);
	scheme_add_global("build-nurbs-sphere", scheme_make_prim_w_arity(build_nurbs_sphere, "build-nurbs-sphere", 2, 2), env);
	scheme_add_global("build-nurbs-plane", scheme_make_prim_w_arity(build_nurbs_plane, "build-nurbs-sphere", 2, 2), env);
	scheme_add_global("build-particles", scheme_make_prim_w_arity(build_particles, "build-particles", 1, 1), env);
	scheme_add_global("build-locator", scheme_make_prim_w_arity(build_locator, "build-locator", 0, 0), env);
	scheme_add_global("locator-bounding-radius", scheme_make_prim_w_arity(locator_bounding_radius, "locator-bounding-radius", 1, 1), env);
	scheme_add_global("build-pixels", scheme_make_prim_w_arity(build_pixels, "build-pixels", 2, 2), env);
	scheme_add_global("build-type", scheme_make_prim_w_arity(build_type, "build-type", 2, 2), env);
	scheme_add_global("build-extruded-type", scheme_make_prim_w_arity(build_extruded_type, "build-extruded-type", 3, 3), env);
	scheme_add_global("load-primitive", scheme_make_prim_w_arity(load_primitive, "load-primitive", 1, 1), env);
	scheme_add_global("save-primitive", scheme_make_prim_w_arity(save_primitive, "save-primitive", 1, 1), env);
	scheme_add_global("clear-geometry-cache", scheme_make_prim_w_arity(clear_geometry_cache, "clear-geometry-cache", 0, 0), env);
	scheme_add_global("pixels-upload", scheme_make_prim_w_arity(pixels_upload, "pixels-upload", 0, 0), env);
	scheme_add_global("pixels-load", scheme_make_prim_w_arity(pixels_load, "pixels-load", 1, 1), env);
	scheme_add_global("pixels-width", scheme_make_prim_w_arity(pixels_width, "pixels-width", 0, 0), env);
	scheme_add_global("pixels-height", scheme_make_prim_w_arity(pixels_height, "pixels-height", 0, 0), env);
	scheme_add_global("pixels->texture", scheme_make_prim_w_arity(pixels2texture, "pixels->texture", 1, 1), env);
	scheme_add_global("text-params", scheme_make_prim_w_arity(text_params, "text-params", 11, 11), env);
	scheme_add_global("build-blobby", scheme_make_prim_w_arity(build_blobby, "build-blobby", 3, 3), env);
	scheme_add_global("blobby->poly", scheme_make_prim_w_arity(blobby2poly, "blobby->poly", 1, 1), env);
	scheme_add_global("type->poly", scheme_make_prim_w_arity(type2poly, "type->poly", 1, 1), env);
	scheme_add_global("draw-instance", scheme_make_prim_w_arity(draw_instance, "draw-instance", 1, 1), env);
	scheme_add_global("draw-cube", scheme_make_prim_w_arity(draw_cube, "draw-cube", 0, 0), env);
	scheme_add_global("draw-plane", scheme_make_prim_w_arity(draw_plane, "draw-plane", 0, 0), env);
	scheme_add_global("draw-sphere", scheme_make_prim_w_arity(draw_sphere, "draw-sphere", 0, 0), env);
	scheme_add_global("draw-cylinder", scheme_make_prim_w_arity(draw_cylinder, "draw-cylinder", 0, 0), env);
	scheme_add_global("draw-torus", scheme_make_prim_w_arity(draw_torus, "draw-torus", 0, 0), env);
	scheme_add_global("destroy", scheme_make_prim_w_arity(destroy, "destroy", 1, 1), env);
	scheme_add_global("poly-set-index", scheme_make_prim_w_arity(poly_set_index, "poly-set-index", 1, 1), env);
	scheme_add_global("poly-indices", scheme_make_prim_w_arity(poly_indices, "poly-indices", 0, 0), env);
	scheme_add_global("poly-type-enum", scheme_make_prim_w_arity(poly_type_enum, "poly-type-enum", 0, 0), env);
	scheme_add_global("poly-indexed?", scheme_make_prim_w_arity(poly_indexed, "poly-indexed?", 0, 0), env);
	scheme_add_global("poly-convert-to-indexed", scheme_make_prim_w_arity(poly_convert_to_indexed, "poly-convert-to-indexed", 0, 0), env);
	scheme_add_global("build-copy", scheme_make_prim_w_arity(build_copy, "build-copy", 1, 1), env);
	scheme_add_global("make-pfunc", scheme_make_prim_w_arity(make_pfunc, "make-pfunc", 1, 1), env);
	scheme_add_global("pfunc-set!", scheme_make_prim_w_arity(pfunc_set, "pfunc-set!", 2, 2), env);
	scheme_add_global("pfunc-run", scheme_make_prim_w_arity(pfunc_run, "pfunc-run", 1, 1), env);
	scheme_add_global("line-intersect", scheme_make_prim_w_arity(line_intersect, "line-intersect", 2, 2), env);
	scheme_add_global("bb-intersect", scheme_make_prim_w_arity(bb_intersect, "bb-intersect", 2, 2), env);
	scheme_add_global("get-children", scheme_make_prim_w_arity(get_children, "get-children", 0, 0), env);
	scheme_add_global("get-parent", scheme_make_prim_w_arity(get_parent, "get-parent", 0, 0), env);
 	MZ_GC_UNREG(); 
}
