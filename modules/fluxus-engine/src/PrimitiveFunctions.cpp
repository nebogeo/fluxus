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
using namespace Fluxus;
using namespace SchemeHelper;

// StartSectionDoc-en
// Primitives
// Primitives are objects that you can render. There isn't really much else in a fluxus scene, 
// except lights, a camera and lots of primitives.
// Example: 
// EndSectionDoc 

// StartSectionDoc-pt
// Primitivas
// Primitivas são objetos que você pode renderizar. Não há muito mais coisas
// numa cena do fluxus, exceto luzes, uma camera e muitas primitivas.
// Exemplo:
// EndSectionDoc

// StartFunctionDoc-en
// build-cube
// Returns: primitiveid-number
// Description:
// A simple cube, texture mapped placement per face.
// @image{images/cube}
// Example:
// (define mynewcube (build-cube))
// EndFunctionDoc

// StartFunctionDoc-pt
// build-cube
// Retorna: número-de-id-da-primitiva
// Descrição:
// Um simples cubo, mapeamento de textura por face.
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
// Example:
// (define mynewshape (build-polygons 100 'triangle-strip))
// EndFunctionDoc

Scheme_Object *build_polygons(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("build-polygons", "iS", argc, argv);
	
	string t = SymbolName(argv[1]);
	PolyPrimitive::Type type=PolyPrimitive::TRISTRIP;
	if (t=="triangle-strip") type=PolyPrimitive::TRISTRIP;
	else if (t=="quad-list") type=PolyPrimitive::QUADS;
	else if (t=="triangle-list") type=PolyPrimitive::TRILIST;
	else if (t=="triangle-fan") type=PolyPrimitive::TRIFAN;
	else if (t=="polygon") type=PolyPrimitive::POLYGON;
	else 
	{
		cerr<<"build-polygons: unknown poly type: "<<t<<endl;
	}
	
	PolyPrimitive *Prim = new PolyPrimitive(type);
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

// StartFunctionDoc-pt
// build-sphere número-de-cortes-horizontais número-de-cortes-verticais
// Retorna: número-id-primitiva
// Descrição:
// Uma esfera com a resolução especificada, a textura mapeada no
// estilo normal "world map".
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

// StartFunctionDoc-pt
// build-plane
// Retorna: número-id-primitiva
// Descrição:
// Um único plano quadrado, mapeado de 0->1 em ambas as dimensões.
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

// StartFunctionDoc-pt
// build-seg-plane número-de-vértices-x número-de-vértices-y
// Retorna: número-id-primitiva
// Descrição:
// Um plano poligonal tesselado, mapeado de 0->1 em ambas dimensões.
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

// StartFunctionDoc-pt
// build-cylinder h-segmentos r-segmentos
// Retorna: número-id-primitiva
// Descrição:
// Um cilindro coberto, textura mapeada em volta, e mal mapeada em
// volta do fim.
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

// StartFunctionDoc-pt
// build-line numpoints-número
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

// StartFunctionDoc-pt
// build-text string-texto
// Retorna: número-id-primitiva
// Descrição:
// Constrói uma sequencia de planos, mapeados de forma que uma textura
// de fonte possa ser usada para visualização. Pode vir a ser útil para
// coisas mais abstratas. A fonte é assumida como não proporcional -
// tem um exemplo de fonte acompanhando o fluxus.
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

// StartFunctionDoc-pt
// build-nurbs-sphere h-segmentos r-segmentos
// Retorna: número-id-primitiva
// Descrição:
// Constrói uma esfera nurbs tesselada, mapead da mesma forma que a
// esfera poligonal.
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

// StartFunctionDoc-pt
// build-nurbs-plane h-segmento r-segmento
// Retorna: número-id-primitiva
// Descrição:
// Constrói um plano nurbs tesselado, mapeado na direção uv.
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

// StartFunctionDoc-pt
// build-locator
// Retorna: número-id-primitiva
// Descrição:
// Um locator é uma primitiva vazia, útil para parentesco (quando você
// não quer ter o objeto pai visivel). Essa primitiva só pode ser
// visualizada com (hint-origin) para mostrar sua origem de
// transformação local.
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

// StartFunctionDoc-pt
// build-pixels número-largura número-altura
// Retorna: número-id-primitiva
// Descrição:
// Faz uma nova primitiva pixel. Uma primitiva pixel é usada pra fazer
// texturas procedurais, que podem então ser aplicadas em outras
// primitivas. Por essa razão, primitivas pixel não vão ser
// renderizadas muito, mas você pode renderizar elas para visualizar a
// texturas em um plano.
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
// pixels-upload
// Returns: void
// Description:
// Uploads the texture data, you need to call this when you've finished writing to the 
// pixelprim, and while it's grabbed.
// Example:
// (define mynewshape (build-pixels 100 100))
// (pixels-upload mynewshape)
// EndFunctionDoc

// StartFunctionDoc-pt
// pixels-upload
// Retorna: void
// Descrição:
// Traz os dados da textura, você precisa chamar isto quando você
// finalizou escrever ao pixelprim, e enquanto ele está "grabbed".
// Example:
// (define mynewshape (build-pixels 100 100))
// (pixels-upload mynewshape)
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
	
	cerr<<"pixels-upload can only be called while a pixelprimitive is grabbed"<<endl;
    return scheme_void;
}

// TODO:document 

Scheme_Object *pixels_load(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("pixels-load", "s", argc, argv);	
	
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
	
	cerr<<"pixels-load can only be called while a pixelprimitive is grabbed"<<endl;
 	MZ_GC_UNREG(); 
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

// StartFunctionDoc-pt
// pixels->texture número-id-pixelprim
// Retorna: número-id-textura
// Descrição:
// Retorna uma textura que você pode usar exatamente igual uma que foi
// carregada normalmente.
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

// StartFunctionDoc-pt
// 
// Retorna:
// Descrição:
// 
// Example:

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
// blobby->poly blobbyprimitiveid-number
// Returns: polyprimid-number
// Description:
// Converts the mesh of a blobby primitive into a triangle list polygon primitive.
// Example:
// (define mynewshape (blobby->poly myblobby))
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
	
	cerr<<"blobby->poly can only be called on a blobbyprimitive"<<endl;
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

// StartFunctionDoc-pt
// draw-cube
// Retorna: void
// Descrição:
// Desenha um cubo no estado imediato corrente.
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

// StartFunctionDoc-pt
// draw-plane
// Retorna: void
// Descrição:
// Desenha um plano no estado corrente em modo imediato
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

// StartFunctionDoc-pt
// draw-sphere
// Retorna: void
// Descrição:
// Desenha uma esfera no estado corrente em modo imediato.
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

// StartFunctionDoc-pt
// draw-cylinder
// Retorna: void
// Descrição:
// Desenha um cilindro no estado corrente em modo imediato.
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

// StartFunctionDoc-pt
// destroy número-id-primitive
// Retorna: void
// Descrição:
// Deleta uma primitiva construída do renderizador.
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

// StartFunctionDoc-en
// poly-set-index index-list
// Returns: void
// Description:
// Switches the primitive to indexed mode, and uses the 
// list as the index values for this primitive.
// primitive.
// Example:
// (clear)
// (define p (build-polygons 8 1))
// 
// (grab p)
// ; setup the vertex data
// (pdata-set "p" 0 (vector -1 -1 -1))
// (pdata-set "p" 1 (vector  1 -1 -1))
// (pdata-set "p" 2 (vector  1 -1  1))
// (pdata-set "p" 3 (vector -1 -1  1))
// (pdata-set "p" 4 (vector -1  1 -1))
// (pdata-set "p" 5 (vector  1  1 -1))
// (pdata-set "p" 6 (vector  1  1  1))
// (pdata-set "p" 7 (vector -1  1  1))
// 
// (hint-wire)
// (hint-unlit)
// 
// ; connect the verts together into faces
// (poly-set-index (list 7 6 5 4  5 6 2 1 
//                        4 5 1 0  1 2 3 0
//                        3 7 4 0  6 7 3 2))
// 
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-pt
// poly-set-index lista-indexada
// Retorna: void
// Descrição:
// Troca a primitiva por modo indexado,e usa a lista como valores de
// index para essa primitiva.
// Example:
// (clear)
// (define p (build-polygons 8 1))
// 
// (grab p)
// ; setup the vertex data
// (pdata-set "p" 0 (vector -1 -1 -1))
// (pdata-set "p" 1 (vector  1 -1 -1))
// (pdata-set "p" 2 (vector  1 -1  1))
// (pdata-set "p" 3 (vector -1 -1  1))
// (pdata-set "p" 4 (vector -1  1 -1))
// (pdata-set "p" 5 (vector  1  1 -1))
// (pdata-set "p" 6 (vector  1  1  1))
// (pdata-set "p" 7 (vector -1  1  1))
// 
// (hint-wire)
// (hint-unlit)
// 
// ; connect the verts together into faces
// (poly-set-index (list 7 6 5 4  5 6 2 1 
//                        4 5 1 0  1 2 3 0
//                        3 7 4 0  6 7 3 2))
// 
// (ungrab)
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
	
	cerr<<"poly-set-index! can only be called while a polyprimitive is grabbed"<<endl;
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
// Example:
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
	
	cerr<<"poly-convert-to-indexed can only be called while a polyprimitive is grabbed"<<endl;
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
// arithmetic
// Example:
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
					cerr<<"pfunc-set! has found a strange sized vector"<<endl;
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
					cerr<<"pfunc-set! only takes lists of numbers"<<endl;
				}
				
			}
			else
			{
				cerr<<"pfunc-set! has found an argument type it can't send, numbers, vectors or lists only"<<endl;
			}
		}
		else
		{
			cerr<<"pfunc-set! has found a mal-formed parameter list"<<endl;
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
// arithmetic
// Example:
// (define mypfunc (make-pfunc "arithmetic"))
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
	scheme_add_global("pixels-upload", scheme_make_prim_w_arity(pixels_upload, "pixels-upload", 0, 0), env);
	scheme_add_global("pixels-load", scheme_make_prim_w_arity(pixels_load, "pixels-load", 1, 1), env);
	scheme_add_global("pixels->texture", scheme_make_prim_w_arity(pixels2texture, "pixels->texture", 1, 1), env);
	scheme_add_global("build-blobby", scheme_make_prim_w_arity(build_blobby, "build-blobby", 3, 3), env);
	scheme_add_global("blobby->poly", scheme_make_prim_w_arity(blobby2poly, "blobby->poly", 1, 1), env);
	scheme_add_global("draw-instance", scheme_make_prim_w_arity(draw_instance, "draw-instance", 1, 1), env);
	scheme_add_global("draw-cube", scheme_make_prim_w_arity(draw_cube, "draw-cube", 0, 0), env);
	scheme_add_global("draw-plane", scheme_make_prim_w_arity(draw_plane, "draw-plane", 0, 0), env);
	scheme_add_global("draw-sphere", scheme_make_prim_w_arity(draw_sphere, "draw-sphere", 0, 0), env);
	scheme_add_global("draw-cylinder", scheme_make_prim_w_arity(draw_cylinder, "draw-cylinder", 0, 0), env);
	scheme_add_global("destroy", scheme_make_prim_w_arity(destroy, "destroy", 1, 1), env);
	scheme_add_global("poly-set-index", scheme_make_prim_w_arity(poly_set_index, "poly-set-index", 1, 1), env);
	scheme_add_global("poly-convert-to-indexed", scheme_make_prim_w_arity(poly_convert_to_indexed, "poly-convert-to-indexed", 0, 0), env);
	scheme_add_global("build-copy", scheme_make_prim_w_arity(build_copy, "build-copy", 1, 1), env);
	scheme_add_global("make-pfunc", scheme_make_prim_w_arity(make_pfunc, "make-pfunc", 1, 1), env);
	scheme_add_global("pfunc-set!", scheme_make_prim_w_arity(pfunc_set, "pfunc-set!", 2, 2), env);
	scheme_add_global("pfunc-run", scheme_make_prim_w_arity(pfunc_run, "pfunc-run", 1, 1), env);
 	MZ_GC_UNREG(); 
}
