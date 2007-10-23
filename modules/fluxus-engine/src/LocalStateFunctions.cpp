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
#include "SchemeHelper.h"
#include "Engine.h"
#include "LocalStateFunctions.h"
#include "Renderer.h"

using namespace LocalStateFunctions;
using namespace SchemeHelper;
using namespace Fluxus;

// StartSectionDoc-en
// local-state
// The local state functions control rendering either for the current state - or the state of 
// the currently grabbed primitive. In fluxus state means the way that things are displayed, 
// either turning on and off rendering features, changing the style of different features, or altering 
// the current transform. 
// Example:
// EndSectionDoc 

// StartSectionDoc-pt
// estado-local
// As funções de estado local controlam a renderização ou do estado
// corrente - ou do estado da primitiva correntemente
// pega(grabbed). Em Fluxus estado significa significa a forma que as
// coisas são mostradas, tanto ligando/desligando opções de render,
// mudando o estilo de diferentes opções, ou alterando a transformação
// corrente.
// Exemplo:
// EndSectionDoc

// StartFunctionDoc-en
// push 
// Returns: void
// Description:
// Pushes a copy of the current drawing state to the top of the stack. The drawing state 
// contains information about things like the current colour, transformation and hints.
// Example:
// (colour (vector 1 0 0)) ; set current colour to red
// (push)                  ; copy and push drawing state
// (colour (vector 0 1 0)) ; set current colour to green
// (draw-cube)             ; draws a green cube
// (pop)				   ; forget old drawing state
// ; current colour is now red again
// EndFunctionDoc

// StartFunctionDoc-pt
// push
// Retorna: void
// Descrição:
// Empurra uma cópia do estado corrente de desenho para o topo da
// pilha. O estado de desenho contém informação sobre coisas como
// cor corrente, transformação e dicas(hints).
// Exemplo:
// (colour (vector 1 0 0)) ; aplica cor corrente pra vermelho
// (push)                  ; copia e empurra estado de desenho
// (colour (vector 0 1 0)) ; aplica cor corrente pra verde
// (draw-cube)			   ; desenha um cubo verde
// (pop)				   ; esquece estado de desenho antigo
// ; cor corrente é vermelha de novo.
// EndFunctionDoc

Scheme_Object *push(int argc, Scheme_Object **argv)
{
	if (Engine::Get()->Grabbed())
	{
		Trace::Stream<<"error: can't (push) while an object is (grab)bed"<<endl;
		return scheme_void;
	}
	
    Engine::Get()->Renderer()->PushState();
    return scheme_void;
}

// StartFunctionDoc-en
// pop
// Returns: void
// Description:
// Destroys the current drawing state, and sets the current one to be the previously pushed 
// one in the stack. The drawing state contains information about things like the current 
// colour, transformation and hints.
// Example:
// (colour (vector 1 0 0)) ; set current colour to red
// (push)                  ; copy and push drawing state
// (colour (vector 0 1 0)) ; set current colour to green
// (draw-cube)             ; draws a green cube
// (pop)                   ; forget old drawing state
// ; current colour is now red again
// EndFunctionDoc

// StartFunctionDoc-pt
// pop
// Retorna: void
// Descrição:
// Destrói o estado de desenho corrente, e aplica para o estado
// corrente o antigo anteriormente empurrado na pilha. O estado   
// de desenho contém informação sobre coisas como cor corrente, 
// transformação e dicas(hints).
// Exemplo:
// (colour (vector 1 0 0)) ; aplica cor corrente pra vermelho
// (push)                  ; copia e empurra estado de desenho
// (colour (vector 0 1 0)) ; aplica cor corrente pra verde
// (draw-cube)             ; desenha um cubo verde
// (pop)                   ; esquece estado de desenho antigo
// ; cor corrente é vermelha de novo.
// EndFunctionDoc

Scheme_Object *pop(int argc, Scheme_Object **argv)
{
	if (Engine::Get()->Grabbed())
	{
		Trace::Stream<<"error: can't (pop) while an object is (grab)bed"<<endl;
		return scheme_void;
	}

    Engine::Get()->Renderer()->PopState();
    return scheme_void;
}

// StartFunctionDoc-en
// grab object-id
// Returns: void
// Description:
// Grabs the specified object. Once an object has grabbed it's state can be modified using
// the same commands used to set the current drawing state. (ungrab) needs to be used to
// return to the normal drawing state. Grabbing can also be stacked, in which case ungrab
// pops to the last grabbed primitive.
// Example:
// (colour (vector 1 0 0))      ; set the current colour to red
// (define mycube (build-cube)) ; makes a red cube 
// (grab mycube)  				       
// (colour (vector 0 1 0)) ; sets the cubes colour to green 
// (ungrab)				   ; return to normal state
// EndFunctionDoc

// StartFunctionDoc-pt
// grab id-do-objeto
// Retorna: void
// Descrição:
// Pega o objeto especificado. Uma vez que o objeto foi pego seu
// estado pode ser modificado usando os mesmos comandos usados pra
// ajustar o estado de desenho atual. (ungrab) precisa ser usado para
// retornar ao estado de desenho normal. "grabbing" também pode ser
// "pilhado", neste caso (ungrab) pula para a próxima primitiva pega.
// Exemplo:
// (colour (vector 1 0 0))      ; aplica a cor atual para vermelho
// (define mycube (build-cube)) ; faz um cubo vermelho
// (grab mycube)  				       
// (colour (vector 0 1 0)) ; aplica a cor do cubo como verde 
// (ungrab)				   ; retorna ao estado normal
// EndFunctionDoc

Scheme_Object *grab(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("grab", "i", argc, argv);
	Engine::Get()->PushGrab(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// ungrab
// Returns: void
// Description:
// Ungrabs the currently grabbed object, and either returns to the normal drawing state, 
// or pops to the last grabbed primitive. 
// Example:
// (colour (vector 1 0 0))      ; set the current colour to red
// (define mycube (build-cube)) ; makes a red cube 
// (grab mycube)  				       
// (colour (vector 0 1 0)) ; sets the cubes colour to green 
// (ungrab)				   ; return to normal state
// EndFunctionDoc

// StartFunctionDoc-pt
// ungrab
// Retorna: void
// Descrição:
// Solta o objeto pego atualmente, e ou retorna ao estado de desenho
// normal, ou tira a ultima primitiva pega do topo da pilha.
// Exemplo:
// (colour (vector 1 0 0))      ; aplica a cor atual para vermelho
// (define mycube (build-cube)) ; faz um cubo vermelho
// (grab mycube)  				       
// (colour (vector 0 1 0)) ; aplica a cor verde ao cubo 
// (ungrab)				   ; retorna ao estado normal
// EndFunctionDoc

Scheme_Object *ungrab(int argc, Scheme_Object **argv)
{
	Engine::Get()->PopGrab();
	return scheme_void;
}

// StartFunctionDoc-en
// apply-transform object-id
// Returns: void
// Description:
// Applies the current object transform to the vertex positions of the supplied object and 
// sets it's transform to identity.
// Example:
// (rotate (vector 45 0 0))     
// (define mycube (build-cube)) ; makes a cube with a rotation 
// (apply mycube)  				; applies the rotation to the points of the cube
// EndFunctionDoc

// StartFunctionDoc-pt
// apply-transform id-do-objeto
// Retorna: void
// Descrição:
// Aplica a transformação corrente às posições dos vértices do objeto
// dado e ajusta sua tranformação para identidade.
// Exemplo:
// (rotate (vector 45 0 0))     
// (define mycube (build-cube)) ; faz um cubo com uma rotação
// (apply mycube)  	      ; aplica a rotação aos pontos do cubo
// EndFunctionDoc

Scheme_Object *apply(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("apply-transform", "i", argc, argv);
	Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]))->ApplyTransform();
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// opacity value
// Returns: void
// Description:
// Sets the opacity of the current drawing state, or the currently grabbed primitive.
// Example:
// (opacity 0.5)
// (define mycube (build-cube)) ; makes a half transparent cube
// EndFunctionDoc

// StartFunctionDoc-pt
// opacity valor
// Retorna: void
// Descrição:
// Ajusta a opacidade do estado de desenho atual, ou da primitiva pega
// atualmente.
// Exemplo:
// (opacity 0.5)
// (define mycube (build-cube)) ; faz um cubo semi-transparente.
// EndFunctionDoc

Scheme_Object *opacity(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("opacity", "f", argc, argv);
    Engine::Get()->State()->Opacity=FloatFromScheme(argv[0]);
	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// wire-opacity value
// Returns: void
// Description:
// Sets the wireframe opacity of the current drawing state, or the currently
// grabbed primitive.
// Example:
// (hint-none)
// (hint-wire)
// (backfacecull 0)
// (line-width 5)
// (wire-colour (vector 1 1 1))
// (wire-opacity 0.5)
// (build-cube) ; makes a half transparent wireframe cube
// EndFunctionDoc

Scheme_Object *wire_opacity(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("wire-opacity", "f", argc, argv);
    Engine::Get()->State()->WireOpacity=FloatFromScheme(argv[0]);
	MZ_GC_UNREG();
    return scheme_void;
}
// StartFunctionDoc-en
// shinyness value
// Returns: void
// Description:
// Sets the shinyness of the current drawing state, or the currently grabbed primitive. 
// This value sets the tightness of the specular highlight.
// Example:
// (shinyness 100)     
// (specular (vector 1 1 1)) ; sets the specular colour
// (define mysphere (build-sphere 10 10)) ; makes a shiny cube 
// EndFunctionDoc

// StartFunctionDoc-pt
// shinyness valor
// Retorna: void
// Descrição:
// Ajusta o brilho do estado atual de desenho, ou da primitiva
// atualmente pega. Esse valor ajusta a densidade do brilho
// especular.
// Exemplo:
// (shinyness 100)     
// (specular (vector 1 1 1)) ; ajusta a cor especular
// (define mysphere (build-sphere 10 10)) ; makes a shiny cube 
// EndFunctionDoc

Scheme_Object *shinyness(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("shinyness", "f", argc, argv);
    Engine::Get()->State()->Shinyness=FloatFromScheme(argv[0]);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// colour colour-vector
// Returns: void
// Description:
// Sets the colour of the current drawing state, or the currently grabbed primitive. 
// Example:
// (colour (vector 1 0.5 0.1)) ; mmm orange...   
// (define mycube (build-cube)) ; makes an orange cube 
// EndFunctionDoc

// StartFunctionDoc-pt
// colour vetor-cor
// Retorna: void
// Descrição:
// Ajusta a cor do estado de desenho atual, ou a primitiva atualmente pega.
// Exemplo:
// (colour (vector 1 0.5 0.1)) ; mmm laranja...   
// (define mycube (build-cube)) ; faz um cubo laranja 
// EndFunctionDoc

Scheme_Object *colour(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("colour", "v", argc, argv);
    Engine::Get()->State()->Colour=ColourFromScheme(argv[0]);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// wire-colour colour-vector
// Returns: void
// Description:
// Sets the wire frame colour of the current drawing state, or the currently grabbed 
// primitive. Visible with (hint-wire) on most primitives.
// Example:
// (wire-colour (vector 1 1 0)) ; set yellow as current wire colour
// (hint-wire)   
// (define mycube (build-cube)) ; makes a cube with yellow wireframe 
// EndFunctionDoc

// StartFunctionDoc-pt
// wire-colour vetor-cor
// Retorna: void
// Descrição:
// Ajusta a cor do "wire frame" do estado de desenho atual, ou a
// primitiva atualmente pega. Visível com (hint-wire) na maioria das
// primitivas. 
// Exemplo:
// (wire-colour (vector 1 1 0)) ; ajusta a cor do fio como amarelo
// (hint-wire)   
// (define mycube (build-cube)) ; faz um cubo com wireframe amarelo
// EndFunctionDoc

Scheme_Object *wire_colour(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("wire-colour", "v", argc, argv);
   	Engine::Get()->State()->WireColour=ColourFromScheme(argv[0]);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// specular colour-vector
// Returns: void
// Description:
// Sets the specular colour of the current drawing state, or the currently grabbed 
// primitive. 
// Example:
// (specular (vector 0 0 1)) ; set blue as specular colour
// (define mysphere (build-sphere 10 10)) ; makes a shiny blue sphere
// EndFunctionDoc

// StartFunctionDoc-pt
// specular vetor-cor
// Retorna: void
// Descrição:
// Ajusta a cor especular do estado de desenho corrente, ou o objeto
// atualmente pego.
// Exemplo:
// (specular (vector 0 0 1)) ; ajusta azul como a cor especular
// (define mysphere (build-sphere 10 10)) ; faz uma esfera azul brilhante.
// EndFunctionDoc

Scheme_Object *specular(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("specular", "v", argc, argv);
    Engine::Get()->State()->Specular=ColourFromScheme(argv[0]);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// ambient colour-vector
// Returns: void
// Description:
// Sets the ambient colour of the current drawing state, or the currently grabbed 
// primitive. 
// Example:
// (ambient (vector 0 0 1)) ; set blue as ambient colour
// (define mysphere (build-sphere 10 10)) ; makes a boringly blue sphere 
// EndFunctionDoc

// StartFunctionDoc-pt
// ambient vetor-cor
// Retorna: void
// Descrição:
// Ajusta a cor ambiente do estado de desenho corrente, ou a primitiva
// atualmente pega.
// Exemplo:
// (ambient (vector 0 0 1)) ; ajusta a cor ambiente como azul
// (define mysphere (build-sphere 10 10)) ; faz uma chata esfera azul 
// EndFunctionDoc

Scheme_Object *ambient(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("ambient", "v", argc, argv);
    Engine::Get()->State()->Ambient=ColourFromScheme(argv[0]);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// opacity value
// Returns: void
// Description:
// Sets the emissive colour of the current drawing state, or the currently grabbed 
// primitive. 
// Example:
// (emissive (vector 0 0 1)) ; set blue as emissive colour
// (define mysphere (build-sphere 10 10)) ; makes an bright blue sphere 
// EndFunctionDoc

// StartFunctionDoc-pt
// opacity valor
// Retorna: void
// Descrição:
// Ajusta a cor emissiva do estado de desenho atual, ou da primitiva
// atualmente pega.
// Exemplo:
// (emissive (vector 0 0 1)) ; ajusta a cor emissiva para azul
// (define mysphere (build-sphere 10 10)) ; faz uma esfera azul brilhante 
// EndFunctionDoc

Scheme_Object *emissive(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("emissive", "v", argc, argv);
    Engine::Get()->State()->Emissive=ColourFromScheme(argv[0]);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// identity
// Returns: void
// Description:
// Sets the drawing state transform to identity, on the state stack, or the currently 
// grabbed primitive. 
// Example:
// (push)
// (scale (vector 2 2 2)) ; set the current scale to double in each dimension
// (define mycube (build-cube)) ; make a scaled cube 
// (pop)
// (grab mycube)
// (identity) ; erases the transform and puts the cube back to its original state
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-pt
// identity
// Retorna: void 
// Descrição:
// Ajusta a transformação do estado de desenho para identidade, no
// estado de pilha, ou a primitiva atualmente pega.
// Exemplo:
// (push)
// (scale (vector 2 2 2)) ; ajusta o tamanho atual pro dobro em cada dimensão
// (define mycube (build-cube)) ; faz um cubo aumentado
// (pop)
// (grab mycube)
// (identity) ; apaga a transformação e coloca o cubo de volta ao seu
//            ; estado original
// (ungrab)
// EndFunctionDoc

Scheme_Object *flux_identity(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Transform.init();
    return scheme_void;
}

// StartFunctionDoc-en
// concat matrix
// Returns: void
// Description:
// Concatenates (multiplies) a matrix on to the current drawing state or grabbed primitive. 
// Example:
// (define mymatrix (mrotate (vector 0 45 0))) ; make a matrix
// (concat mymatrix) ; concat it into the current state
// (build-cube) ; make a cube with this rotation
// EndFunctionDoc

// StartFunctionDoc-pt
// concat matrix
// Retorna: void
// Descrição:
// Concatena (multiplica) uma matriz para o estado de desenho atual ou
// primitiva pega.
// Exemplo:
// (define mymatrix (mrotate (vector 0 45 0))) ; faz uma matriz
// (concat mymatrix) ; concat ela no estado atual
// (build-cube) ; faz um cubo com esta rotação
// EndFunctionDoc

Scheme_Object *concat(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("concat", "m", argc, argv);
	dMatrix m;
	FloatsFromScheme(argv[0],m.arr(),16);
    Engine::Get()->State()->Transform*=m;
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// translate vector
// Returns: void
// Description:
// Applies a translation to the current drawing state transform or grabbed primitive.
// Example:
// (transform (vector 0 1.4 0)) ; translates the current transform up a bit
// (build-cube) ; build a cube with this transform
// EndFunctionDoc

// StartFunctionDoc-pt
// translate vetor
// Retorna: void
// Descrição:
// Aplica uma translação ao estado de desenho atual ou primitiva pega
// Exemplo:
// (transform (vector 0 1.4 0)) ; translada a transformação atual pra
//                              ; cima um pouco
// (build-cube) ; constrói um cubo com esta transformação
// EndFunctionDoc

Scheme_Object *translate(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("translate", "v", argc, argv);
	dVector t;
	FloatsFromScheme(argv[0],t.arr(),3);
    Engine::Get()->State()->Transform.translate(t.x,t.y,t.z);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// rotate vector-or-quaternion
// Returns: void
// Description:
// Applies a rotation to the current drawing state transform or grabbed primitive.
// Example:
// (rotate (vector 0 45 0)) ; turns 45 degrees in the Y axis
// (build-cube) ; build a cube with this transform
// EndFunctionDoc

// StartFunctionDoc-pt
// rotate vetor-ou-quaternion
// Retorna: void
// Descrição:
// Aplica uma rotação ao estado de desenho atual ou primitiva pega.
// Exemplo:
// (rotate (vector 0 45 0)) ; vira 45 graus no eixo Y 
// (build-cube) ; constrói um cubo com esta transformação
// EndFunctionDoc

Scheme_Object *rotate(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
  	if (!SCHEME_VECTORP(argv[0])) scheme_wrong_type("rotate", "vector", 0, argc, argv);
	
	if (SCHEME_VEC_SIZE(argv[0])==3)
	{
		// euler angles
		float rot[3];
		FloatsFromScheme(argv[0],rot,3);
	    Engine::Get()->State()->Transform.rotxyz(rot[0],rot[1],rot[2]);
	}
	else if (SCHEME_VEC_SIZE(argv[0])==4)
	{
		// quaternion
		dQuat a;
		FloatsFromScheme(argv[0],a.arr(),4);
		dMatrix m=a.toMatrix();
	 	Engine::Get()->State()->Transform*=m;
	}
	else
	{
		Trace::Stream<<"rotate - wrong number of elements in vector"<<endl;
	}
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// scale vector
// Returns: void
// Description:
// Applies a scale to the current drawing state transform or grabbed primitive.
// Example:
// (scale (vector 0.5 0.5 0.5)) ; scales the current transform to half the size
// (build-cube) ; build a cube with this transform
// EndFunctionDoc

// StartFunctionDoc-pt
// scale vetor
// Retorna: void
// Descrição:
// Aplica uma escalagem ao estado de desenho atual ou primitiva pega.
// Exemplo:
// (scale (vector 0.5 0.5 0.5)) ; escala a tranformação atual para
//                              ; metade do tamanho
// (build-cube) ; constrói um cubo com esta transformação
// EndFunctionDoc

Scheme_Object *scale(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("scale", "v", argc, argv);
	dVector t;
	FloatsFromScheme(argv[0],t.arr(),3);
    Engine::Get()->State()->Transform.scale(t.x,t.y,t.z);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// get-transform
// Returns: matrix-vector
// Description:
// Returns a matrix representing the current state transform or for the 
// grabbed primitive.
// Example:
// (translate (vector 1 0 0))
// (display (get-transform))(newline) ; prints the current transform
// (define shape (build-sphere 10 10))
// (grab shape)
// (translate (vector 0 1 0))
// (display (get-transform))(newline) ; prints shape's transform
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-pt
// get-transform
// Retorna: vetor-matriz
// Descrição:
// Retorna uma matriz representando o estado de tranformação corrente
// ou para a primitiva pega.
// Exemplo:
// (translate (vector 1 0 0))
// (display (get-transform))(newline) ; imprime a transformação corrente
// (define shape (build-sphere 10 10))
// (grab shape)
// (translate (vector 0 1 0))
// (display (get-transform))(newline) ; imprime a transformação da forma
// (ungrab)
// EndFunctionDoc

Scheme_Object *get_transform(int argc, Scheme_Object **argv)
{
	return FloatsToScheme(Engine::Get()->State()->Transform.arr(),16);
}

// StartFunctionDoc-en
// parent primitive-id
// Returns: void
// Description:
// Parents the currently grabbed primitive to the supplied parent primitive. The current
// primitive will now be moved around with the parent by aquiring all the parent's
// transforms.
// Example:
// (define parent-prim (build-cube)) ; make a parent cube
// (translate (vector 2 0 0)) ; move a bit in x
// (parent parent-prim) ; set parent-prim as the current parent
// (define child-prim (build-cube)) ; make a child cube
// (grab parent-prim) 
// (rotate (vector 0 45 0)) ; the child will now be moved by this transform in addition to its own
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-pt
// parent id-primitiva
// Retorna: void
// Descrição:
// Parenteia a primitiva correntemente pega à primitiva pai dada. A
// primitiva corrente vai ser agora movida junto com o pai por
// adquirir todas as suas transformações.
// Exemplo:
// (define parent-prim (build-cube)) ; make a parent cube
// (translate (vector 2 0 0)) ; move a bit in x
// (parent parent-prim) ; set parent-prim as the current parent
// (define child-prim (build-cube)) ; make a child cube
// (grab parent-prim) 
// (rotate (vector 0 45 0)) ; the child will now be moved by this transform in addition to its own
// (ungrab)
// EndFunctionDoc

Scheme_Object *parent(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("parent", "i", argc, argv);
    Engine::Get()->State()->Parent=IntFromScheme(argv[0]);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// line-width value
// Returns: void
// Description: 
// Sets the line width (in screen space) of the current drawing state, or the currently 
// grabbed primitive. Affects wireframe and things like that.
// Example:
// (line-width 5)
// (hint-wire)
// (build-sphere 10 10) ; make a sphere with thick wireframe
// EndFunctionDoc

// StartFunctionDoc-pt
// line-width valor
// Retorna: void
// Descrição:
// Ajusta a largura da linha (em espaço de tela) do estado de desenho
// corrente, ou da primitiva correntemente pega, Afeta wireframe e
// afins.
// Exemplo:
// (line-width 5)
// (hint-wire)
// (build-sphere 10 10) ; faz uma esfera com um denso wireframe
// EndFunctionDoc

Scheme_Object *line_width(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("line-width", "f", argc, argv);
    Engine::Get()->State()->LineWidth=FloatFromScheme(argv[0]);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// point-width value
// Returns: void
// Description: 
// Sets the point width (in screen space) of the current drawing state, or the currently 
// grabbed primitive. Affects point rendering and particles in hardware point mode.
// Example:
// (point-width 5)
// (hint-points)
// (build-sphere 10 10) ; make a sphere with thick points
// EndFunctionDoc

// StartFunctionDoc-pt
// point-width value
// Retorna: void
// Descrição:
// Ajusta o tamanho do ponto (em espaço na tela) do estado de desenho
// corrente, ou da primitiva pega. Afeta a renderização de pontos e
// particulas no hardware.
// Exemplo:
// (point-width 5)
// (hint-points)
// (build-sphere 10 10) ; faz uma esfera com pontos grossos
// EndFunctionDoc

Scheme_Object *point_width(int argc, Scheme_Object **argv)
{
 	DECL_ARGV();
 	ArgCheck("point-width", "f", argc, argv);
    Engine::Get()->State()->PointWidth=FloatFromScheme(argv[0]);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// blend-mode src dst
// Returns: void
// Description: 
// Sets the blend mode of the current drawing state, or the currently grabbed primitive. 
// This is the way that alpha is composited to the rendering surface. Blendmode symbols can
// consist of:
// zero one dst-color one-minus-dst-color src-alpha one-minus-src-alpha dst-alpha 
// one-minus-dst-alpha
// Also src-alpha-saturate as an option for the source blendmode only.
// Example:
// ; list out all the possible blendmodes
// 
// (define src-blend (vector 'zero 'one 'dst-color 'one-minus-dst-color 'src-alpha
//                     'one-minus-src-alpha 'dst-alpha 'one-minus-dst-alpha
//                     'src-alpha-saturate))
// 
// (define dst-blend (vector 'zero 'one 'dst-color 'one-minus-dst-color 'src-alpha
//                     'one-minus-src-alpha 'dst-alpha 'one-minus-dst-alpha))
// 
// ; picks a random element
// (define (pick-rnd-item l)
//     (vector-ref l (random (vector-length l))))
// 
// ; make lots of random spheres
// (define (rnd-sphere n)
//     (push)
//     (hint-depth-sort)
//     (opacity 0.5)
//     (colour (vector (flxrnd) (flxrnd) (flxrnd)))
// 
//     ; set a random blendmode
//     (blend-mode (pick-rnd-item src-blend) (pick-rnd-item dst-blend))
// 
//     (translate (vector (flxrnd) (flxrnd) (flxrnd)))
//     (scale (vector 0.1 0.1 0.1))
//     (build-sphere 10 10)
//     (pop)
//     (if (zero? n)
//         0
//         (rnd-sphere (- n 1))))
// 
// (clear)
// (clear-colour (vector 0.5 0.5 0.5))
// (rnd-sphere 100)
// EndFunctionDoc

// StartFunctionDoc-pt
// blend-mode src dst
// Retorna: void
// Descrição:
// Ajusta o modo de mistura do estado de desenho corrente, ou da
// primitiva pega. Esse é o modo que o alpha é composto no superficie
// renderizada.
// Exemplo:
// ; list out all the possible blendmodes
// 
// (define src-blend (vector 'zero 'one 'dst-color 'one-minus-dst-color 'src-alpha
//                     'one-minus-src-alpha 'dst-alpha 'one-minus-dst-alpha
//                     'src-alpha-saturate))
// 
// (define dst-blend (vector 'zero 'one 'dst-color 'one-minus-dst-color 'src-alpha
//                     'one-minus-src-alpha 'dst-alpha 'one-minus-dst-alpha))
// 
// ; picks a random element
// (define (pick-rnd-item l)
//     (vector-ref l (random (vector-length l))))
// 
// ; make lots of random spheres
// (define (rnd-sphere n)
//     (push)
//     (hint-depth-sort)
//     (opacity 0.5)
//     (colour (vector (flxrnd) (flxrnd) (flxrnd)))
// 
//     ; set a random blendmode
//     (blend-mode (pick-rnd-item src-blend) (pick-rnd-item dst-blend))
// 
//     (translate (vector (flxrnd) (flxrnd) (flxrnd)))
//     (scale (vector 0.1 0.1 0.1))
//     (build-sphere 10 10)
//     (pop)
//     (if (zero? n)
//         0
//         (rnd-sphere (- n 1))))
// 
// (clear)
// (clear-colour (vector 0.5 0.5 0.5))
// (rnd-sphere 100)
// EndFunctionDoc

Scheme_Object *blend_mode(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
  	ArgCheck("blend-mode", "SS", argc, argv);
	string s=SymbolName(argv[0]);	
	string d=SymbolName(argv[1]);	
		
	if (s=="zero") Engine::Get()->State()->SourceBlend=GL_ZERO;
	else if (s=="one") Engine::Get()->State()->SourceBlend=GL_ONE;
	else if (s=="dst-color") Engine::Get()->State()->SourceBlend=GL_DST_COLOR;  
	else if (s=="one-minus-dst-color") Engine::Get()->State()->SourceBlend=GL_ONE_MINUS_DST_COLOR;
	else if (s=="src-alpha") Engine::Get()->State()->SourceBlend=GL_SRC_ALPHA;		   
	else if (s=="one-minus-src-alpha") Engine::Get()->State()->SourceBlend=GL_ONE_MINUS_SRC_ALPHA;
	else if (s=="dst-alpha") Engine::Get()->State()->SourceBlend=GL_DST_ALPHA;
	else if (s=="one-minus-dst-alpha") Engine::Get()->State()->SourceBlend=GL_ONE_MINUS_DST_ALPHA;
	else if (s=="src-alpha-saturate") Engine::Get()->State()->SourceBlend=GL_SRC_ALPHA_SATURATE;
	else Trace::Stream<<"source blend mode not recognised: "<<s<<endl;
	
	if (d=="zero") Engine::Get()->State()->DestinationBlend=GL_ZERO;
	else if (d=="one") Engine::Get()->State()->DestinationBlend=GL_ONE;
	else if (d=="dst-color") Engine::Get()->State()->DestinationBlend=GL_DST_COLOR;  
	else if (d=="one-minus-dst-color") Engine::Get()->State()->DestinationBlend=GL_ONE_MINUS_DST_COLOR;
	else if (d=="src-alpha") Engine::Get()->State()->DestinationBlend=GL_SRC_ALPHA;		   
	else if (d=="one-minus-src-alpha") Engine::Get()->State()->DestinationBlend=GL_ONE_MINUS_SRC_ALPHA;
	else if (d=="dst-alpha") Engine::Get()->State()->DestinationBlend=GL_DST_ALPHA;
	else if (d=="one-minus-dst-alpha") Engine::Get()->State()->DestinationBlend=GL_ONE_MINUS_DST_ALPHA;
	else Trace::Stream<<"dest blend mode not recognised: "<<d<<endl;
	
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// hint-solid
// Returns: void
// Description: 
// Sets the render hints to solid of the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-solid) ; this is the default render style so this isn't too exciting
// (build-cube) ; make a solid rendered cube 
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-solid
// Retorna: void
// Descrição:
// Ajusta as dicas pra renderizar como solid no estado de desenho
// corrente, ou a primitiva pega. Dicas de render mudam a forma como
// as primitivas são renderizadas, mas podem ter efeitos diferentes -
// ou nenhum efeito em certas primitivas portanto o nome dicas.
// Exemplo:
// (hint-solid) ; esse é o estilo de render original então não deve
//              ; muito estimulante
// (build-cube) ; faz um cubo renderizado sólido 
// EndFunctionDoc

Scheme_Object *hint_solid(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_SOLID;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-wire
// Returns: void
// Description: 
// Sets the render hints to wireframe of the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-wire)
// (build-cube) ; make a wirefame rendered cube 
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-wire
// Retorna: void
// Descrição:
// Ajusta o render para wireframe no modo de estado corrente, ou da
// primitiva pega. Dicas de render mudam a forma como
// as primitivas são renderizadas, mas podem ter efeitos diferentes -
// ou nenhum efeito em certas primitivas portanto o nome dicas.
// Exemplo:
// (hint-wire)
// (build-cube) ; faz um cubo em wireframe 
// EndFunctionDoc


Scheme_Object *hint_wire(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_WIRE;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-normal
// Returns: void
// Description: 
// Sets the render hints to display normals in the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-normal)
// (build-cube) ; display the normals on this cube 
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-normal
// Retorna: void
// Descrição:
// Ajusta as dicas de render para fazer aparecer normais no estado de
// desenho corrente, ou da primitiva pega. Dicas de render mudam a forma como
// as primitivas são renderizadas, mas podem ter efeitos diferentes -
// ou nenhum efeito em certas primitivas portanto o nome dicas.
// Exemplo:
// (hint-normal)
// (build-cube) ; mostra as normais do cubo
// EndFunctionDoc

Scheme_Object *hint_normal(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_NORMAL;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-points
// Returns: void
// Description: 
// Sets the render hints to display points in the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-points)
// (build-cube) ; display the vertex points on this cube 
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-points
// Retorna: void
// Descrição:
// Ajusta as dicas para aparecer pontos no estado de desenho corrente,
// ou da primitiva pega. Dicas de render mudam a forma como
// as primitivas são renderizadas, mas podem ter efeitos diferentes -
// ou nenhum efeito em certas primitivas portanto o nome dicas.
// Exemplo:
// (hint-points)
// (build-cube) ; mostra os pontos dos vertices deste cubo
// EndFunctionDoc

Scheme_Object *hint_points(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_POINTS;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-anti-alias
// Returns: void
// Description: 
// Sets the render hints to anti-alias in the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-anti-alias)
// (build-cube) ; display a smoothed cube 
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-anti-alias
// Retorna: void
// Descrição:
// Ajusta as dicas de render para anti-alias no estado de desenho
// atual, ou da primitiva pega. Dicas de render mudam a forma como
// as primitivas são renderizadas, mas podem ter efeitos diferentes -
// ou nenhum efeito em certas primitivas portanto o nome dicas.
// Exemplo:
// (hint-anti-alias)
// (build-cube) ; mostra um cubo macio
// EndFunctionDoc

Scheme_Object *hint_anti_alias(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_AALIAS;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-unlit
// Returns: void
// Description: 
// Sets the render hints to unlit in the current drawing state, or the currently grabbed 
// primitive. Render hints change the way that primitives are rendered, but may have 
// different effects - or no effect on certain primitive types, hence the name hint.
// Example:
// (hint-unlit)
// (build-cube) ; display an unlit cube
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-unlit
// Retorna: void
// Descrição:
// Ajusta as dicas de render para luzes desligadas no estado de
// desenho corrente, ou da primitiva pega. Dicas de render mudam a forma como
// as primitivas são renderizadas, mas podem ter efeitos diferentes -
// ou nenhum efeito em certas primitivas portanto o nome dicas.
// Exemplo:
// (hint-unlit)
// (build-cube) ; mostra um cubo sem iluminação
// EndFunctionDoc

Scheme_Object *hint_unlit(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_UNLIT;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-vertcols
// Returns: void
// Description: 
// Sets the render hints to use vertex colours in the current drawing state, or the 
// currently grabbed primitive. Render hints change the way that primitives are rendered, 
// but may have different effects - or no effect on certain primitive types, hence the 
// name hint. Vertex colours override the current (colour) state.
// Example: 
// TODO:example doesnt seem to work
// (hint-vertcols)
// (define mycube (build-cube)) ; make a cube with vertcols enabled
// (grab mycube)
// (pdata-set "c" 0 (vector 0 1 0)) ; set the colour of the first vertex to green
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-vertcols
// Retorna: void
// Descrição:
// Ajusta as dicas de render pra usar cores de vértices no estado de
// desenho corrente, ou da primitiva pega. Dicas de render mudam a forma como
// as primitivas são renderizadas, mas podem ter efeitos diferentes -
// ou nenhum efeito em certas primitivas portanto o nome dicas. Cores
// de vértices modificam o estado atual de (colour).
// Exemplo:
// (hint-vertcols)
// (define mycube (build-cube)) ; faz um cubo com vertcols ativado
// (grab mycube)
// (pdata-set "c" 0 (vector 0 1 0)) ; ajusta a cor do primeiro vert
//                                  ; pra verde
// (ungrab)
// EndFunctionDoc

Scheme_Object *hint_vertcols(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_VERTCOLS;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-box
// Returns: void
// Description: 
// Sets the render hints to bounding box display in the current drawing state, or the 
// currently grabbed primitive. Render hints change the way that primitives are rendered, 
// but may have different effects - or no effect on certain primitive types, hence the 
// name hint. 
// Example:
// (hint-box)
// (build-sphere 10 10) ; make a sphere with bounding box displayed
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-box
// Retorna: void 
// Descrição:
// Ajusta as dicas de render para mostrar a caixa envolvente no estado
// de desenho corrente, ou a primitiva pega. Dicas de render mudam a forma como
// as primitivas são renderizadas, mas podem ter efeitos diferentes -
// ou nenhum efeito em certas primitivas portanto o nome dicas.
// Exemplo:
// (hint-box)
// (build-sphere 10 10) ; faz uma esfera com a bounding box
// EndFunctionDoc

Scheme_Object *hint_box(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_BOUND;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-multitex
// Returns: void
// Description: 
// Sets the render hints to use multitexturing in the current drawing state, or the 
// currently grabbed primitive. Render hints change the way that primitives are rendered, 
// but may have different effects - or no effect on certain primitive types, hence the 
// name hint. 
// Example:
// (hint-multitexture)
// (multitexture 0 (load-texture "tex1.png"))
// (multitexture 1 (load-texture "tex2.png"))
// (build-sphere 10 10) ; make a sphere with overlayed textures
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-multitex
// Retorna: void
// Descrição:
// Ajusta as dicas de render para usar multiplas texturas no estado de
// desenho corrente. Dicas de render mudam a forma como
// as primitivas são renderizadas, mas podem ter efeitos diferentes -
// ou nenhum efeito em certas primitivas portanto o nome dicas.
// Exemplo:
// (hint-multitexture)
// (multitexture 0 (load-texture "tex1.png"))
// (multitexture 1 (load-texture "tex2.png"))
// (build-sphere 10 10) ; faz uma esfera com texturas sobrepostas
// EndFunctionDoc

Scheme_Object *hint_multitex(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_MULTITEX;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-none
// Returns: void
// Description: 
// Clears the render hints in the current drawing state, or the currently grabbed primitive.
// This allows you mainly to get rid of the default solid style, but also means that you can
// turn on and off hints without using push or pop.
// Example:
// (hint-none)
// (hint-wire)
// (build-cube) ; make a cube only visible with wireframe
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-none
// Retorna: void
// Descrição:
// Limpa as dicas de render no estado de desenho corrente, ou da
// primitiva pega. Isso permite que você se livre do estilo sólido
// default, mas também significa que voce pode ligar e desligar dicas
// sem usar push ou pop
// Exemplo:
// (hint-none)
// (hint-wire)
// (build-cube) ; faz um cubo vísivel apenas em wireframe
// EndFunctionDoc

Scheme_Object *hint_none(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints=0;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-origin
// Returns: void
// Description: 
// Sets the render hints to display the object space origin of the
// primitive in the current drawing state, or the 
// currently grabbed primitive. Render hints change the way that primitives are rendered, 
// but may have different effects - or no effect on certain primitive types, hence the 
// name hint. 
// Example:
// (hint-origin)
// (build-sphere 10 10) ; make a sphere with the origin displayed
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-origin
// Retorna: void
// Descrição:
// Ajusta as dicas de render para mostrar a origem espacial do objeto
// no estado de desenho corrente, ou da primitiva pega. Dicas de
// render mudam a forma como  as primitivas são renderizadas,
// mas podem ter efeitos diferentes - ou nenhum efeito em certas
// primitivas portanto o nome dicas.
// Exemplo:
// (hint-origin)
// (build-sphere 10 10) ; make a sphere with the origin displayed
// EndFunctionDoc

Scheme_Object *hint_origin(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_ORIGIN;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-cast-shadow
// Returns: void
// Description: 
// (note: Not yet implemented)
// Sets the render hints to cast shadows for the current drawing state, or the 
// currently grabbed primitive. Render hints change the way that primitives are rendered, 
// but may have different effects - or no effect on certain primitive types, hence the 
// name hint. 
// Example:
// (hint-origin)
// (build-sphere 10 10) ; make a sphere with the origin displayed
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-cast-shadow
// Retorna: void
// Descrição:
// (nota: não implementado ainda)                         
// Ajusta as dicas de render para fazer sombra para o estado de
// desenho atual, ou a primitiva pega. Dicas de render podem mudar a
// forma que as primitivas são renderizadas, mas pode ter efeitos
// diferentes- ou nenhum efeito em certas primitivas, portanto o nome dica.
// Exemplo:
// (hint-origin)
// (build-sphere 10 10) ; make a sphere with the origin displayed
// EndFunctionDoc

Scheme_Object *hint_cast_shadow(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_CAST_SHADOW;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-depth-sort
// Returns: void
// Description: 
// Sets the render hints to depth sort for the current drawing state, or the 
// currently grabbed primitive. Render hints change the way that primitives are rendered, 
// but may have different effects - or no effect on certain primitive types, hence the 
// name hint. 
// Example:
// (hint-depth-sort)
// (build-sphere 10 10) 
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-depth-sort
// Retorna: void
// Descrição:
// Ajusta as dicas de render para separar em profundidade o estado
// corrente, ou a primitiva pega. Dicas de render mudam a forma como
// as primitivas são renderizadas, mas podem ter efeitos diferentes -
// ou nenhum efeito em certas primitivas portanto o nome dicas.
// Exemplo:
// (hint-depth-sort)
// (build-sphere 10 10) 
// EndFunctionDoc

Scheme_Object *hint_depth_sort(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_DEPTH_SORT;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-ignore-depth
// Returns: void
// Description: 
// Sets the render hints to ignore depth tests for the current drawing state, or the 
// currently grabbed primitive. Render hints change the way that primitives are rendered, 
// but may have different effects - or no effect on certain primitive types, hence the 
// name hint. This feature is useful for rendering transparent objects, as it means objects 
// will be shown behind previously rendered ones.
// Example:
// TODO:) examples
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-ignore-depth
// Retorna: void
// Descrição:
// Ajusta as dicas de render para ignorar tested de profundidade no
// estado corrente de desenho, ou da primitiva pega. Dicas de render
// mudam a forma como as primitivas são renderizadas, mas podem ter
// efeitos diferentes - ou nenhum efeito em certas primitivas
// portanto o nome dicas. Essa capacidade é util para renderizar
// objetos transparentes, já que ela significa que objetos vão
// aparecer atrás de outros já renderizados. 
// Exemplo:
// EndFunctionDoc

Scheme_Object *hint_ignore_depth(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_IGNORE_DEPTH;	
    return scheme_void;
}

// StartFunctionDoc-en
// hint-lazy-parent
// Returns: void
// Description: 
// Sets the render hints to prevent this primitive passing it's transform to it's children.
// Render hints change the way that primitives are rendered, 
// but may have different effects - or no effect on certain primitive types, hence the 
// name hint. 
// Example:
// (hint-lazy-parent)
// (build-sphere 10 10) ; make a sphere with the origin displayed
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-lazy-parent
// Retorna: void
// Descrição:
// Ajusta as dicas de render para prevenir esta primitiva de passar
// suas transformações a um filho. Dicas de render
// mudam a forma como as primitivas são renderizadas, mas podem ter
// efeitos diferentes - ou nenhum efeito em certas primitivas
// portanto o nome dicas.
// Exemplo:
// EndFunctionDoc

Scheme_Object *hint_lazy_parent(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_LAZY_PARENT;	
    return scheme_void;
}

// StartFunctionDoc-en
// texture textureid-number
// Returns: void
// Description: 
// Sets the texture of the current drawing state, or the currently 
// grabbed primitive. Texture ids can be generated by the load-texture function.
// Example:
// (texture (load-texture "mytexture.png"))
// (build-sphere 10 10) ; make a sphere textured with mytexture.png
// EndFunctionDoc

// StartFunctionDoc-pt
// texture número-id-textura
// Retorna: void
// Descrição:
// Ajusta a textura do estado corrente, ou da primitiva pega. Ids de
// textura podem ser geradas pela função load-texture.
// Exemplo:
// (texture (load-texture "mytexture.png"))
// (build-sphere 10 10) ; faz uma textura com mytexture.png
// EndFunctionDoc

Scheme_Object *texture(int argc, Scheme_Object **argv)
{
 	DECL_ARGV();
 	ArgCheck("texture", "i", argc, argv);
	Engine::Get()->State()->Textures[0]=(int)IntFromScheme(argv[0]);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// multitexture textureunit-number textureid-number
// Returns: void
// Description: 
// Sets the texture of the current drawing state, or the currently 
// grabbed primitive in the same way as the texture function, but allows you to specify
// the texture unit (0-7) to apply the texture to. Multitexturing allows you to apply different 
// textures and texture coordinates to the same object at once. Texture unit 0 is the default one 
// (which uses the pdata "t" for it's texture coords) texture unit n looks for pdata "tn" - ie 
// multitexture 1 looks for "t1". You need to add these yourself using (pdata-add) or (pdata-copy). 
// Multitexturing is useful when the textures contain alpha, as they can be overlayed, i.e. decals
// placed on background textures.
// Note: fluxus needs to be built using scons MULTITEXTURE=1 to enable this feature.
// Example:
// (define obj (build-sphere 10 10)) ; make a sphere 
// (grab obj)
// (multitexture 0 (load-texture "mytextureA.png")) 
// (multitexture 1 (load-texture "mytextureB.png"))
// (pdata-add "t1" "v")   ; make some texture coords for texture B
// (pdata-copy "t" "t1")  ; copy them from the default texture coords
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-pt
// multitexture número-textureunit número-id-primitiva
// Retorna: void
// Descrição:
// Ajusta a textura do estado corrente de desenho, ou da primitiva
// pega da mesma forma que a função textura, mas permite que você
// especifique a unidade de textura (0-7) para aplicar-lá.Multitextura
// permite você aplicar diferentes texturas e coordenadas ao mesmo
// objeto de uma vez. Unidade de textura 0 é o padrão (que usa a
// pdata "t" para suas coordenadas) unidade de textura n olha pela
// pdata "tn" - ie multitexture 1 olha por "t1". Você precisa
// adicionar estas você mesmo usando (pdata-add) ou
// (pdata-copy). Multitexturas é útil quando a textura contém alpha,
// já que elas podem ser sobrepostas, por exemplo adesivos colocados
// em texturas de fundo.
// Nota: Fluxus precisa ser compilado usando scons MULTITEXTURE=1 para
// ativar essa capacidade. 
// Exemplo:
// (define obj (build-sphere 10 10)) ; make a sphere 
// (grab obj)
// (multitexture 0 (load-texture "mytextureA.png")) 
// (multitexture 1 (load-texture "mytextureB.png"))
// (pdata-add "t1" "v")   ; make some texture coords for texture B
// (pdata-copy "t" "t1")  ; copy them from the default texture coords
// (ungrab)
// EndFunctionDoc

Scheme_Object *multitexture(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
  	ArgCheck("multitexture", "ii", argc, argv);
	Engine::Get()->State()->Textures[IntFromScheme(argv[0])]=IntFromScheme(argv[1]);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// print-scene-graph 
// Returns: void
// Description: 
// Prints out the current scene graph, useful for debugging.
// Example:
// (print-scene-graph) ; exciting...
// EndFunctionDoc

// StartFunctionDoc-pt
// print-scene-graph
// Retorna: void
// Descrição:
// Mostra o scene graph corrente, útil para debug.
// Exemplo:
// (print-scene-graph) ; exciting...
// EndFunctionDoc

Scheme_Object *print_scene_graph(int argc, Scheme_Object **argv)
{
	Engine::Get()->Renderer()->GetSceneGraph().Dump();
	return scheme_void;
}

// StartFunctionDoc-en
// hide hidden-number
// Returns: void
// Description: 
// Sets the hidden state for the grabbed primitive (also affects all child primitives). Hidden primitives
// can be treated as normal in every way - they just won't be rendered.
// Example:
// (define obj (build-cube))
// (grab obj)
// (hide 1) ; hide this cube
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-pt
// hide número-escondido
// Retorna: void
// Descrição:
// Ajusta o estado escondido para a primitiva pega (tbm afeta todas as
// primitivas filhas). Primitivas escondidas podem ser tratadas
// normalmente em todas as formas - elas apenas não serão renderizadas.
// Exemplo:
// (define obj (build-cube))
// (grab obj)
// (hide 1) ; hide this cube
// (ungrab)
// EndFunctionDoc

Scheme_Object *hide(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
  	ArgCheck("hide", "i", argc, argv);
	if (Engine::Get()->Grabbed()) 
	{
		Engine::Get()->Grabbed()->Hide(FloatFromScheme(argv[0]));
	}
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// selectable selectable-number
// Returns: void
// Description: 
// Sets whether the grabbed primitive can be selected or not using the select command.
// Example:
// (define obj (build-cube))
// (grab obj)
// (selectable 0) ; now it won't be "seen" by calling select
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-pt
// selectable número-selecionável
// Retorna: void
// Descrição:
// Ajusta se a primitiva pega pode ser selecionada ou não.
// Exemplo:
// (define obj (build-cube))
// (grab obj)
// (selectable 0) ; agora ela nao vai ser "vista", quando chamar select
// (ungrab)
// EndFunctionDoc

Scheme_Object *selectable(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
  	ArgCheck("selectable", "i", argc, argv);
	if (Engine::Get()->Grabbed()) 
	{
		Engine::Get()->Grabbed()->Selectable(FloatFromScheme(argv[0]));
	}
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// shader vertexprogram-string fragmentprogram-string
// Returns: void
// Description: 
// Loads, compiles and sets the GLSL harware shader pair for the current drawing state, or the 
// currently grabbed primitive. Requires OpenGL 2 support.
// The shader's uniform data can be controlled via shader-set! and all the pdata is sent through as 
// per-vertex attribute data to the shader.
// Example:
// (push)
// ; assign the shaders to the surface
// (shader "simplevert.glsl" "simplefrag.glsl")
// (define s (build-sphere 20 20))
// (pop)
// 
// (grab s)
// ; add and set the pdata - this is then picked up in the vertex shader 
// ; as an input attribute called "testcol"
// (pdata-add "testcol" "v")
// (set-cols (pdata-size))
// (ungrab)
// 
// (define (animate)
//     (grab s)
// 	; animate the deformamount uniform input parameter 
//     (shader-set! (list "deformamount" (cos (time))))
//     (ungrab))
// 
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-pt
// shader vertexprograma-string fragmentprogram-string
// Retorna: void
// Descrição:
// Abre, compila e ajusta o pard de GLSL hardware shader para o estado
// de desenho atual, ou a primitiva pega. Requer OpenGL 2.
// Os dados uniformes do shader podem ser controlados via shader-set!
// e todas as pdatas são enviadas como um atributo por vértice ao
// shader. 
// Exemplo:
// (push)
// ; assign the shaders to the surface
// (shader "simplevert.glsl" "simplefrag.glsl")
// (define s (build-sphere 20 20))
// (pop)
// 
// (grab s)
// ; add and set the pdata - this is then picked up in the vertex shader 
// ; as an input attribute called "testcol"
// (pdata-add "testcol" "v")
// (set-cols (pdata-size))
// (ungrab)
// 
// (define (animate)
//     (grab s)
// 	; animate the deformamount uniform input parameter 
//     (shader-set! (list "deformamount" (cos (time))))
//     (ungrab))
// 
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *shader(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
  	ArgCheck("shader", "ss", argc, argv);
	
	string vert=StringFromScheme(argv[0]);
	string frag=StringFromScheme(argv[1]);
	
 	GLSLShader *shader = new GLSLShader(vert,frag);
	
    // remove the old one
	if (Engine::Get()->State()->Shader)
	{
		delete Engine::Get()->State()->Shader;
	}
		
	Engine::Get()->State()->Shader=shader;
	
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// shader-set! argument-list
// Returns: void
// Description: 
// Sets the uniform shader parameters for the GLSL shader. The list consists of token-string value 
// pairs, which relate to the corresponding shader parameters names and values. 
// Example:
// (push)
// ; assign the shaders to the surface
// (shader "simplevert.glsl" "simplefrag.glsl")
// (define s (build-sphere 20 20))
// (pop)
// 
// (grab s)
// ; add and set the pdata - this is then picked up in the vertex shader 
// ; as an input attribute called "testcol"
// (pdata-add "testcol" "v")
// (set-cols (pdata-size))
// (ungrab)
// 
// (define (animate)
//     (grab s)
// 	; animate the deformamount uniform input parameter 
//     (shader-set! (list "deformamount" (cos (time))))
//     (ungrab))
// 
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-pt
// shader-set! lista-argumentos
// Retorna: void
// Descrição:
// Ajusta os parâmetros do shader uniforme para o shader GLSL. A lista
// contém valores pares simbolos-strings, que relacionam os parâmetros
// de shader correspondentes nomes e valores
// Exemplo:
// (push)
// ; assign the shaders to the surface
// (shader "simplevert.glsl" "simplefrag.glsl")
// (define s (build-sphere 20 20))
// (pop)
// 
// (grab s)
// ; add and set the pdata - this is then picked up in the vertex shader 
// ; as an input attribute called "testcol"
// (pdata-add "testcol" "v")
// (set-cols (pdata-size))
// (ungrab)
// 
// (define (animate)
//     (grab s)
// 	; animate the deformamount uniform input parameter 
//     (shader-set! (list "deformamount" (cos (time))))
//     (ungrab))
// 
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *shader_set(int argc, Scheme_Object **argv)
{	
	Scheme_Object *paramvec = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, paramvec);
	MZ_GC_REG();
			
   	ArgCheck("shader-set!", "l", argc, argv);
	GLSLShader *shader=Engine::Get()->State()->Shader;

	if (shader)
	{
		// vectors seem easier to handle than lists with this api
		paramvec = scheme_list_to_vector(argv[0]);

		// apply to set parameters
		shader->Apply();

		for (int n=0; n<SCHEME_VEC_SIZE(paramvec); n+=2)
		{
			if (SCHEME_CHAR_STRINGP(SCHEME_VEC_ELS(paramvec)[n]))
			{
				// get the parameter name
				string param = StringFromScheme(SCHEME_VEC_ELS(paramvec)[n]);

				if (SCHEME_NUMBERP(SCHEME_VEC_ELS(paramvec)[n+1]))
				{
					if (SCHEME_EXACT_INTEGERP(SCHEME_VEC_ELS(paramvec)[n+1])) 
					{
						shader->SetInt(param,IntFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]));
					}
					else 
					{
						shader->SetFloat(param,(float)FloatFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]));
					}
				}
				else if (SCHEME_VECTORP(SCHEME_VEC_ELS(paramvec)[n+1]))
				{
					if (SCHEME_VEC_SIZE(SCHEME_VEC_ELS(paramvec)[n+1]) == 3)
					{
						dVector vec;
						FloatsFromScheme(SCHEME_VEC_ELS(paramvec)[n+1],vec.arr(),3);
						shader->SetVector(param,vec);
					}
					else if (SCHEME_VEC_SIZE(SCHEME_VEC_ELS(paramvec)[n+1]) == 4)
					{
						dColour vec;
						FloatsFromScheme(SCHEME_VEC_ELS(paramvec)[n+1],vec.arr(),4);
						shader->SetColour(param,vec);
					}
					else
					{	
						Trace::Stream<<"shader has found an argument vector of a strange size"<<endl;
					}
				}
				else
				{
					Trace::Stream<<"shader has found an argument type it can't send, numbers and vectors only"<<endl;
				}
			}
			else
			{
				Trace::Stream<<"shader has found a mal-formed parameter list"<<endl;
			}
		}
		GLSLShader::Unapply();
	}   
	
	MZ_GC_UNREG(); 
	return scheme_void;
}

void LocalStateFunctions::AddGlobals(Scheme_Env *env)
{	
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();
	// renderstate operations
	scheme_add_global("push",scheme_make_prim_w_arity(push,"push",0,0), env);
	scheme_add_global("pop",scheme_make_prim_w_arity(pop,"pop",0,0), env);
	scheme_add_global("grab",scheme_make_prim_w_arity(grab,"grab",1,1), env);
    scheme_add_global("ungrab",scheme_make_prim_w_arity(ungrab,"ungrab",0,0), env);
    scheme_add_global("print-scene-graph",scheme_make_prim_w_arity(print_scene_graph,"print-scene-graph",0,0), env);
	scheme_add_global("apply-transform",scheme_make_prim_w_arity(apply,"apply",1,1), env);
	scheme_add_global("identity",scheme_make_prim_w_arity(flux_identity,"identity",0,0), env);
	scheme_add_global("concat",scheme_make_prim_w_arity(concat,"concat",1,1), env);
    scheme_add_global("translate",scheme_make_prim_w_arity(translate,"translate",1,1), env);
    scheme_add_global("rotate",scheme_make_prim_w_arity(rotate,"rotate",1,1), env);
    scheme_add_global("scale",scheme_make_prim_w_arity(scale,"scale",1,1), env);
	scheme_add_global("get-transform", scheme_make_prim_w_arity(get_transform, "get-transform", 0, 0), env);
    scheme_add_global("colour",scheme_make_prim_w_arity(colour,"colour",1,1), env);
    scheme_add_global("wire-colour",scheme_make_prim_w_arity(wire_colour,"wire-colour",1,1), env);
    scheme_add_global("opacity",scheme_make_prim_w_arity(opacity,"opacity",1,1), env);
    scheme_add_global("wire-opacity",scheme_make_prim_w_arity(wire_opacity,"wire-opacity",1,1), env);
    scheme_add_global("specular",scheme_make_prim_w_arity(specular,"specular",1,1), env);
    scheme_add_global("ambient",scheme_make_prim_w_arity(ambient,"ambient",1,1), env);
    scheme_add_global("emissive",scheme_make_prim_w_arity(emissive,"emissive",1,1), env);
	scheme_add_global("shinyness",scheme_make_prim_w_arity(shinyness,"shinyness",1,1), env);
	scheme_add_global("texture",scheme_make_prim_w_arity(texture,"texture",1,1), env);
	scheme_add_global("multitexture",scheme_make_prim_w_arity(multitexture,"multitexture",2,2), env);
    scheme_add_global("hint-solid",scheme_make_prim_w_arity(hint_solid,"hint-solid",0,0), env);
    scheme_add_global("hint-wire",scheme_make_prim_w_arity(hint_wire,"hint-wire",0,0), env);
    scheme_add_global("hint-normal",scheme_make_prim_w_arity(hint_normal,"hint-normal",0,0), env);
    scheme_add_global("hint-points",scheme_make_prim_w_arity(hint_points,"hint-points",0,0), env);
    scheme_add_global("hint-anti-alias",scheme_make_prim_w_arity(hint_anti_alias,"hint-anti-alias",0,0), env);
    scheme_add_global("hint-none",scheme_make_prim_w_arity(hint_none,"hint-none",0,0), env);
    scheme_add_global("hint-unlit",scheme_make_prim_w_arity(hint_unlit,"hint-unlit",0,0), env);
    scheme_add_global("hint-vertcols",scheme_make_prim_w_arity(hint_vertcols,"hint-vertcols",0,0), env);
    scheme_add_global("hint-box",scheme_make_prim_w_arity(hint_box,"hint-box",0,0), env);
    scheme_add_global("hint-multitex",scheme_make_prim_w_arity(hint_multitex,"hint-multitex",0,0), env);
    scheme_add_global("hint-origin",scheme_make_prim_w_arity(hint_origin,"hint-origin",0,0), env);
    scheme_add_global("hint-cast-shadow",scheme_make_prim_w_arity(hint_cast_shadow,"hint-cast-shadow",0,0), env);
    scheme_add_global("hint-ignore-depth",scheme_make_prim_w_arity(hint_ignore_depth,"hint-ignore-depth",0,0), env);
    scheme_add_global("hint-depth-sort",scheme_make_prim_w_arity(hint_depth_sort,"hint-depth-sort",0,0), env);
    scheme_add_global("hint-lazy-parent",scheme_make_prim_w_arity(hint_lazy_parent,"hint-lazy-parent",0,0), env);
	scheme_add_global("line-width",scheme_make_prim_w_arity(line_width,"line-width",1,1), env);
	scheme_add_global("point-width",scheme_make_prim_w_arity(point_width,"point-width",1,1), env);
	scheme_add_global("blend-mode",scheme_make_prim_w_arity(blend_mode,"blend-mode",2,2), env);
    scheme_add_global("parent",scheme_make_prim_w_arity(parent,"parent",1,1), env);
    scheme_add_global("parent",scheme_make_prim_w_arity(parent,"parent",1,1), env);
	scheme_add_global("hide",scheme_make_prim_w_arity(hide,"hide",1,1), env);
	scheme_add_global("selectable",scheme_make_prim_w_arity(selectable,"selectable",1,1), env);
	scheme_add_global("shader",scheme_make_prim_w_arity(shader,"shader",2,2), env);
	scheme_add_global("shader-set!",scheme_make_prim_w_arity(shader_set,"shader-set!",1,1), env);
 	MZ_GC_UNREG(); 
}
