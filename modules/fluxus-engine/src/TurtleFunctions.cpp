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
#include "TurtleFunctions.h"
#include "Renderer.h"

using namespace SchemeHelper;
using namespace Fluxus;

// StartSectionDoc-en
// turtle
// The turtle polybuilder is an experimental way of building polygonal objects using a logo style turtle in 
// 3D space. As you drive the turtle around you can place vertices and build shapes procedurally. The turtle 
// can also be used to deform existing polygonal primitives, by attaching it to objects you have already created. 
// Example:
// (define (build n)
//     (turtle-reset)
//     (turtle-prim 4)
//     (build-loop n n)
//     (turtle-build))
// 
// (define (build-loop n t)
//     (turtle-turn (vector 0 (/ 360 t) 0))
//     (turtle-move 1)
//     (turtle-vert)
//     (if (< n 1)
//         0
//         (build-loop (- n 1) t)))
// EndSectionDoc 

// StartSectionDoc-pt
// tartaruga
// O construtor polygonal Turtle é um modo experimental de construir
// objetos poligonais usando uma tartaruga do estilo logo no espaço
// 3d. Em que você pode dirigir a tartaruga ao redor colocando vértices
// e construindo formas proceduralmente. A tartaruga também pode ser usa
// para deformar objetos poligonais existentes, fixando ela a objetos
// que você já criou.
// Exemplo:
// (define (build n)
//     (turtle-reset)
//     (turtle-prim 4)
//     (build-loop n n)
//     (turtle-build))
// 
// (define (build-loop n t)
//     (turtle-turn (vector 0 (/ 360 t) 0))
//     (turtle-move 1)
//     (turtle-vert)
//     (if (< n 1)
//         0
//         (build-loop (- n 1) t)))
// EndSectionDoc

// StartFunctionDoc-en
// turtle-prim type-number
// Returns: void
// Description:
// Starts building a new polygon primitive with the turtle. The type specifies the polygon face type and is one 
// of the following: 0: triangle strip, 1: quad list, 2: triangle list, 3: triangle fan, 4: general polygon
// Example:
// (turtle-prim 0)
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-prim número-tipo
// Retorna: void
// Descrição:
// Inicia a construção de uma nova primitiva poligonal com a
// tartaruga. O tipo especifica o tipo da face do poligono e é um dos
// seguintes: 0: triangle strip, 1: quad list, 2: triangle list, 3: triangle fan, 4: general polygon
// Exemplo:
// (turtle-prim 0)
// EndFunctionDoc

Scheme_Object *turtle_prim(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("turtle-prim", "i", argc, argv);		
	Engine::Get()->GetTurtle()->Prim(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// turtle-vert 
// Returns: void
// Description:
// Creates a new vertex in the current position, or sets the current vertex if the turtle builder is attached.
// Example:
// (turtle-vert)
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-vert
// Retorna: void
// Descrição:
// Cria um novo vértice na posição atual, ou ajusta o vértice atual se o
// construtor tartaruga estiver fixado.
// Exemplo:
// (turtle-vert)
// EndFunctionDoc

Scheme_Object *turtle_vert(int argc, Scheme_Object **argv)
{
	Engine::Get()->GetTurtle()->Vert();
	return scheme_void;
}

// StartFunctionDoc-en
// turtle-build 
// Returns: primitiveid-number
// Description:
// Builds the object with the vertex list defined and gives it to the renderer. 
// Has no effect if the turtle builder is attached to a primitive.
// Example:
// (define mynewshape (turtle-build))
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-build
// Retorna: void
// Descrição:
// Constrói o objeto com a lista de vértices definida e dá isto ao
// renderizador. Não tem efeito se o construtor tartaruga estive fixado
// a uma primitiva.
// Exemplo:
// (define mynewshape (turtle-build))
// EndFunctionDoc

Scheme_Object *turtle_build(int argc, Scheme_Object **argv)
{
	return scheme_make_integer_value(Engine::Get()->GetTurtle()->Build(Engine::Get()->Renderer()));
}

// StartFunctionDoc-en
// turtle-move distance-number
// Returns: void
// Description:
// Moves the turtle forward in it's current orientation.
// Example:
// (turtle-move 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-move número-distância
// Retorna: void
// Descrição:
// Move a tartaruga pra frente na sua orientação atual.
// Exemplo:
// (turtle-move 1)
// EndFunctionDoc

Scheme_Object *turtle_move(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("turtle-move", "f", argc, argv);		
	Engine::Get()->GetTurtle()->Move(FloatFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// turtle-push
// Returns: void
// Description:
// The turtle build has it's own transform stack. Push remembers the current position and orientation.
// Example:
// (turtle-push)
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-push
// Retorna: void
// Descrição:
// O construtor tartaruga tem sua própria pilha de transformações, Push
// lembra a posição atual e orientação.
// Exemplo:
// (turtle-push)
// EndFunctionDoc

Scheme_Object *turtle_push(int argc, Scheme_Object **argv)
{
	Engine::Get()->GetTurtle()->Push();
	return scheme_void;
}

// StartFunctionDoc-en
// turtle-pop
// Returns: void
// Description:
// The turtle build has it's own transform stack. Pop forgets the current position and orientation, and 
// goes back to the state at the last push.
// Example:
// (turtle-pop)
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-pop
// Retorna: void
// Descrição:
// O construtor tartaruga tem sua própria pilha de transformações. Pop
// esquece a posição atual e orientação, e vai de volta para o estado do
// último push.
// Exemplo:
// (turtle-pop)
// EndFunctionDoc

Scheme_Object *turtle_pop(int argc, Scheme_Object **argv)
{
	Engine::Get()->GetTurtle()->Pop();
	return scheme_void;
}

// StartFunctionDoc-en
// turtle-turn rotation-vector
// Returns: void
// Description:
// Rotates the turtle's orientation with the supplied euler angles (rotations in x, y and z).
// Example:
// (turtle-turn (vector 45 0 0))
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-turn vetor-rotação
// Retorna: void
// Descrição:
// Rotaciona a orientação da tartaruga com o ângulos euler fornecidos
// (rotações em x, y e z)
// Exemplo:
// (turtle-turn (vector 45 0 0))
// EndFunctionDoc

Scheme_Object *turtle_turn(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("turtle-turn", "v", argc, argv);		
	float rot[3];
	FloatsFromScheme(argv[0],rot,3);
	Engine::Get()->GetTurtle()->Turn(dVector(rot[0],rot[1],rot[2]));
	MZ_GC_UNREG(); 
	return scheme_void;	
}

// StartFunctionDoc-en
// turtle-reset 
// Returns: void
// Description:
// Resets the current position and rotation of the turtle to the origin.
// Example:
// (turtle-reset)
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-reset
// Retorna: void
// Descrição:
// Reinicia a atual posição e rotação da tartaruga para a origem.
// Exemplo:
// (turtle-reset)
// EndFunctionDoc

Scheme_Object *turtle_reset(int argc, Scheme_Object **argv)
{
	Engine::Get()->GetTurtle()->Reset();
	return scheme_void;	
}

// StartFunctionDoc-en
// turtle-attach primitiveid-number
// Returns: void
// Description:
// Attaches the turtle to an existing poly primitive. This means you are able to 
// deform an existing objects points using the turtle builder.
// Example:
// (define myshape (build-sphere 10 10))
// (turtle-attach myshape)
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-attach número-id-primitiva
// Retorna: void
// Descrição:
// Anexa a tartaruga a uma primitiva poligonal existente. Isso significa
// que você será capaz de deformar pontos de objetos existentes usando o
// construtor tartaruga. 
// Exemplo:
// (define myshape (build-sphere 10 10))
// (turtle-attach myshape)
// EndFunctionDoc

Scheme_Object *turtle_attach(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("turtle-attach", "i", argc, argv);		
	PolyPrimitive *poly = dynamic_cast<PolyPrimitive*>(Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0])));
	if (poly)
	{
		Engine::Get()->GetTurtle()->Attach(poly);
	}
	else
	{
		Trace::Stream<<"turtle-attach only works on polys"<<endl;
	}
	MZ_GC_UNREG(); 
	
	return scheme_void;	
}

// StartFunctionDoc-en
// turtle-skip count-number
// Returns: void
// Description:
// When attached, causes the turtle to skip vertices. This value may be negative, which will set 
// the turtle to write to previous vertices.
// Example:
// (turtle-skip -1)
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-skip número-contador
// Retorna: void
// Descrição:
// Quando anexado, faz com que a tartaruga pule vértices. Este valor
// pode ser negativo, o que vai resultar na tartaruga escrevendo para
// vértices anteriores.
// Exemplo:
// (turtle-skip -1)
// EndFunctionDoc

Scheme_Object *turtle_skip(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("turtle-skip", "i", argc, argv);		
	Engine::Get()->GetTurtle()->Skip(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_void;	
}

// StartFunctionDoc-en
// turtle-position
// Returns: count-number
// Description:
// When attached, returns the current pdata index the turtle is writing to.
// Example:
// (display (turtle-position))(newline)
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-position
// Retorna: void
// Descrição:
// Quando anexado, retorna o índice atual da pdata que a tartaruga esta
// escrevendo para.
// Exemplo:
// (display (turtle-position))(newline)
// EndFunctionDoc

Scheme_Object *turtle_position(int argc, Scheme_Object **argv)
{
	return scheme_make_integer_value(Engine::Get()->GetTurtle()->Position());
}

// StartFunctionDoc-en
// turtle-seek position-number
// Returns: void
// Description:
// When attached, sets the absolute pdata index the turtle is writing to.
// Example:
// (turtle-seek 0)
// EndFunctionDoc

// StartFunctionDoc-pt
// turtle-seek número-posição
// Retorna: void
// Descrição:
// Quando anexado, ajusta o índice absoluto da pdata que a tartaruga
// está escrevendo para.
// Exemplo:
// (turtle-seek 0)
// EndFunctionDoc

Scheme_Object *turtle_seek(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("turtle-seek", "i", argc, argv);		
	Engine::Get()->GetTurtle()->SetPosition(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_void;	
}

void TurtleFunctions::AddGlobals(Scheme_Env *env)
{	
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();
	scheme_add_global("turtle-prim", scheme_make_prim_w_arity(turtle_prim, "turtle-prim", 1, 1), env);
	scheme_add_global("turtle-vert", scheme_make_prim_w_arity(turtle_vert, "turtle-vert", 0, 0), env);
	scheme_add_global("turtle-build", scheme_make_prim_w_arity(turtle_build, "turtle-build", 0, 0), env);
	scheme_add_global("turtle-move", scheme_make_prim_w_arity(turtle_move, "turtle-move", 1, 1), env);
	scheme_add_global("turtle-push", scheme_make_prim_w_arity(turtle_push, "turtle-push", 0, 0), env);
	scheme_add_global("turtle-pop", scheme_make_prim_w_arity(turtle_pop, "turtle-pop", 0, 0), env);
	scheme_add_global("turtle-turn", scheme_make_prim_w_arity(turtle_turn, "turtle-turn", 1, 1), env);
	scheme_add_global("turtle-reset", scheme_make_prim_w_arity(turtle_reset, "turtle-reset", 0, 0), env);
	scheme_add_global("turtle-attach", scheme_make_prim_w_arity(turtle_attach, "turtle-attach", 1, 1), env);
	scheme_add_global("turtle-skip", scheme_make_prim_w_arity(turtle_skip, "turtle-skip", 1, 1), env);
	scheme_add_global("turtle-position", scheme_make_prim_w_arity(turtle_position, "turtle-position", 0, 0), env);
	scheme_add_global("turtle-seek", scheme_make_prim_w_arity(turtle_seek, "turtle-seek", 1, 1), env);
 	MZ_GC_UNREG(); 
}
