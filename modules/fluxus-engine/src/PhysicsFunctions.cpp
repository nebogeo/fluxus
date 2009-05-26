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
#include "PhysicsFunctions.h"
#include "Physics.h"
#include "dada.h"

using namespace PhysicsFunctions;
using namespace SchemeHelper;
using namespace Fluxus;

// StartSectionDoc-en
// physics
// The physics system used in fluxus is based on the ode library, which allows you to add 
// physical properties to objects and set them in motion. Since ODE is designed for 
// rigid-body simulations, structures are described in terms of objects, joints and 
// forces.
//
// A much more comprehensive explanation of these concepts can be found in the ODE 
// documentation, which you have probably downloaded if you have compiled fluxus, or can 
// be found at @url{http://ode.org/ode-docs.html}
// 
// To help with debugging joints, try calling (render-physics) every frame, which will 
// render locators showing you positions and axes of joints that have positional 
// information.
// Example:
// EndSectionDoc 

// StartSectionDoc-pt
// fisica
// O sistema de física em fluxus é baseado na biblioteca ODE, que
// permite a você adicionar propriedades físicas a objetos e
// colocá-los em movimento. Já que ODE foi projetada para simulações
// em corpos rígidos, estruturas são descritas em termos de objetos,
// dobras e forças.
// 
// Uma explicação muito mais compreensiva destes conceitos pode ser
// encontrada na documentação de ODE, que você provavelmente baixou se
// teve que compilar fluxus, ou pode ser encontrado em
// @url{http://ode.org/ode-docs.html}
//
// Para ajudar com a depuração de dobras, tente chamar
// (render-physics) em cada frame, o que vai renderizar locators
// mostrando a você posições e eixos de dobras que possuem informação
// posicional.
// Exemplo:
// EndSectionDoc

// StartFunctionDoc-en
// collisions on/off-number
// Returns: void
// Description:
// Enables or disables collision detection. Defaults to off.
// Example:
// (collisions 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// collisions on/off-número
// Retorna: void
// Descrição:
// Habilita ou desabilita detecção de colisão. Padrão é desligado.
// Exemplo:
// (collisions 1)
// EndFunctionDoc

Scheme_Object *collisions(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("collisions", "i", argc, argv);
	Engine::Get()->Physics()->SetCollisions(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// ground-plane plane-vector offset-number
// Returns: void
// Description:
// Create an infinite passive plane for use as the 'ground'
// Example:
// (ground-plane (vector 0 1 0) 0)
// EndFunctionDoc

// StartFunctionDoc-pt
// ground-plane vetor-plano número-offset
// Retorna: void
// Descrição:
// Cria um plano passivo infinito pra usar como 'chão'.
// Exemplo:
// (ground-plane (vector 0 1 0) 0)
// EndFunctionDoc

Scheme_Object *ground_plane(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("ground-plane", "vf", argc, argv);
    float ori[3];
	FloatsFromScheme(argv[0],ori,3);
	Engine::Get()->Physics()->GroundPlane(dVector(ori[0],ori[1],ori[2]),FloatFromScheme(argv[1]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// active-box primitiveid-number
// Returns: void
// Description:
// Enable the object to be acted upon by the physics system, using a box as the bounding volume. As 
// an active object, it will be transformed by ode. Note: rotations only work correctly if 
// you specify your transforms scale first, then rotate (translate doesn't matter) 
// basically, ode can't deal with shearing transforms. 
// Example:
// (define mycube (build-cube))
// (active-box mycube)
// EndFunctionDoc

// StartFunctionDoc-pt
// active-box número-id-primitiva
// Retorna: void
// Descrição:
// Permite que o objeto seja afetado pelo sistema físico, usando uma
// caixa como um volume limite. Como um objeto ativo, ele vai ser
// transformado por ODE. Nota: rotações só funcionam corretamente se
// você específicar suas escalagens primeiro, depois rotacionar
// (translação não importa) basicamente, ode não pode lidar com
// tranformações cortadas.
// Exemplo:
// (define mycube (build-cube))
// (active-box mycube)
// EndFunctionDoc

Scheme_Object *active_box(int argc, Scheme_Object **argv)
{
 	DECL_ARGV();
	ArgCheck("active-box", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakeActive(name,1.0f,Physics::BOX);
 	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// active-cylinder primitiveid-number
// Returns: void
// Description:
// Enable the object to be acted upon by the physics system, using a cylinder as the bounding volume. As 
// an active object, it will be transformed by ode. Note: rotations only work correctly if 
// you specify your transforms scale first, then rotate (translate doesn't matter) 
// basically, ode can't deal with shearing transforms. 
// Example:
// (define mycube (build-cube))
// (active-cylinder mycube)
// EndFunctionDoc

// StartFunctionDoc-pt
// active-cylinder número-id-primitiva
// Retorna: void
// Descrição:
// Permite que o objeto seja afetado pelo sistema físico, usando um
// cilindro como um volume limite. Como um objeto ativo, ele vai ser
// transformado por ODE. Nota: rotações só funcionam corretamente se
// você específicar suas escalagens primeiro, depois rotacionar
// (translação não importa) basicamente, ode não pode lidar com
// tranformações cortadas. 
// Exemplo:
// (define mycube (build-cube))
// (active-cylinder mycube)
// EndFunctionDoc

Scheme_Object *active_cylinder(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("active-cylinder", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakeActive(name,1.0f,Physics::CYLINDER);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// active-sphere primitiveid-number
// Returns: void
// Description:
// Enable the object to be acted upon by the physics system, using a sphere as the bounding volume. As 
// an active object, it will be transformed by ode. Note: rotations only work correctly if 
// you specify your transforms scale first, then rotate (translate doesn't matter) 
// basically, ode can't deal with shearing transforms. 
// Example:
// (define mycube (build-cube))
// (active-sphere mycube)
// EndFunctionDoc

// StartFunctionDoc-pt
// active-sphere número-id-primitiva
// Retorna: void
// Descrição:
// Permite que o objeto seja afetado pelo sistema físico, usando uma
// esfera como um volume limite. Como um objeto ativo, ele vai ser
// transformado por ODE. Nota: rotações só funcionam corretamente se
// você específicar suas escalagens primeiro, depois rotacionar
// (translação não importa) basicamente, ode não pode lidar com
// tranformações cortadas. 
// Exemplo:
// (define mycube (build-cube))
// (active-sphere mycube)
// EndFunctionDoc

Scheme_Object *active_sphere(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("active-sphere", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakeActive(name,1.0f,Physics::SPHERE);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// passive-box primitiveid-number
// Returns: void
// Description:
// Enable the object to be acted upon by the physics system, using a box as the bounding volume. As 
// a passive object, active objects will collide with it, but it will not be transformed. 
// Note: rotations only work correctly if you specify your transforms scale first, then 
// rotate (translate doesn't matter) basically, ode can't deal with shearing transforms. 
// Example:
// (define mycube (build-cube))
// (passive-box mycube)
// EndFunctionDoc

// StartFunctionDoc-pt
// passive-box número-id-primitiva
// Retorna: void
// Descrição:
// Permite que o objeto seja resolvido pelo sistema de física, usando
// uma caixa como o limite. Como um objeto passivo, objetos ativos vão
// colidir com ele, mas este não vai ser transformado.
// Nota: rotações só funcionam corretamente se você especificar suas
// transformaçoes de escala primeiro, depois rotacionar (translação
// não importa) basicamente, ode não pode lidar com transformações
// cortadas. 
// Exemplo:
// (define mycube (build-cube))
// (passive-box mycube)
// EndFunctionDoc

Scheme_Object *passive_box(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("passive-sphere", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakePassive(name,1.0f,Physics::BOX);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// passive-cylinder primitiveid-number
// Returns: void
// Description:
// Enable the object to be acted upon by the physics system, using a cylinder as the bounding volume. As 
// a passive object, active objects will collide with it, but it will not be transformed. 
// Note: rotations only work correctly if you specify your transforms scale first, then 
// rotate (translate doesn't matter) basically, ode can't deal with shearing transforms. 
// Example:
// (define mycube (build-cube))
// (passive-cylinder mycube)
// EndFunctionDoc

// StartFunctionDoc-pt
// passive-cylinder número-id-primitiva
// Retorna: void
// Descrição:
// Permite que o objeto seja resolvido pelo sistema de física, usando
// um cilindro como o limite. Como um objeto passivo, objetos ativos vão
// colidir com ele, mas este não vai ser transformado.
// Nota: rotações só funcionam corretamente se você especificar suas
// transformaçoes de escala primeiro, depois rotacionar (translação
// não importa) basicamente, ode não pode lidar com transformações
// cortadas.  
// Exemplo:
// (define mycube (build-cube))
// (passive-cylinder mycube)
// EndFunctionDoc

Scheme_Object *passive_cylinder(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("passive-cylinder", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakePassive(name,1.0f,Physics::CYLINDER);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// passive-sphere primitiveid-number
// Returns: void
// Description:
// Enable the object to be acted upon by the physics system, using a sphere as the bounding volume. As 
// a passive object, active objects will collide with it, but it will not be transformed. 
// Note: rotations only work correctly if you specify your transforms scale first, then 
// rotate (translate doesn't matter) basically, ode can't deal with shearing transforms. 
// Example:
// (define mycube (build-cube))
// (passive-sphere mycube)
// EndFunctionDoc

// StartFunctionDoc-pt
// passive-sphere número-id-primitiva
// Retorna: void
// Descrição:
// Permite que o objeto seja resolvido pelo sistema de física, usando
// uma esfera como o limite. Como um objeto passivo, objetos ativos vão
// colidir com ele, mas este não vai ser transformado.
// Nota: rotações só funcionam corretamente se você especificar suas
// transformaçoes de escala primeiro, depois rotacionar (translação
// não importa) basicamente, ode não pode lidar com transformações
// cortadas. 
// Exemplo:
// (define mycube (build-cube))
// (passive-sphere mycube)
// EndFunctionDoc

Scheme_Object *passive_sphere(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("passive-sphere", "i", argc, argv);
	int name=IntFromScheme(argv[0]);	
	Engine::Get()->Physics()->MakePassive(name,1.0f,Physics::SPHERE);
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// physics-remove primitiveid-number
// Returns: void
// Description:
// Remove the object from the physics system.
// Example:
// (define mycube (build-cube))
// (active-box mycube)
// (physics-remove mycube)
// EndFunctionDoc

Scheme_Object *physics_remove(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("physics-remove", "i", argc, argv);
	int name = IntFromScheme(argv[0]);
	Engine::Get()->Physics()->Free(name);
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// surface-params slip1-number slip2-number softerp-number softcfm-number
// Returns: void
// Description:
// Sets some global surface attributes that affect friction and bouncyness. see section 
// 7.3.7 of the ODE docs for an explanation of these parameters 
// Example:
// (surface-params 0.1 0.1 0.1 0.1)
// EndFunctionDoc

// StartFunctionDoc-pt
// surface-params slip1-número slip2-número softerp-número softcfm-número
// Retorna: void
// Descrição:
// Ajusta alguns parâmetros globais de superfície que afetam o atrito
// e reflexão. Veja seção 7.3.7 dos docs de ODE para uma explicação
// desses parâmetros.
// Exemplo:
// (surface-params 0.1 0.1 0.1 0.1)
// EndFunctionDoc

Scheme_Object *surface_params(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("surface-params", "ffff", argc, argv);
	Engine::Get()->Physics()->SetGlobalSurfaceParams(FloatFromScheme(argv[0]),FloatFromScheme(argv[1]),
												 FloatFromScheme(argv[2]),FloatFromScheme(argv[3]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// build-balljoint primitiveid-number primitiveid-number axis-vector 
// Returns: void
// Description:
// Creates a balljoint to connect two objects (see the ode docs for a detailed 
// description of the differences between the joint types). ODE considers joints to be a 
// constraint that is enforced between two objects. When creating a joint, it is important 
// to have the two primitives being joined in the desired positions before creating the 
// joint. Joints can be created, modified and indexed in a similar way to other primitives. 
// Example:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (build-balljoint shape1 shape2 (vector 0 0 0)) 
// (kick shape1 (vector 0 2 0))
// 
// (set-physics-debug #t)
// EndFunctionDoc

// StartFunctionDoc-pt
// build-balljoint número-id-primitiva número-id-primitiva vetor-eixo
// Retorna: void
// Descrição:
// Cria um balljoint para conectar dois objetos (veja os docs de ode
// para uma descrição detalhada das diferenças entre os tipos de
// dobras). ODE considera as juntas(joints) serem uma restrição
// imposta entre dois objetos. Quando criando uma junção, é importante
// ter as duas primitivas sendo juntas na posição desejada antes de
// criar uma junção. Junções podem ser criadas, modificadas e
// indexadas de uma maneira similar a outras primitivas.
// Exemplo:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (build-balljoint shape1 shape2 (vector 0 0 0)) 
// (kick shape1 (vector 0 2 0))
// 
// (set-physics-debug #t)
// EndFunctionDoc

Scheme_Object *build_balljoint(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("build-balljoint", "iiv", argc, argv);
	int name1=IntFromScheme(argv[0]);
	int name2=IntFromScheme(argv[1]);	
	float anchor[3];
	FloatsFromScheme(argv[2],anchor,3);
	MZ_GC_UNREG(); 
	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointBall(name1, name2, dVector(anchor[0],anchor[1],anchor[2])));
}

// StartFunctionDoc-en
// build-fixedjoint primitiveid-number
// Returns: void
// Description:
// Creates a joint to connect an object to the global environment. This locks the 
// object in place.
// Example:
// (clear)
// (define shape1 (with-state 
//         (translate (vector 0 1 0))
//         (build-cube)))
// (active-box shape1)
// 
// (build-fixedjoint shape1) ; not very exciting... 
// EndFunctionDoc

// StartFunctionDoc-pt
// build-fixedjoint número-id-primitiva
// Retorna: void
// Descrição:
// Cria uma junção para conectar um objeto ao ambiente global. Isto
// trava o objeto no lugar.
// Exemplo:
// (clear)
// (define shape1 (with-state 
//         (translate (vector 0 1 0))
//         (build-cube)))
// (active-box shape1)
// 
// (build-fixedjoint shape1) ; not very exciting... 
// EndFunctionDoc

Scheme_Object *build_fixedjoint(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("build-fixedjoint", "i", argc, argv);
    int name1=IntFromScheme(argv[0]);
	MZ_GC_UNREG(); 
	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointFixed(name1));
}

// StartFunctionDoc-en
// build-hingejoint primitiveid1-number primitiveid2-number anchor-vector axis-vector 
// Returns: hingeid-number
// Description:
// Creates a ball joint to connect two objects (see the ode docs for a detailed 
// description of the differences between the joint types). ODE considers joints to be a 
// constraint that is enforced between two objects. When creating a joint, it is important 
// to have the two primitives being joined in the desired positions before creating the 
// joint. Joints can be created, modified and indexed in a similar way to other primitives. 
// Example:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (build-hingejoint shape1 shape2 (vector 0 0 0) (vector 0 0 1)) 
// (kick shape1 (vector 0 2 0))
// 
// (set-physics-debug #t)
// EndFunctionDoc

// StartFunctionDoc-pt
// build-hingejoint numero-id-primitiva1 número-id-primitiva2 vetor-ancora vetor eixo
// Retorna: void
// Descrição:
// Cria uma junção circular para conectar dois objetos ( veja os docs
// de ODE para uma descrição detalhada entre tipos de junções).
// ODE considera as juntas(joints) serem uma restrição
// imposta entre dois objetos. Quando criando uma junção, é importante
// ter as duas primitivas sendo juntas na posição desejada antes de
// criar uma junção. Junções podem ser criadas, modificadas e
// indexadas de uma maneira similar a outras primitivas.
// Exemplo:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (build-hingejoint shape1 shape2 (vector 0 0 0) (vector 0 0 1)) 
// (kick shape1 (vector 0 2 0))
// 
// (set-physics-debug #t)
// EndFunctionDoc

Scheme_Object *build_hingejoint(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
 	ArgCheck("build-hingejoint", "iivv", argc, argv);
    
    int name1=IntFromScheme(argv[0]);
	int name2=IntFromScheme(argv[1]);
	
	float anchor[3];
	FloatsFromScheme(argv[2],anchor,3);
	
	dVector Hinge;
	float temp[3];
	FloatsFromScheme(argv[3],temp,3);
	Hinge.x=temp[0];
	Hinge.y=temp[1];
	Hinge.z=temp[2];

	MZ_GC_UNREG(); 
	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointHinge(name1, name2, dVector(anchor[0],anchor[1],anchor[2]), Hinge));
}

// StartFunctionDoc-en
// build-sliderjoint primitiveid1-number primitiveid2-number axis-vector 
// Returns: hingeid-number
// Description:
// Creates a slider joint to connect two objects (see the ode docs for a detailed 
// description of the differences between the joint types). ODE considers joints to be a 
// constraint that is enforced between two objects. When creating a joint, it is important 
// to have the two primitives being joined in the desired positions before creating the 
// joint. Joints can be created, modified and indexed in a similar way to other primitives. 
// Example:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (build-sliderjoint shape1 shape2 (vector 1 0 0)) 
// (kick shape1 (vector 0 2 0))
// 
// (set-physics-debug #t)
// EndFunctionDoc

// StartFunctionDoc-pt
// build-sliderjoint número-id-primitiva1 número-id-primitiva2 vetor-eixo
// Retorna: hingeid-number
// Descrição:
// Cria uma junção deslizante entre dois objetos (veja a documentação
// de ODE para uma descrição detalhada das diferenças entre os tipos
// de conexões). ODE considera as juntas(joints) serem uma restrição
// imposta entre dois objetos. Quando criando uma junção, é importante
// ter as duas primitivas sendo juntas na posição desejada antes de
// criar uma junção. Junções podem ser criadas, modificadas e
// indexadas de uma maneira similar a outras primitivas.
// Exemplo:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (build-sliderjoint shape1 shape2 (vector 1 0 0)) 
// (kick shape1 (vector 0 2 0))
// 
// (set-physics-debug #t)
// EndFunctionDoc

Scheme_Object *build_sliderjoint(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
  	ArgCheck("build-sliderjoint", "iiv", argc, argv);
   
    int name1=IntFromScheme(argv[0]);
	int name2=IntFromScheme(argv[1]);
		
	dVector Hinge;
	float temp[3];
	FloatsFromScheme(argv[2],temp,3);
	Hinge.x=temp[0];
	Hinge.y=temp[1];
	Hinge.z=temp[2];

	MZ_GC_UNREG(); 
	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointSlider(name1, name2, Hinge));
}

// StartFunctionDoc-en
// build-hinge2joint primitiveid1-number primitiveid2-number anchor-vector axis1-vector axis2-vector 
// Returns: hingeid-number
// Description:
// Creates a hinge2 joint to connect two objects (see the ode docs for a detailed 
// description of the differences between the joint types). ODE considers joints to be a 
// constraint that is enforced between two objects. When creating a joint, it is important 
// to have the two primitives being joined in the desired positions before creating the 
// joint. Joints can be created, modified and indexed in a similar way to other primitives. 
// Example:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (build-hinge2joint shape1 shape2 (vector 0 0 0) (vector 1 0 0) (vector 0 1 0)) 
// (kick shape1 (vector 0 2 0))
// 
// (set-physics-debug #t)
// EndFunctionDoc

// StartFunctionDoc-pt
// build-hinge2joint número-id-primitiva1 número-id-primitiva2 vetor-ancora vetor-eixo1 vetor-eixo2
// Retorna: numero-id-dobradiça(hinge)
// Descrição:
// Cria uma dobradiça de junções para conectar dois objetos (veja a documentação
// de ODE para uma descrição detalhada das diferenças entre os tipos
// de conexões). ODE considera as juntas(joints) serem uma restrição
// imposta entre dois objetos. Quando criando uma junção, é importante
// ter as duas primitivas sendo juntas na posição desejada antes de
// criar uma junção. Junções podem ser criadas, modificadas e
// indexadas de uma maneira similar a outras primitivas.
// Exemplo:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (build-hinge2joint shape1 shape2 (vector 0 0 0) (vector 1 0 0) (vector 0 1 0)) 
// (kick shape1 (vector 0 2 0))
// 
// (set-physics-debug #t)
// EndFunctionDoc

Scheme_Object *build_hinge2joint(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
   	ArgCheck("build-hinge2joint", "iivvv", argc, argv);
   
    int name1=IntFromScheme(argv[0]);
	int name2=IntFromScheme(argv[1]);
	
	float anchor[3];
	FloatsFromScheme(argv[2],anchor,3);
	
	dVector Hinge[2];
	float temp[3];
	FloatsFromScheme(argv[3],temp,3);
	Hinge[0].x=temp[0];
	Hinge[0].y=temp[1];
	Hinge[0].z=temp[2];
	
	FloatsFromScheme(argv[4],temp,3);
	Hinge[1].x=temp[0];
	Hinge[1].y=temp[1];
	Hinge[1].z=temp[2];
	
	MZ_GC_UNREG(); 
	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointHinge2(name1, name2, dVector(anchor[0],anchor[1],anchor[2]), Hinge));
}

// StartFunctionDoc-en
// build-amotorjoint primitiveid1-number primitiveid2-number axis-vector 
// Returns: hingeid-number
// Description:
// Creates a angular motor joint to connect two objects (see the ode docs for a detailed 
// description of the differences between the joint types). ODE considers joints to be a 
// constraint that is enforced between two objects. When creating a joint, it is important 
// to have the two primitives being joined in the desired positions before creating the 
// joint. Joints can be created, modified and indexed in a similar way to other primitives. 
// Example:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (build-amotorjoint shape1 shape2 (vector 1 0 0)) 
// (kick shape1 (vector 0 2 0))
// 
// (set-physics-debug #t)
// EndFunctionDoc

// StartFunctionDoc-pt
// build-amotorjoint número-id-primitiva1 número-id-primitiva2 vetor-eixo
// Retorna: número-id-dobradiça(hinge)
// Descrição:
// Cria uma junção com mobilidade angular para conectar dois objetos (veja a documentação
// de ODE para uma descrição detalhada das diferenças entre os tipos
// de conexões). ODE considera as juntas(joints) serem uma restrição
// imposta entre dois objetos. Quando criando uma junção, é importante
// ter as duas primitivas sendo juntas na posição desejada antes de
// criar uma junção. Junções podem ser criadas, modificadas e
// indexadas de uma maneira similar a outras primitivas.
// Exemplo:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (build-amotorjoint shape1 shape2 (vector 1 0 0)) 
// (kick shape1 (vector 0 2 0))
// 
// (set-physics-debug #t)
// EndFunctionDoc

Scheme_Object *build_amotorjoint(int argc, Scheme_Object **argv)
{
 	DECL_ARGV();
  	ArgCheck("build-amotorjoint", "iiv", argc, argv);
    int name1=IntFromScheme(argv[0]);
	int name2=IntFromScheme(argv[1]);	
	float axis[3];
	FloatsFromScheme(argv[2],axis,3);
	MZ_GC_UNREG(); 
	return scheme_make_integer_value(Engine::Get()->Physics()->CreateJointAMotor(name1, name2, dVector(axis[0],axis[1],axis[2])));
}

// StartFunctionDoc-en
// joint-param jointid-number param-string value-number 
// Returns: hingeid-number
// Description:
// Sets the joint parameter for a joint where param is one of the following: "HiStop", 
// "Vel", "FMax", "FudgeFactor", "Bounce", "CFM", "StopERP", "StopCFM","SuspensionERP", 
// "SuspensionCFM", "Vel2", "FMax2". see section 7.5.1 of the ODE docs for an explanation 
// of each of these parameters, and which joint types they apply to.
// Example:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (define j (build-hinge2joint shape1 shape2 (vector 0 0 0) (vector 1 0 0) (vector 0 1 0)))
// (joint-param j "Vel2" 0.1)
// (joint-param j "FMax2" 0.2)
// (joint-param j "LoStop" -0.75)
// (joint-param j "HiStop" 0.75)
// 
// (set-physics-debug #t)
// EndFunctionDoc

// StartFunctionDoc-pt
// joint-param número-id-junção param-string número-valor
// Retorna: número-id-dobradiça(hinge)
// Descrição:
// Ajusta o parâmetro da junção para uma junta onde param é um dos
// seguintes: "HiStop", "Vel", "FMax", "FudgeFactor", "Bounce, "CFM",
// "StopERP", "StopCFM", "SuspensionERP", "SuspensionCFM", "Vel2",
// "FMax2". Veja Seção 7.5.1 da documentação de ODE para uma
// explicação de cada um desses parâmetros, e a qual tipos de junções
// eles aplicam.
// Exemplo:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (define j (build-hinge2joint shape1 shape2 (vector 0 0 0) (vector 1 0 0) (vector 1 0 0)))
// (joint-param j "Vel2" 0.1)
// (joint-param j "FMax2" 0.2)
// (joint-param j "LoStop" -0.75)
// (joint-param j "HiStop" 0.75)
// 
// (set-physics-debug #t)
// EndFunctionDoc

Scheme_Object *joint_param(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
   	ArgCheck("joint-param", "isf", argc, argv);
    int joint=0;
	joint=IntFromScheme(argv[0]);
    double v = FloatFromScheme(argv[2]);
	Engine::Get()->Physics()->SetJointParam(joint,StringFromScheme(argv[1]),v);
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// joint-angle jointid-number angle-number vel-number 
// Returns: void
// Description:
// Set a new angle for this joint, with a given velocity taken to get there
// Example:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (define j (build-hingejoint shape1 shape2 (vector 0 0 0) (vector 0 1 0)))
// (joint-param j "FMax" 20)
// (joint-param j "LoStop" -1)
// (joint-param j "HiStop" 1)
// 
// (set-physics-debug #t)
// 
// (define (animate)
//     (joint-angle j 0.1 (* 5 (sin (time)))))
// EndFunctionDoc

// StartFunctionDoc-pt
// joint-angles jointid-number número-angulo número-vel
// Retorna: void
// Descrição:
// Ajusta um novo ângulo para este joint, com uma velocidade dada para
// chegar lá.
// Exemplo:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (define j (build-hingejoint shape1 shape2 (vector 0 0 0) (vector 0 1 0)))
// (joint-param j "FMax" 20)
// (joint-param j "LoStop" -1)
// (joint-param j "HiStop" 1)
// 
// (set-physics-debug #t)
// 
// (define (animate)
//     (joint-angle j 0.1 (* 5 (sin (time)))))
// EndFunctionDoc

Scheme_Object *joint_angle(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
   	ArgCheck("joint-angle", "iff", argc, argv);
	Engine::Get()->Physics()->SetJointAngle(IntFromScheme(argv[0]),FloatFromScheme(argv[1]),FloatFromScheme(argv[2]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// joint-slide jointid-number force 
// Returns: void
// Description:
// Applies the given force in the slider's direction. That is, it applies a force with magnitude force, 
// in the direction slider's axis, to body1, and with the same magnitude but opposite direction to body2. 
//
// Example:
// (clear)
// (ground-plane (vector 0 1 0) -1)
// (collisions 1)
// 
// (define shape1 (with-state 
//         (translate (vector -1 0 0))
//         (build-cube)))
// (active-box shape1)
// 
// (define shape2 (with-state 
//         (translate (vector 1 0 0))
//         (build-cube)))
// (active-box shape2)
// 
// (define j (build-sliderjoint shape1 shape2 (vector 1 0 0)))
// (joint-param j "FMax" 20)
// (joint-param j "LoStop" -1)
// (joint-param j "HiStop" 1)
// 
// (set-physics-debug #t)
// 
// (define (animate)
//     (joint-slide j (* 5 (sin (time)))))
// EndFunctionDoc
Scheme_Object *joint_slide(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
   	ArgCheck("joint-slide", "if", argc, argv);
	Engine::Get()->Physics()->JointSlide(IntFromScheme(argv[0]),FloatFromScheme(argv[1]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// set-max-physical max-number 
// Returns: void
// Description:
// Sets the maximum number of objects the physics system can deal with. When the max 
// level has been reached the oldest objects are automatically destroyed.
// Example:
// (clear)
// (set-max-physical 200)
// 
// (every-frame
//     (with-state
//     (translate (vector 0 5 0))
//         (scale (srndvec))
//         (colour (rndvec))
//         (let ((ob (build-cube)))    
//             (active-box ob)
//             (kick ob (vmul (srndvec) 3))
//             (twist ob (vmul (srndvec) 2)))))
// EndFunctionDoc

// StartFunctionDoc-pt
// set-max-physical número-max
// Retorna: void
// Descrição:
// Ajusta o máximo número de objetos que o sistema de física pode
// lidar. Quando o máximo nível foi alcançado os objetos mais antigos
// são automaticamente destroidos.
// Exemplo:
// (clear)
// (set-max-physical 200)
// 
// (every-frame
//     (with-state
//     (translate (vector 0 5 0))
//         (scale (srndvec))
//         (colour (rndvec))
//         (let ((ob (build-cube)))    
//             (active-box ob)
//             (kick ob (vmul (srndvec) 3))
//             (twist ob (vmul (srndvec) 2)))))
// EndFunctionDoc

Scheme_Object *set_max_physical(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
   	ArgCheck("set-max-physical", "i", argc, argv);
    Engine::Get()->Physics()->SetMaxObjectCount(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// set-mass primitiveid-number mass-number 
// Returns: void
// Description:
// Sets the mass of an active object
// Example:
// (clear)
// (ground-plane (vector 0 1 0) 0)
// (collisions 1)
// (set-max-physical 20)
// 
// ; not a great example, but these boxes will have 
// ; different mass, so behave a bit differently.
// 
// (every-frame
//     (when (> (rndf) 0.92)
//         (with-state
//             (translate (vector 0 5 0))
//             (scale (vmul (rndvec) 5))
//             (colour (rndvec))
//             (let ((ob (build-cube)))    
//                 (active-box ob)
//                 (set-mass ob (* (rndf) 10))
//                 (kick ob (vmul (srndvec) 3))
//                 (twist ob (vmul (srndvec) 2))))))    
// EndFunctionDoc

// StartFunctionDoc-pt
// set-mass número-id-primitiva número-massa
// Retorna: void
// Descrição:
// Ajusta a massa de um objeto ativo no sistema de física
// Exemplo:
// (clear)
// (ground-plane (vector 0 1 0) 0)
// (collisions 1)
// (set-max-physical 20)
// 
// ; not a great example, but these boxes will have 
// ; different mass, so behave a bit differently.
// 
// (every-frame
//     (when (> (rndf) 0.92)
//         (with-state
//             (translate (vector 0 5 0))
//             (scale (vmul (rndvec) 5))
//             (colour (rndvec))
//             (let ((ob (build-cube)))    
//                 (active-box ob)
//                 (set-mass ob (* (rndf) 10))
//                 (kick ob (vmul (srndvec) 3))
//                 (twist ob (vmul (srndvec) 2))))))    
// EndFunctionDoc

Scheme_Object *set_mass(int argc, Scheme_Object **argv)
{
 	DECL_ARGV();
  	ArgCheck("set-mass", "if", argc, argv);
    int obj=IntFromScheme(argv[0]);
    float mass=FloatFromScheme(argv[1]);
	Engine::Get()->Physics()->SetMass(obj,mass);
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// gravity gravity-vector
// Returns: void
// Description:
// Sets the strength and direction of gravity.
// Example:
// (clear)
// (ground-plane (vector 0 1 0) 0)
// (collisions 1)
// (set-max-physical 20)
// 
// (every-frame
//     (begin
//         (gravity (vector 0 (sin (time)) 0)) ; change gravity! :)
//         (when (> (rndf) 0.92)
//             (with-state
//                 (translate (vector 0 5 0))
//                 (scale (rndvec))
//                 (colour (rndvec))
//                 (let ((ob (build-cube)))    
//                     (active-box ob)
//                     (kick ob (vmul (srndvec) 3))
//                     (twist ob (vmul (srndvec) 2)))))))
// EndFunctionDoc

// StartFunctionDoc-pt
// gravity vetor-gravidade
// Retorna: void
// Descrição:
// Ajusta a força e direção da gravidade.
// Exemplo:
// (clear)
// (ground-plane (vector 0 1 0) 0)
// (collisions 1)
// (set-max-physical 20)
// 
// (every-frame
//     (begin
//         (gravity (vector 0 (sin (time)) 0)) ; change gravity! :)
//         (when (> (rndf) 0.92)
//             (with-state
//                 (translate (vector 0 5 0))
//                 (scale (rndvec))
//                 (colour (rndvec))
//                 (let ((ob (build-cube)))    
//                     (active-box ob)
//                     (kick ob (vmul (srndvec) 3))
//                     (twist ob (vmul (srndvec) 2)))))))
// EndFunctionDoc

Scheme_Object *gravity(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
   	ArgCheck("gravity", "v", argc, argv);
	float vec[3];
	FloatsFromScheme(argv[0],vec,3);
	Engine::Get()->Physics()->SetGravity(dVector(vec[0],vec[1],vec[2]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// kick primitiveid-number kick-vector
// Returns: void
// Description:
// Applies translation force to the object
// Example:
// (clear)
// (collisions 1)
// (set-max-physical 20)
// (gravity (vector 0 0 0)) 
// 
// (every-frame
//     (when (> (rndf) 0.92)
//         (with-state
//             (scale (rndvec))
//             (colour (rndvec))
//             (let ((ob (build-cube)))    
//                 (active-box ob)
//                 (kick ob (vmul (srndvec) 3))
//                 (twist ob (vmul (srndvec) 2))))))
// EndFunctionDoc

// StartFunctionDoc-pt
// kick número-id-primitiva vetor-chute
// Retorna: void
// Descrição:
// Aplica força de translação ao objeto.
// Exemplo:
// (clear)
// (collisions 1)
// (set-max-physical 20)
// (gravity (vector 0 0 0)) 
// 
// (every-frame
//     (when (> (rndf) 0.92)
//         (with-state
//             (scale (rndvec))
//             (colour (rndvec))
//             (let ((ob (build-cube)))    
//                 (active-box ob)
//                 (kick ob (vmul (srndvec) 3))
//                 (twist ob (vmul (srndvec) 2))))))
// EndFunctionDoc

Scheme_Object *kick(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
    ArgCheck("kick", "iv", argc, argv);
    int obj=IntFromScheme(argv[0]);
    float vec[3];
    FloatsFromScheme(argv[1],vec,3);
	Engine::Get()->Physics()->Kick(obj,dVector(vec[0],vec[1],vec[2]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// twist primitiveid-number spin-vector
// Returns: void
// Description:
// Applies rotational force to the object
// Example:
// (clear)
// (collisions 1)
// (set-max-physical 20)
// (gravity (vector 0 0 0)) 
// 
// (every-frame
//     (when (> (rndf) 0.92)
//         (with-state
//             (scale (rndvec))
//             (colour (rndvec))
//             (let ((ob (build-cube)))    
//                 (active-box ob)
//                 (kick ob (vmul (srndvec) 3))
//                 (twist ob (vmul (srndvec) 2))))))
// EndFunctionDoc

// StartFunctionDoc-pt
// twist número-id-primitiva vetor-giro
// Retorna: void
// Descrição:
// Aplica força rotacional ao objeto
// Exemplo:
// (clear)
// (collisions 1)
// (set-max-physical 20)
// (gravity (vector 0 0 0)) 
// 
// (every-frame
//     (when (> (rndf) 0.92)
//         (with-state
//             (scale (rndvec))
//             (colour (rndvec))
//             (let ((ob (build-cube)))    
//                 (active-box ob)
//                 (kick ob (vmul (srndvec) 3))
//                 (twist ob (vmul (srndvec) 2))))))
// EndFunctionDoc

Scheme_Object *twist(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
    ArgCheck("twist", "iv", argc, argv);
    int obj=IntFromScheme(argv[0]);
    float vec[3];
    FloatsFromScheme(argv[1],vec,3);
	Engine::Get()->Physics()->Twist(obj,dVector(vec[0],vec[1],vec[2]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// has-collided primitiveid-number
// Returns: void
// Description:
// Returns true if the grabbed object collided in the last frame
// Example:
// (clear)
// (ground-plane (vector 0 1 0) 0)
// (collisions 1)
// (set-max-physical 20)
// 
// (define ob (with-state
//     (translate (vector 0 5 0))
//     (build-cube)))
// 
// (active-box ob)
// 
// (every-frame
//     (when (has-collided ob)
//         (with-primitive ob
//             (colour (rndvec)))))
// EndFunctionDoc

// StartFunctionDoc-pt
// has-collided número-id-primitiva
// Retorna: void
// Descrição:
// Retorna verdadeiro se a primitiva pega colidiu no ultimo frame.
// Exemplo:
// (clear)
// (ground-plane (vector 0 1 0) 0)
// (collisions 1)
// (set-max-physical 20)
// 
// (define ob (with-state
//     (translate (vector 0 5 0))
//     (build-cube)))
// 
// (active-box ob)
// 
// (every-frame
//     (when (has-collided ob)
//         (with-primitive ob
//             (colour (rndvec)))))
// EndFunctionDoc

Scheme_Object *has_collided(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
    ArgCheck("has-collided", "i", argc, argv);
	if (Engine::Get()->Physics()->HasCollided(IntFromScheme(argv[0])))
	{
		MZ_GC_UNREG(); 
		return scheme_make_true();
	}
	else
	{
		MZ_GC_UNREG(); 
		return scheme_make_false();
	}
}

void PhysicsFunctions::AddGlobals(Scheme_Env *env)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();
	scheme_add_global("collisions", scheme_make_prim_w_arity(collisions, "collisions", 1, 1), env);
	scheme_add_global("ground-plane", scheme_make_prim_w_arity(ground_plane, "ground-plane", 2, 2), env);
	scheme_add_global("active-box", scheme_make_prim_w_arity(active_box, "active-box", 1, 1), env);
	scheme_add_global("active-cylinder", scheme_make_prim_w_arity(active_box, "active-cylinder", 1, 1), env);
	scheme_add_global("active-sphere", scheme_make_prim_w_arity(active_box, "active-sphere", 1, 1), env);
	scheme_add_global("passive-box", scheme_make_prim_w_arity(passive_box, "passive-box", 1, 1), env);
	scheme_add_global("passive-cylinder", scheme_make_prim_w_arity(passive_box, "passive-cylinder", 1, 1), env);
	scheme_add_global("passive-sphere", scheme_make_prim_w_arity(passive_box, "passive-sphere", 1, 1), env);
	scheme_add_global("physics-remove", scheme_make_prim_w_arity(physics_remove, "physics-remove", 1, 1), env);
	scheme_add_global("surface-params", scheme_make_prim_w_arity(surface_params, "surface-params", 4, 4), env);
	scheme_add_global("build-balljoint", scheme_make_prim_w_arity(build_balljoint, "build-balljoint", 3, 3), env);
	scheme_add_global("build-fixedjoint", scheme_make_prim_w_arity(build_fixedjoint, "build-fixedjoint", 1, 1), env);
	scheme_add_global("build-hingejoint", scheme_make_prim_w_arity(build_hingejoint, "build-hingejoint", 4, 4), env);
	scheme_add_global("build-sliderjoint", scheme_make_prim_w_arity(build_sliderjoint, "build-sliderjoint", 3, 3), env);
	scheme_add_global("build-hinge2joint", scheme_make_prim_w_arity(build_hinge2joint, "build-hinge2joint", 5, 5), env);
	scheme_add_global("build-amotorjoint", scheme_make_prim_w_arity(build_amotorjoint, "build-amotorjoint", 3, 3), env);
	scheme_add_global("joint-param", scheme_make_prim_w_arity(joint_param, "joint-param", 3, 3), env);
	scheme_add_global("joint-angle", scheme_make_prim_w_arity(joint_angle, "joint-angle", 3, 3), env);
	scheme_add_global("joint-slide", scheme_make_prim_w_arity(joint_slide, "joint-slide", 2, 2), env);
	scheme_add_global("set-max-physical", scheme_make_prim_w_arity(set_max_physical, "set-max-physical", 1, 1), env);
	scheme_add_global("set-mass", scheme_make_prim_w_arity(set_mass, "set-mass", 2, 2), env);
	scheme_add_global("gravity", scheme_make_prim_w_arity(gravity, "gravity", 1, 1), env);
	scheme_add_global("kick", scheme_make_prim_w_arity(kick, "kick", 2, 2), env);
	scheme_add_global("twist", scheme_make_prim_w_arity(twist, "twist", 2, 2), env);
	scheme_add_global("has-collided", scheme_make_prim_w_arity(has_collided, "has-collided", 1, 1), env);
	MZ_GC_UNREG();
}
