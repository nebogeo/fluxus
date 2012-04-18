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
#include "ShaderCache.h"

using namespace LocalStateFunctions;
using namespace SchemeHelper;
using namespace Fluxus;

// StartSectionDoc-en
// local-state
// The local state functions control rendering either for the current state - or the state of
// the current primitive. In fluxus state means the way that things are displayed,
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

// StartSectionDoc-fr
// local-state
// Les fonctions d'état local contrôlent le rendu ainsi que l'état courant - ou l'état de la
// primitive en cours. Dans fluxus, l'état signifie la façon dont les choses sont affichées,
// ainsi qu'activé ou non les options de rendu, changer le style de différentes fonctionnalités,
// ou interagir sur la transformation en cours.
// Exemple:
// EndSectionDoc

// StartFunctionDoc-en
// push
// Returns: void
// Description:
// Pushes a copy of the current drawing state to the top of the stack. The drawing state
// contains information about things like the current colour, transformation and hints.
// This function has been superseded by (with-state).
// Example:
// (colour (vector 1 0 0)) ; set current colour to red
// (push)                  ; copy and push drawing state
// (colour (vector 0 1 0)) ; set current colour to green
// (draw-cube)             ; draws a green cube
// (pop)           ; forget old drawing state
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
// (draw-cube)         ; desenha um cubo verde
// (pop)           ; esquece estado de desenho antigo
// ; cor corrente é vermelha de novo.
// EndFunctionDoc

// StartFunctionDoc-fr
// push
// Retour: vide
// Description:
// Pousse une copie de l'état courant de rendu au sommet de la pîle. L'état de rendu contient
// les informations sur les objets comme la couleur, les transformations et les options de rendu.
// Cette fonction a été remplacée par (with-state).
// Exemple:
// (colour (vector 1 0 0)) ; fixe la couleur courante à rouge
// (push)                  ; copie et pousse l'état de rendu
// (colour (vector 0 1 0)) ; fixe la couleur courante à vert
// (draw-cube)             ; dessine un cube vert
// (pop)           ; oublie l'ancien état de rendu
// ; la couleur en cours redevient rouge
// EndFunctionDoc

Scheme_Object *push(int argc, Scheme_Object **argv)
{
	// record the push on the grabstack too - this means we can combine them
	Engine::Get()->PushGrab(0);
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
// This function has been superseded by (with-state).
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

// StartFunctionDoc-fr
// pop
// Retour: vide
// Description:
// Détruis l'état courant de rendu, et replace l'état précédement poussé dans la pîle.
// L'état de rendu contient les informations sur les objets comme la couleur,
// les transformations et les options de rendu.
// Cette fonction a été remplacée par (with-state).
// Exemple:
// (colour (vector 1 0 0)) ; fixe la couleur courante à rouge
// (push)                  ; copie et pousse l'état de rendu
// (colour (vector 0 1 0)) ; fixe la couleur courante à vert
// (draw-cube)             ; dessine un cube vert
// (pop)           ; oublie l'ancien état de rendu
// ; la couleur en cours redevient rouge
// EndFunctionDoc

Scheme_Object *pop(int argc, Scheme_Object **argv)
{
	Engine::Get()->PopGrab();
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
// This function has been superseded by (with-primitive).
// Example:
// (colour (vector 1 0 0))      ; set the current colour to red
// (define mycube (build-cube)) ; makes a red cube
// (grab mycube)
// (colour (vector 0 1 0)) ; sets the cubes colour to green
// (ungrab)          ; return to normal state
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
// (ungrab)          ; retorna ao estado normal
// EndFunctionDoc

// StartFunctionDoc-fr
// grab objet-id
// Retour: vide
// Description:
// Attrape l'objet spécifié. Une fois l'objet accroché, son état peut être modifié en
// utilisant les mêmes commandes pour modifier l'état de rendu. (ungrab) doit être utilisé
// pour retourner à l'état de rendu en cours. L'accrochage peut aussi être empilé, dans ce cas
// il faut relacher les primtives jusqu'à la dernière accrochée.
// Cette fonction a été remplacée par (with-primitive).
// Exemple:
// (colour (vector 1 0 0))      ; fixe la couleur courante à rouge
// (define mycube (build-cube)) ; fabrique un cube rouge
// (grab mycube)
// (colour (vector 0 1 0)) ; fixe la couleur du cube à vert
// (ungrab)          ; retour à l'état normal
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
// Ungrabs the current object, and either returns to the normal drawing state,
// or pops to the last grabbed primitive.
// This function has been superseded by (with-primitive).
// Example:
// (colour (vector 1 0 0))      ; set the current colour to red
// (define mycube (build-cube)) ; makes a red cube
// (grab mycube)
// (colour (vector 0 1 0)) ; sets the cubes colour to green
// (ungrab)          ; return to normal state
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
// (ungrab)          ; retorna ao estado normal
// EndFunctionDoc

// StartFunctionDoc-fr
// ungrab
// Retour: vide
// Description:
// Relache l'objet courant, et retourne à l'état normale ou la denière
// primitive accrochée.
// Cette fonction a été remplacée par (with-primitive).
// Exemple:
// (colour (vector 1 0 0))      ; fixe la couleur courante à rouge
// (define mycube (build-cube)) ; fabrique un cube rouge
// (grab mycube)
// (colour (vector 0 1 0)) ; fixe la couleur du cube à vert
// (ungrab)          ; retour à l'état normal
// EndFunctionDoc

Scheme_Object *ungrab(int argc, Scheme_Object **argv)
{
	Engine::Get()->PopGrab();
	return scheme_void;
}

// StartFunctionDoc-en
// apply-transform optional-object-id
// Returns: void
// Description:
// Applies the current object transform to the vertex positions of the current object and
// sets it's transform to identity. Will also use the optional id passed in for the aniquated
// version of this command
// Example:
// (rotate (vector 45 0 0))
// (define mycube (build-cube)) ; makes a cube with a rotation
// (with-primitive mycube (apply-transform)) ; applies the rotation to the points of the cube
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
// (with-primitive mycube (apply-transform)) ; aplica a rotação aos pontos do cubo
// EndFunctionDoc

// StartFunctionDoc-fr
// apply-transform optionel-objet-id
// Retour: vide
// Description:
// Applique la transformation de l'objet courant aux positions des vertex de l'objet courant
// et l'applique comme son identité. Peut-être passé l'id optionel pour la version archaïque
// de cette commande.
// Exemple:
// (rotate (vector 45 0 0))
// (define mycube (build-cube)) ; fabriche un cube avec une rotation
// (with-primitive mycube (apply-transform)) ; applique la rotation aux points du cube
// EndFunctionDoc

Scheme_Object *apply(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	if (argc==1)
	{
		ArgCheck("apply-transform", "i", argc, argv);
		Engine::Get()->Renderer()->GetPrimitive(IntFromScheme(argv[0]))->ApplyTransform();
	}
	else
	{
		if (Engine::Get()->Grabbed())
		{
			Engine::Get()->Grabbed()->ApplyTransform();
		}
	}
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// opacity value
// Returns: void
// Description:
// Sets the opacity of the current drawing state, or the current primitive.
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

// StartFunctionDoc-fr
// opacity valeur
// Retour: vide
// Description:
// Fixe l'opacité de l'état courant de rendu, ou de la primtive en cours.
// Exemple:
// (opacity 0.5)
// (define mycube (build-cube)) ; fabrique un cube semi-transparent
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
// Sets the wireframe opacity of the current drawing state, or the current primitive.
// Example:
// (hint-none)
// (hint-wire)
// (backfacecull 0)
// (line-width 5)
// (wire-colour (vector 1 1 1))
// (wire-opacity 0.5)
// (build-cube) ; makes a half transparent wireframe cube
// EndFunctionDoc

// StartFunctionDoc-pt
// wire-opacity valor
// Retorna: void
// Descrição:
// Ajusta a opacidade das beiradas da malha no estado ou da primitiva atual.
// Exemplo:
// (hint-none)
// (hint-wire)
// (backfacecull 0)
// (line-width 5)
// (wire-colour (vector 1 1 1))
// (wire-opacity 0.5)
// (build-cube) ; makes a half transparent wireframe cube
// EndFunctionDoc

// StartFunctionDoc-fr
// wire-opacity valeur
// Retour: vide
// Description:
// Fixe l'opacité du maillage de l'état courant de rendu, ou de la primitive en cours.
// Exemple:
// (hint-none)
// (hint-wire)
// (backfacecull 0)
// (line-width 5)
// (wire-colour (vector 1 1 1))
// (wire-opacity 0.5)
// (build-cube) ; fabrique un cube en fil de fer semi-transparent 
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
// Sets the shinyness of the current drawing state, or the current primitive.
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

// StartFunctionDoc-fr
// shinyness valeur
// Retour: vide
// Description:
// Fixe la brillance de l'état courant de rendu, ou de la primitive en cours.
// Cette valeur fixe l'étroitesse des reflets spéculaires .
// Exemple:
// (shinyness 100)
// (specular (vector 1 1 1)) ; fixe la couleur des spéculaires
// (define mysphere (build-sphere 10 10)) ; fabrique un cube brillant
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
// Sets the colour of the current drawing state, or the current primitive.
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

// StartFunctionDoc-fr
// colour colour-vector
// Retour: vide
// Description:
// Fixe la couleur de l'état courant de rendu, ou de la primitive en cours.
// Exemple:
// (colour (vector 1 0.5 0.1)) ; mmm orange...
// (define mycube (build-cube)) ; fabrique un cube orange
// EndFunctionDoc

Scheme_Object *colour(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("colour", "c", argc, argv);
	dColour c=ColourFromScheme(argv[0], Engine::Get()->State()->ColourMode);
	Engine::Get()->State()->Colour=c;
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// colour-mode mode
// Returns: void
// Description:
// Changes the way Fluxus interprets colour data for the current drawing
// state, or the current primitive.
// Colourmode symbols can consist of: rgb hsv
// Example:
// (clear)
// (colour-mode 'hsv)
//
// (for ((x (in-range 0 10)))
//   (translate (vector 1 0 0))
//   (colour (vector (/ x 10) 1 1))
//   (build-cube))
// EndFunctionDoc

// StartFunctionDoc-pt
// colour-mode modo
// Returns: void
// Description:
// Muda a forma como Fluxus interpreta dados de cor para o estado de
// desenho ou da primitiva atual.
// Os simbolos dos modos de cor são: rgb hsv
// Exemplo:
// (clear)
// (colour-mode 'hsv)
//
// (for ((x (in-range 0 10)))
//   (translate (vector 1 0 0))
//   (colour (vector (/ x 10) 1 1))
//   (build-cube))
// EndFunctionDoc

// StartFunctionDoc-fr
// colour-mode mode
// Retour: vide
// Description:
// Change la façon dont Fluxus interprète les valeurs de couleur de l'état courant de rendu,
// ou de la primitive en cours.
// Les symboles Colourmode  possibles sont: rgb hsv
// Exemple:
// (clear)
// (colour-mode 'hsv)
//
// (for ((x (in-range 0 10)))
//   (translate (vector 1 0 0))
//   (colour (vector (/ x 10) 1 1))
//   (build-cube))
// EndFunctionDoc

Scheme_Object *colour_mode(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("colour-mode", "S", argc, argv);
	string mode=SymbolName(argv[0]);

	if (mode=="rgb") Engine::Get()->State()->ColourMode=MODE_RGB;
	else if (mode=="hsv") Engine::Get()->State()->ColourMode=MODE_HSV;
	else Trace::Stream<<"colour mode not recognised: "<<mode<<endl;

	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// rgb->hsv colour-vector
// Returns: vector
// Description:
// Converts the RGB colour to HSV.
// Example:
// (rgb->hsv (vector 1 0.5 0.1))
// EndFunctionDoc

// StartFunctionDoc-pt
// rgb->hsv vetor-cor
// Retorna: vetor
// Descrição:
// Converte a cor RGB para HSV.
// Exemplo:
// (rgb->hsv (vector 1 0.5 0.1))
// EndFunctionDoc

// StartFunctionDoc-fr
// rgb->hsv couleur-vecteur
// Retour: vecteur
// Description:
// Convertis une couleur RGB en HSV.
// Exemple:
// (rgb->hsv (vector 1 0.5 0.1))
// EndFunctionDoc

Scheme_Object *rgbtohsv(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("rgb->hsv", "v", argc, argv);
	float rgb[4], hsv[4];
	int vec_size = SCHEME_VEC_SIZE(argv[0]);
	FloatsFromScheme(argv[0], rgb, vec_size);
	dColour::RGBtoHSV(rgb[0], rgb[1], rgb[2], hsv);
	hsv[3] = rgb[3];
	MZ_GC_UNREG();
	return FloatsToScheme(hsv, vec_size);
}

// StartFunctionDoc-en
// hsv->rgb colour-vector
// Returns: vector
// Description:
// Converts the HSV colour to RGB.
// Example:
// (clear)
// (for* ((x (in-range 0 10))  ; builds a 10x10 HSV colour pattern
//        (y (in-range 0 10)))
//     (identity)
//     (translate (vector x y 0))
//     (colour (hsv->rgb (vector (/ x 10) (/ y 10) 1)))
//     (build-cube))
// EndFunctionDoc

// StartFunctionDoc-pt
// hsv->rgb vetor-cor
// Retorna: vetor
// Descrição:
// Converte a cor HSV para RGB
// Exemplo:
// (clear)
// (for* ((x (in-range 0 10))  ; builds a 10x10 HSV colour pattern
//        (y (in-range 0 10)))
//     (identity)
//     (translate (vector x y 0))
//     (colour (hsv->rgb (vector (/ x 10) (/ y 10) 1)))
//     (build-cube))
// EndFunctionDoc

// StartFunctionDoc-fr
// hsv->rgb couleur-vecteur
// Retour: vecteur
// Description:
// Convertis une couleur HSV en RGB.
// Exemple:
// (clear)
// (for* ((x (in-range 0 10))  ; construit un motif de couleur HSV en 10x10
//        (y (in-range 0 10)))
//     (identity)
//     (translate (vector x y 0))
//     (colour (hsv->rgb (vector (/ x 10) (/ y 10) 1)))
//     (build-cube))
// EndFunctionDoc

Scheme_Object *hsvtorgb(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("hsv->rgb", "v", argc, argv);
	float hsv[4], rgb[4];
	int vec_size = SCHEME_VEC_SIZE(argv[0]);
	FloatsFromScheme(argv[0], hsv, vec_size);
	dColour::HSVtoRGB(hsv[0], hsv[1], hsv[2], rgb);
	rgb[3] = hsv[3];
	MZ_GC_UNREG();
	return FloatsToScheme(rgb, vec_size);
}

// StartFunctionDoc-en
// wire-colour colour-vector
// Returns: void
// Description:
// Sets the wire frame colour of the current drawing state, or the current
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

// StartFunctionDoc-fr
// wire-colour couleur-vecteur
// Retour: vide
// Description:
// Fixe la couleur du maillage de l'état courant de rendu, ou de la primitive en cours.
// Visible avec (hint-wire) sur la plupart des primitives.
// Exemple:
// (wire-colour (vector 1 1 0)) ; fixe jaune la couleur de maillage courante
// (hint-wire)
// (define mycube (build-cube)) ; fabrique un cube en fil de fer jaune
// EndFunctionDoc
Scheme_Object *wire_colour(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("wire-colour", "c", argc, argv);
	dColour c=ColourFromScheme(argv[0], Engine::Get()->State()->ColourMode);
	Engine::Get()->State()->WireColour=c;
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// normal-colour colour-vector
// Returns: void
// Description:
// Sets the normals frame colour of the current drawing state, or the current
// primitive. Visible with (hint-normal) on most primitives.
// Example:
// (normal-colour (vector 1 1 0)) ; set yellow as current normals colour
// (hint-normal)
// (define mycube (build-cube)) ; makes a cube with yellow wireframe
// EndFunctionDoc

// StartFunctionDoc-fr
// normal-colour couleur-vecteur
// Retour: vide
// Description:
// Fixe la couleur des normales de l'état courant de rendu, ou de la primitive en cours.
// Visible avec (hint-normal) sur la plupart des primitives.
// Example:
// (normal-colour (vector 1 1 0)) ; fixe à jaune la couleur des normales
// (hint-normal)
// (define mycube (build-cube)) ; fabrique un cube aux normales jaunes
// EndFunctionDoc

Scheme_Object *normal_colour(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("normal-colour", "c", argc, argv);
	dColour c=ColourFromScheme(argv[0], Engine::Get()->State()->ColourMode);
	Engine::Get()->State()->NormalColour=c;
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// specular colour-vector
// Returns: void
// Description:
// Sets the specular colour of the current drawing state, or the current
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

// StartFunctionDoc-fr
// specular couleur-vecteur
// Retour: vide
// Description:
// Fixe la couleur des spéculaires de l'état courant de rendu, ou de la primitive en cours.
// Exemple:
// (specular (vector 0 0 1)) ; Fixe à bleu la couleur des spéculaires
// (define mysphere (build-sphere 10 10)) ; Fabrique une sphère à brillance bleue
// EndFunctionDoc

Scheme_Object *specular(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("specular", "c", argc, argv);
	Engine::Get()->State()->Specular=ColourFromScheme(argv[0]);
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// ambient colour-vector
// Returns: void
// Description:
// Sets the ambient colour of the current drawing state, or the current
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

// StartFunctionDoc-fr
// ambient couleur-vecteur
// Retour: vide
// Description:
// Fixe la couleur ambiente de l'état courant de rendu, ou de la primitive en cours.
// Exemple:
// (ambient (vector 0 0 1)) ; Fixe à bleu la couleur ambiente
// (define mysphere (build-sphere 10 10)) ; Fabrique une sphère bleue
// EndFunctionDoc

Scheme_Object *ambient(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("ambient", "c", argc, argv);
	Engine::Get()->State()->Ambient=ColourFromScheme(argv[0]);
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// emissive value
// Returns: void
// Description:
// Sets the emissive colour of the current drawing state, or the current
// primitive.
// Example:
// (emissive (vector 0 0 1)) ; set blue as emissive colour
// (define mysphere (build-sphere 10 10)) ; makes an bright blue sphere
// EndFunctionDoc

// StartFunctionDoc-pt
// emissive valor
// Retorna: void
// Descrição:
// Ajusta a cor emissiva do estado de desenho atual, ou da primitiva
// atualmente pega.
// Exemplo:
// (emissive (vector 0 0 1)) ; ajusta a cor emissiva para azul
// (define mysphere (build-sphere 10 10)) ; faz uma esfera azul brilhante
// EndFunctionDoc

// StartFunctionDoc-fr
// emissive couleur-vecteur
// Retour: vide
// Description:
// Fixe la couleur d'émission de l'état courant de rendu, ou de la primitive en cours.
// Exemple:
// (emissive (vector 0 0 1)) ; Fixe la couleur d'émission à bleue
// (define mysphere (build-sphere 10 10)) ; Fabrique une éclatante sphère bleue
// EndFunctionDoc

Scheme_Object *emissive(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("emissive", "c", argc, argv);
	Engine::Get()->State()->Emissive=ColourFromScheme(argv[0]);
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// identity
// Returns: void
// Description:
// Sets the drawing state transform to identity, on the state stack, or the current primitive.
// Example:
// (define mycube (with-state
//     (scale (vector 2 2 2)) ; set the current scale to double in each dimension
//     (build-cube))) ; make a scaled cube
//
// (with-primitive mycube
//     (identity)) ; erases the transform and puts the cube back to its original state
// EndFunctionDoc

// StartFunctionDoc-pt
// identity
// Retorna: void
// Descrição:
// Ajusta a transformação do estado de desenho para identidade, no
// estado da pilha, ou a primitiva atualmente pega.
// Exemplo:
// (define mycube (with-state
//     (scale (vector 2 2 2)) ; ajusta o tamanho atual pro dobro em cada dimensão
//     (build-cube))) ; faz um cubo aumentado
//
// (with-primitive mycube
//     (identity)) ; apaga a transformação e coloca o cubo de volta ao seu
//                 ; estado original
// EndFunctionDoc

// StartFunctionDoc-fr
// identity
// Retour: vide
// Description:
// Retourne l'état courant de rendu, ou de la primitive en cours à son identité.
// Exemple:
// (define mycube (with-state
//     (scale (vector 2 2 2)) ; fixe la taille courante au double de sa dimension
//     (build-cube))) ; fabrique un cube doublé
//
// (with-primitive mycube
//     (identity)) ; efface la transformation et retourne le cube à son état original
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
// Concatenates (multiplies) a matrix on to the current drawing state or current primitive.
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

// StartFunctionDoc-fr
// concat matrice
// Retour: vide
// Description:
// Concatène (multiplie) une matrice à l'état courant de rendu, ou à la primitive en cours.
// Exemple:
// (define mymatrix (mrotate (vector 0 45 0))) ; fabrication d'une matrice
// (concat mymatrix) ; concaténationà l'état courant
// (build-cube) ; fabrique un cube avec cette rotation
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
// Applies a translation to the current drawing state transform or current primitive.
// Example:
// (translate (vector 0 1.4 0)) ; translates the current transform up a bit
// (build-cube) ; build a cube with this transform
// EndFunctionDoc

// StartFunctionDoc-pt
// translate vetor
// Retorna: void
// Descrição:
// Aplica uma translação ao estado de desenho atual ou primitiva pega
// Exemplo:
// (translate (vector 0 1.4 0)) ; translada a transformação atual pra
//                              ; cima um pouco
// (build-cube) ; constrói um cubo com esta transformação
// EndFunctionDoc

// StartFunctionDoc-fr
// translate vecteur
// Retour: vide
// Description:
// Applique une translation à l'état courant de rendu, ou à la primitive en cours.
// Exemple:
// (translate (vector 0 1.4 0)) ; translationne la transformation courante d'un petit peu
// (build-cube) ; fabrique un cube avec cette transformation
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
// Applies a rotation to the current drawing state transform or current primitive.
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

// StartFunctionDoc-fr
// rotate vecteur-ou-quaternion
// Retour: vide
// Description:
// Applique une rotation à l'état courant de rendu, ou à la primitive en cours.
// Exemple:
// (rotate (vector 0 45 0)) ; tourne de 45 degrés en axe des Y
// (build-cube) ; fabrique un cube avec cette transformation
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
// Applies a scale to the current drawing state transform or current primitive.
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

// StartFunctionDoc-fr
// scale vecteur
// Retour: vide
// Description:
// Applique une nouvelle échelle à l'état courant de rendu, ou à la primitive en cours.
// Exemple:
// (scale (vector 0.5 0.5 0.5)) ; change la taille de la transformation courante de moitié
// (build-cube) ; fabrique un cube avec cette transformation
// EndFunctionDoc

Scheme_Object *scale(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	if ((!SCHEME_VECTORP(argv[0]) || (SCHEME_VEC_SIZE(argv[0])!=3))
			&& (!SCHEME_NUMBERP(argv[0])))
		scheme_wrong_type("scale", "vector size 3 or number", 0, argc, argv);

	if (SCHEME_VECTORP(argv[0]))
	{
		dVector t;
		FloatsFromScheme(argv[0],t.arr(),3);
		Engine::Get()->State()->Transform.scale(t.x,t.y,t.z);
	}
	else
	{
		float t=FloatFromScheme(argv[0]);
		Engine::Get()->State()->Transform.scale(t,t,t);
	}
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// get-transform
// Returns: matrix-vector
// Description:
// Returns a matrix representing the current state transform for the
// current primitive.
// Example:
// (clear)
// ; build a hierarchy
// (define a
//     (with-state
//         (colour (vector 1 0.5 0.5))
//         (build-cube)))
// (define b (with-state
//         (colour (vector 0.5 1 0.5))
//         (parent a)
//         (translate (vector 2 0 0))
//         (build-cube)))
// (define c (with-state
//         (colour (vector 0.5 0.5 1))
//         (parent b)
//         (translate (vector 2 0 0))
//         (build-cube)))
//
// (define (animate)
//     ; animate the hierarchy
//     (with-primitive a (rotate (vector 0 0 (sin (time)))))
//     (with-primitive b (rotate (vector 0 0 (sin (time)))))
//     (with-primitive c (rotate (vector 0 0 (sin (time)))))
//
//     ; position a yellow sphere with c's local transform
//     (with-state
//         (concat (with-primitive c (get-transform)))
//         (opacity 0.5)
//         (colour (vector 1 1 0))
//         (draw-sphere))
//
//     ; position a purple sphere with c's global transform
//     (with-state
//         (concat (with-primitive c (get-global-transform)))
//         (opacity 0.5)
//         (colour (vector 1 0 1))
//         (draw-sphere)))
//
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-pt
// get-transform
// Retorna: vetor-matriz
// Descrição:
// Retorna uma matriz representando o estado de tranformação corrente
// ou para a primitiva pega.
// Exemplo:
// (clear)
// ; build a hierarchy
// (define a
//     (with-state
//         (colour (vector 1 0.5 0.5))
//         (build-cube)))
// (define b (with-state
//         (colour (vector 0.5 1 0.5))
//         (parent a)
//         (translate (vector 2 0 0))
//         (build-cube)))
// (define c (with-state
//         (colour (vector 0.5 0.5 1))
//         (parent b)
//         (translate (vector 2 0 0))
//         (build-cube)))
//
// (define (animate)
//     ; animate the hierarchy
//     (with-primitive a (rotate (vector 0 0 (sin (time)))))
//     (with-primitive b (rotate (vector 0 0 (sin (time)))))
//     (with-primitive c (rotate (vector 0 0 (sin (time)))))
//
//     ; position a yellow sphere with c's local transform
//     (with-state
//         (concat (with-primitive c (get-transform)))
//         (opacity 0.5)
//         (colour (vector 1 1 0))
//         (draw-sphere))
//
//     ; position a purple sphere with c's global transform
//     (with-state
//         (concat (with-primitive c (get-global-transform)))
//         (opacity 0.5)
//         (colour (vector 1 0 1))
//         (draw-sphere)))
//
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-fr
// get-transform
// Retour: matrice-vecteur
// Description:
// Retourne une matrice représentant l'état courant de transformation
// pour la primitive en cours.
// Exemple:
// (clear)
// ; établissement d'une hiérarchie
// (define a
//     (with-state
//         (colour (vector 1 0.5 0.5))
//         (build-cube)))
// (define b (with-state
//         (colour (vector 0.5 1 0.5))
//         (parent a)
//         (translate (vector 2 0 0))
//         (build-cube)))
// (define c (with-state
//         (colour (vector 0.5 0.5 1))
//         (parent b)
//         (translate (vector 2 0 0))
//         (build-cube)))
//
// (define (animate)
//     ; animation de la hiérarchie
//     (with-primitive a (rotate (vector 0 0 (sin (time)))))
//     (with-primitive b (rotate (vector 0 0 (sin (time)))))
//     (with-primitive c (rotate (vector 0 0 (sin (time)))))
//
//     ; positionne une sphère jaune avec les transformations de 'c'
//     (with-state
//         (concat (with-primitive c (get-transform)))
//         (opacity 0.5)
//         (colour (vector 1 1 0))
//         (draw-sphere))
//
//     ; positionne une sphère jaune avec les transformations globales de 'c'
//     (with-state
//         (concat (with-primitive c (get-global-transform)))
//         (opacity 0.5)
//         (colour (vector 1 0 1))
//         (draw-sphere)))
//
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *get_transform(int argc, Scheme_Object **argv)
{
	return FloatsToScheme(Engine::Get()->State()->Transform.arr(),16);
}

// StartFunctionDoc-en
// get-global-transform
// Returns: matrix-vector
// Description:
// Returns a matrix representing the current global transform for the
// current primitive.
// Example:
// (clear)
// ; build a hierarchy
// (define a
//     (with-state
//         (colour (vector 1 0.5 0.5))
//         (build-cube)))
// (define b (with-state
//         (colour (vector 0.5 1 0.5))
//         (parent a)
//         (translate (vector 2 0 0))
//         (build-cube)))
// (define c (with-state
//         (colour (vector 0.5 0.5 1))
//         (parent b)
//         (translate (vector 2 0 0))
//         (build-cube)))
//
// (define (animate)
//     ; animate the heirarchy
//     (with-primitive a (rotate (vector 0 0 (sin (time)))))
//     (with-primitive b (rotate (vector 0 0 (sin (time)))))
//     (with-primitive c (rotate (vector 0 0 (sin (time)))))
//
//     ; position a yellow sphere with c's local transform
//     (with-state
//         (concat (with-primitive c (get-transform)))
//         (opacity 0.5)
//         (colour (vector 1 1 0))
//         (draw-sphere))
//
//     ; position a purple sphere with c's global transform
//     (with-state
//         (concat (with-primitive c (get-global-transform)))
//         (opacity 0.5)
//         (colour (vector 1 0 1))
//         (draw-sphere)))
//
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-pt
// get-global-transform
// Retorna: matriz-vetor
// Descrição:
// Retorna uma matriz representando a transformação global atual para
// a primitiva atual
// Exemplo:
// (clear)
// ; build a hierarchy
// (define a
//     (with-state
//         (colour (vector 1 0.5 0.5))
//         (build-cube)))
// (define b (with-state
//         (colour (vector 0.5 1 0.5))
//         (parent a)
//         (translate (vector 2 0 0))
//         (build-cube)))
// (define c (with-state
//         (colour (vector 0.5 0.5 1))
//         (parent b)
//         (translate (vector 2 0 0))
//         (build-cube)))
//
// (define (animate)
//     ; animate the heirarchy
//     (with-primitive a (rotate (vector 0 0 (sin (time)))))
//     (with-primitive b (rotate (vector 0 0 (sin (time)))))
//     (with-primitive c (rotate (vector 0 0 (sin (time)))))
//
//     ; position a yellow sphere with c's local transform
//     (with-state
//         (concat (with-primitive c (get-transform)))
//         (opacity 0.5)
//         (colour (vector 1 1 0))
//         (draw-sphere))
//
//     ; position a purple sphere with c's global transform
//     (with-state
//         (concat (with-primitive c (get-global-transform)))
//         (opacity 0.5)
//         (colour (vector 1 0 1))
//         (draw-sphere)))
//
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-fr
// get-global-transform
// Retour: matrice-vecteur
// Description:
// Retourne une matrice représentant l'état global courant de transformation
// pour la primitive en cours.
// Exemple:
// (clear)
// ; établissement d'une hiérarchie
// (define a
//     (with-state
//         (colour (vector 1 0.5 0.5))
//         (build-cube)))
// (define b (with-state
//         (colour (vector 0.5 1 0.5))
//         (parent a)
//         (translate (vector 2 0 0))
//         (build-cube)))
// (define c (with-state
//         (colour (vector 0.5 0.5 1))
//         (parent b)
//         (translate (vector 2 0 0))
//         (build-cube)))
//
// (define (animate)
//     ; animation de la hiérarchie
//     (with-primitive a (rotate (vector 0 0 (sin (time)))))
//     (with-primitive b (rotate (vector 0 0 (sin (time)))))
//     (with-primitive c (rotate (vector 0 0 (sin (time)))))
//
//     ; positionne une sphère jaune avec les transformations de 'c'
//     (with-state
//         (concat (with-primitive c (get-transform)))
//         (opacity 0.5)
//         (colour (vector 1 1 0))
//         (draw-sphere))
//
//     ; positionne une sphère jaune avec les transformations globales de 'c'
//     (with-state
//         (concat (with-primitive c (get-global-transform)))
//         (opacity 0.5)
//         (colour (vector 1 0 1))
//         (draw-sphere)))
//
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *get_global_transform(int argc, Scheme_Object **argv)
{
	if (Engine::Get()->Grabbed())
	{
		SceneNode *a=(SceneNode*)(Engine::Get()->Renderer()->GetSceneGraph().FindNode(Engine::Get()->GrabbedID()));
		if (a)
		{
			return FloatsToScheme(Engine::Get()->Renderer()->GetSceneGraph().GetGlobalTransform(a).arr(),16);
		}
	}
	return scheme_void;
}

// StartFunctionDoc-en
// parent primitive-id
// Returns: void
// Description:
// Parents the current primitive to the supplied parent primitive. The current
// primitive will now be moved around with the parent by acquiring all the parent's
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

// StartFunctionDoc-fr
// parent primitive-id
// Retour: vide
// Description:
// Rend parente la primitive courante de la primitive ciblée. La primitive courante
// sera désormais déplacée avec le parent en prenant toutes les transformations
// des parents.
// Exemple:
// (define parent-prim (build-cube)) ; fabrication du cube parent
// (translate (vector 2 0 0)) ; mouvement léger en X
// (parent parent-prim) ; fix parent-prim comme le parent en cours
// (define child-prim (build-cube)) ; fabrication du cube enfant
// (grab parent-prim)
// (rotate (vector 0 45 0)) ; l'enfant se déplace par cette transformation en plus de la sienne
// (ungrab)
// EndFunctionDoc

Scheme_Object *parent(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("parent", "i", argc, argv);
	Primitive *Grabbed=Engine::Get()->Renderer()->Grabbed();
	if (Grabbed) // reparent...
	{
		Engine::Get()->Renderer()->GetSceneGraph().
			ReparentNode(Engine::Get()->GrabbedID(),IntFromScheme(argv[0]));
		// also set the state, although it probably isn't needed
		Engine::Get()->State()->Parent=IntFromScheme(argv[0]);
	}
	else
	{
		Engine::Get()->State()->Parent=IntFromScheme(argv[0]);
	}
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// line-width value
// Returns: void
// Description:
// Sets the line width (in screen space) of the current drawing state, or the current
// primitive. Affects wireframe and things like that.
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

// StartFunctionDoc-fr
// line-width valeur
// Retour: vide
// Description:
// Fixe la largeur des lignes (à l'écran) de l'état en cours de rendu, ou de la primitive courante.
// N'affecte que les fils de fer.
// Exemple:
// (line-width 5)
// (hint-wire)
// (build-sphere 10 10) ; fabrication d'une sphère en maillage épais
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
// Sets the point width (in screen space) of the current drawing state, or the current
// primitive. Affects point rendering and particles in hardware point mode.
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

// StartFunctionDoc-fr
// point-width valeur
// Retour: vide
// Description:
// Fixe la largeur des lignes (à l'écran) de l'état en cours de rendu, ou de la primitive courante.
// N'affecte que les rendu de points et les particule en mode de points matériel.
// Exemple:
// (point-width 5)
// (hint-points)
// (build-sphere 10 10) ; fabrication d'une sphère en points épais
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
// Sets the blend mode of the current drawing state, or the current primitive.
// This is the way that alpha is composited to the rendering surface. Blendmode symbols can
// consist of:
// zero one dst-color one-minus-dst-color src-alpha one-minus-src-alpha dst-alpha
// one-minus-dst-alpha
// Also 'src-alpha-saturate as an option for the source blendmode only.
// The initial value is 'src-alpha for the source, and 'one-minus-src-alpha for the
// destination blendmode.
// Example:
// ; list out all the possible blendmodes
//
// (define src-blend (vector 'zero 'one 'dst-color 'one-minus-dst-color 'src-alpha
//                     'one-minus-src-alpha 'dst-alpha 'one-minus-dst-alpha
//                     'src-alpha-saturate))
//
// (define dst-blend (vector 'zero 'one 'src-color 'one-minus-src-color 'src-alpha
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
// (define dst-blend (vector 'zero 'one 'src-color 'one-minus-src-color 'src-alpha
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

// StartFunctionDoc-fr
// blend-mode src dst
// Retour: vide
// Description:
// Fice le mode de fusion de l'état en cours de rendu, ou de la primitive courante.
// C'est de cette façon que l'alpha est combiné à la surface de rendu. Le symbole Blendmode
// peut être:
// zero one dst-color one-minus-dst-color src-alpha one-minus-src-alpha dst-alpha
// one-minus-dst-alpha
// Aussi 'src-alpha-saturate en option pour une fusion uniquement avec la source.
// La valeur initiale est 'src-alpha pour la source, et 'one-minus-src-alpha
// pour la destination.
// Exemple:
// ; list de toutes les possibilités de mode de fusion
//
// (define src-blend (vector 'zero 'one 'dst-color 'one-minus-dst-color 'src-alpha
//                     'one-minus-src-alpha 'dst-alpha 'one-minus-dst-alpha
//                     'src-alpha-saturate))
//
// (define dst-blend (vector 'zero 'one 'src-color 'one-minus-src-color 'src-alpha
//                     'one-minus-src-alpha 'dst-alpha 'one-minus-dst-alpha))
//
// ; prend un élément aléatoire
// (define (pick-rnd-item l)
//     (vector-ref l (random (vector-length l))))
//
// ; fabrication de multiples sphères aléatoires
// (define (rnd-sphere n)
//     (push)
//     (hint-depth-sort)
//     (opacity 0.5)
//     (colour (vector (flxrnd) (flxrnd) (flxrnd)))
//
//     ; fixe le mode de fusion
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
	else if (d=="src-color") Engine::Get()->State()->DestinationBlend=GL_SRC_COLOR;
	else if (d=="one-minus-src-color") Engine::Get()->State()->DestinationBlend=GL_ONE_MINUS_SRC_COLOR;
	else if (d=="src-alpha") Engine::Get()->State()->DestinationBlend=GL_SRC_ALPHA;
	else if (d=="one-minus-src-alpha") Engine::Get()->State()->DestinationBlend=GL_ONE_MINUS_SRC_ALPHA;
	else if (d=="dst-alpha") Engine::Get()->State()->DestinationBlend=GL_DST_ALPHA;
	else if (d=="one-minus-dst-alpha") Engine::Get()->State()->DestinationBlend=GL_ONE_MINUS_DST_ALPHA;
	else Trace::Stream<<"dest blend mode not recognised: "<<d<<endl;

	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// hint-on hint-symbol ...
// Returns: void
// Description:
// Render hints change the way that primitives are rendered, but may have
// different effects - or no effect on certain primitive types, hence the
// name hint.
// The following symbols set the render hints of the current drawing state,
// or the current primitive.
// 'solid - solid rendering
// 'wire - wireframe
// 'normal - display normals
// 'points - display points
// 'anti-alias - anti-alias lines
// 'box - display bounding box
// 'vertcols - use vertex colours, overrides the current (colour) state
// 'origin - display object space origin
// 'cast-shadow - cast shadow
// 'depth-test - use depth tests, it is useful to switch off for rendering
//    transparent objects, as it means objects will be shown behind previously
//    rendered ones.
// 'depth-sort - depth sort
// 'lazy-parent - prevent this primitive passing it's transform to it's children
// 'cull-ccw - flips the faces which get backface culled
// 'cull-cw - backface cull clockwise winding order faces (default)
// 'wire-stippled - stippled wireframe
// 'sphere-map - render objects as if they were perfecly reflective
// 'frustum-cull - turn frustum culling, when using frustum culling, make sure
//    you call (recalc-bb) on the primitive too.
// 'normalise - if the current state transform contains a scale transformation,
//    transformed normals might not be unit length, resulting in undesirable
//    lighting problems. This hint makes all normals unit length after they are
//    transformed. This is required if the current state transform contains
//    nonuniform scaling.
// 'blending - use blending, useful to disable if objects with blending rendered
//    into a pixelprimitive.
// 'zwrite - Enables/disables z writes. Useful to disable for sometimes hacking
//    transparency.
// 'lit - turn on lighting
// 'all - all of the hints above
//
// Example:
// (clear)
// (hint-on 'wire 'anti-alias 'origin)
// (hint-off 'solid)
// (build-cube)
//
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-on hint-symbole ...
// Retour: vide
// Description:
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Les symboles suivants fixe les indications de rendu de l'état
// en cours de rendu, ou de la primitive courante.
// 'solid - rendu en solide
// 'wire - fil de fer
// 'normal - affiche les normales
// 'points - affiche les points
// 'anti-alias - ligne avec anti-alias
// 'box - affiche les bôites de rebond
// 'vertcols - utilise les couleurs de vertex, supplante l'état (couleur) courant
// 'origin - affiche l'origine spatiale des objets
// 'cast-shadow - ombres portées
// 'depth-test - utilise le test de profondeur, utile pour le rendu des objets
//               transparents, les objets seront derrières ceux rendu précédement.
// 'depth-sort - rangement par profondeur
// 'lazy-parent - empêche la primitive de passé ses transformation à ses enfants
// 'cull-ccw - inverse les faces qui sont en face arrière
// 'cull-cw - met les faces en face arrière dans le sens des aiguilles d'une montre (default)
// 'wire-stippled - fil de fer en pointillés
// 'sphere-map - les objets sont rendus comme s'ils étaient parfaitement réfléchissante
// 'frustum-cull - active l'élimination par frustrum (pyramide de caméra). En utilisant cette option,
//                 assurez vous d'utiliser sur la primitive (recalc-bb)  également.
// 'normalise - si l'état courant de transformation contient une transformation d'échelle,
//              les normales transformées pourraient ne pas être de longueur unitaire, en résultant
//              des problêmes d'éclairage indésirables. Cette fonction est requise si la transformation
//              d'état courante contient une transforamtion d'échelle non-uniforme.
// 'blending - utilise les modes de fusion, utile si les objets avec fusion sont rendus dans une pixelprimitive.
// 'zwrite - Active/désactive l'écriture en Z. Utile pour certains hacks de transparence
// 'lit - active l'éclairage
// 'all - actives toutes les indications de rendu ci-dessus
//
// Exemple:
// (clear)
// (hint-on 'wire 'anti-alias 'origin)
// (hint-off 'solid)
// (build-cube)
//
// EndFunctionDoc

Scheme_Object *hints(int argc, Scheme_Object **argv, int on)
{
	DECL_ARGV();
	for (int n = 0; n < argc; n++)
	{
		if (!SCHEME_SYMBOLP(argv[n]))
		{
			scheme_wrong_type(on ? "hint-on" : "hint-off",
					"symbol", n, argc, argv);
		}
	}

	unsigned flags = 0;
	unsigned neg_flags = 0;

	for (int n = 0; n < argc; n++)
	{
		string s = SymbolName(argv[n]);
		if (s == "all")
		{
			flags = ~0;
			neg_flags = 0;
		}
		else if (s == "solid")
		{
			flags |= HINT_SOLID;
		}
		else if (s == "wire")
		{
			flags |= HINT_WIRE;
		}
		else if (s == "normal")
		{
			flags |= HINT_NORMAL;
		}
		else if (s == "points")
		{
			flags |= HINT_POINTS;
		}
		else if (s == "anti-alias")
		{
			flags |= HINT_AALIAS;
		}
		else if (s == "box")
		{
			flags |= HINT_BOUND;
		}
		else if (s == "vertcols")
		{
			flags |= HINT_VERTCOLS;
		}
		else if (s == "origin")
		{
			flags |= HINT_ORIGIN;
		}
		else if (s == "cast-shadow")
		{
			flags |= HINT_CAST_SHADOW;
		}
		else if (s == "depth-test")
		{
			neg_flags |= HINT_IGNORE_DEPTH;
		}
		else if (s == "depth-sort")
		{
			flags |= HINT_DEPTH_SORT;
		}
		else if (s == "lazy-parent")
		{
			flags |= HINT_LAZY_PARENT;
		}
		else if (s == "cull-ccw")
		{
			flags |= HINT_CULL_CCW;
		}
		else if (s == "wire-stippled")
		{
			flags |= HINT_WIRE_STIPPLED;
		}
		else if (s == "sphere-map")
		{
			flags |= HINT_SPHERE_MAP;
		}
		else if (s == "frustum-cull")
		{
			flags |= HINT_FRUSTUM_CULL;
		}
		else if (s == "normalise")
		{
			flags |= HINT_NORMALISE;
		}
		else if (s == "blending")
		{
			neg_flags |= HINT_NOBLEND;
		}
		else if (s == "zwrite")
		{
			neg_flags |= HINT_NOZWRITE;
		}
		else if (s == "lit")
		{
			neg_flags |= HINT_UNLIT;
		}
		else if (s == "cull-cw")
		{
			neg_flags |= HINT_CULL_CCW;
		}
		else
		{
			Trace::Stream << "hint symbol not recognised: " << s << endl;
		}
	}

	if (on)
	{
		Engine::Get()->State()->Hints |= flags;
		Engine::Get()->State()->Hints &= ~neg_flags;
	}
	else
	{
		Engine::Get()->State()->Hints |= neg_flags;
		Engine::Get()->State()->Hints &= ~flags;
	}

	MZ_GC_UNREG();
    return scheme_void;
}

Scheme_Object *hint_on(int argc, Scheme_Object **argv)
{
	return hints(argc, argv, 1);
}

// StartFunctionDoc-en
// hint-off hint-symbol ...
// Returns: void
// Description:
// Render hints change the way that primitives are rendered, but may have
// different effects - or no effect on certain primitive types, hence the
// name hint.
// (hint-off) disables the render hints of the current drawing state,
// or the current primitive. Check (hint-on) for more information on possible
// hint symbols.
//
// Example:
// (clear)
// (hint-on 'wire 'anti-alias 'origin)
// (hint-off 'solid)
// (build-cube)
//
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-off hint-symbole ...
// Retour: vide
// Description:
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// (hint-off) désactive les indications de rendu de l'état
// en cours de rendu, ou de la primitive courante.
// Recherchez (hint-on) pour plus d'information sur les symboles possibles.
//
// Exemple:
// (clear)
// (hint-on 'wire 'anti-alias 'origin)
// (hint-off 'solid)
// (build-cube)
//
// EndFunctionDoc

Scheme_Object *hint_off(int argc, Scheme_Object **argv)
{
	return hints(argc, argv, 0);
}

// StartFunctionDoc-en
// hint-solid
// Returns: void
// Description:
// Sets the render hints to solid of the current drawing state, or the current
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

// StartFunctionDoc-fr
// hint-solid
// Retour: vide
// Description:
// Active l'indication de rendu solide à l'état en cours de rendu, ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Exemple:
// (hint-solid) ; ceci est le style de rendu par défaut, ce qui n'est donc pas très excitant
// (build-cube) ; fabrique un cube solide
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
// Sets the render hints to wireframe of the current drawing state, or the current
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

// StartFunctionDoc-fr
// hint-wire
// Retour: vide
// Description:
// Active l'indication de rendu en fil de fer à l'état en cours de rendu, ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Exemple:
// (hint-wire)
// (build-cube) ; fabrication d'un cube avec rendu du maillage
// EndFunctionDoc

Scheme_Object *hint_wire(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_WIRE;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-wire-stippled
// Returns: void
// Description:
// Sets the render hints to stippled wireframe of the current drawing state, or the
// current primitive.
// Example:
// (hint-none)
// (hint-wire-stippled)
// (build-cube) ; make a stippled wirefame cube
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-wire-stippled
// Retorna: void
// Descrição:
// Ajusta as dicas de render para fios pontilhados no estado de
// desenho ou primitiva atual.
// Exemplo:
// (hint-none)
// (hint-wire-stippled)
// (build-cube) ; make a stippled wirefame cube
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-wire-stippled
// Retour: vide
// Description:
// Active l'indication de rendu en fil de fer en pointillés à l'état en cours de rendu,
// ou à la primitive courante. Les indications de rendu changent la façon dont
// les primitives sont rendues, mais peuvent avoir différents effets - ou aucun effet -
// sur certains types de primitives, d'où l'appellation indication (hint).
// Exemple:
// (hint-none)
// (hint-wire-stippled)
// (build-cube) ; fabrication d'un cube avec rendu du maillage en pointillés
// EndFunctionDoc

Scheme_Object *hint_wire_stippled(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_WIRE_STIPPLED;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-frustum-cull
// Returns: void
// Description:
// Sets the render hints to turn frustum culling on for the current drawing state, or the
// current primitive. Render hints change the way that primitives are rendered, but may have
// different effects - or no effect on certain primitive types, hence the name hint.
// When using frustum culling, make sure you call (recalc-bb) on the primitive too.
// Example:
// (hint-frustum-cull)
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-frustum-cull
// Retorna: void
// Descrição:
// Ajusta as dicas de render para ativar frustum culling no estado de
// desenho, ou primitiva atual. Dicas de render muda a forma como as
// primitivas são renderizadas, mas podem ter efeitos diferentes - ou
// não ter efeito em certos tipos de primitivas, por isso o nome
// dica. Quando usar frustum culling, certifique-se de chamar
// (recalc-bb) na primitiva também.
// Exemplo:
// (hint-frustum-cull)
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-frustum-cull
// Retour: vide
// Description:
// Active l'indication de rendu de la limitation au frustrum à l'état en cours de rendu,
// ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Lors de l'utilisation du frustrum culling, assurez vous d'appeller également (recalc-bb)
// sur la primitive.
// Exemple:
// (hint-frustum-cull)
// EndFunctionDoc

Scheme_Object *hint_frustum_cull(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_FRUSTUM_CULL;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-normalise
// Returns: void
// Description:
// If the current state transform contains a scale transformation, transformed
// normals might not be unit length, resulting in undesirable lighting problems.
// (hint-normalise) makes all normals unit length after they are transformed. This
// is required if the current state transform contains nonuniform scaling.
// Example:
// (clear)
// (hint-normalise)
// (build-cube)
// ; non uniform scaling
// (with-primitive (build-cube)
//    (translate #(.5 0 0))
//    (scale #(3 1 1))
//    (translate #(.5 0 0)))
// ; uniform scaling
// (with-primitive (build-cube)
//    (translate #(0 0 2))
//    (scale 2))
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-normalise
// Retorna: void
// Descrição:
// Se o estado de transformação atual contém uma escalagem, as normais
// transformadas podem não ser unitárias em tamanho, resultando em
// problemas indesejáveis em relação à iluminação. (hint-normalise)
// faz todas as normais serem de tamanho unitário depois de terem sido
// transformadas. Isto é requerido se o estado de desenho atual contém
// escalagem não uniforme.
// Exemplo:
// (clear)
// (hint-normalise)
// (build-cube)
// ; non uniform scaling
// (with-primitive (build-cube)
//    (translate #(.5 0 0))
//    (scale #(3 1 1))
//    (translate #(.5 0 0)))
// ; uniform scaling
// (with-primitive (build-cube)
//    (translate #(0 0 2))
//    (scale 2))
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-normalise
// Retour: vide
// Description:
// Si l'état courant de transformation contient une transformation d'échelle,
// les normales transformées pourraient ne pas être de longueur unitaire, en résultant
// des problêmes d'éclairage indésirables. Cette fonction est requise si la transformation
// d'état courante contient une transforamtion d'échelle non-uniforme.
// Exemple:
// (clear)
// (hint-normalise)
// (build-cube)
// ; échelle non-uniform
// (with-primitive (build-cube)
//    (translate #(.5 0 0))
//    (scale #(3 1 1))
//    (translate #(.5 0 0)))
// ; échelle uniforme
// (with-primitive (build-cube)
//    (translate #(0 0 2))
//    (scale 2))
// EndFunctionDoc

Scheme_Object *hint_normalise(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_NORMALISE;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-noblend
// Returns: void
// Description:
// Disables blending. Useful if objects with blending rendered into
// a pixelprimitive.
// Example:
// (clear)
// (hint-wire)
// (scale #(9.8 8 1))
// (translate #(-1 -.5 0))
// (define p0 (build-pixels 512 512 #t))
// (with-pixels-renderer p0
//    (hint-ignore-depth)
//    (with-primitive (build-particles 2048)
//        (pdata-map! (lambda (p) (vmul (crndvec) 2)) "p")
//        (pdata-map! (lambda (c) #(1 .5)) "c")))
//
// (hint-noblend)
// (translate #(1 0 0))
// (define p1 (build-pixels 512 512 #t))
// (with-pixels-renderer p1
//    (hint-ignore-depth)
//    (with-primitive (build-particles 2048)
//        (pdata-map! (lambda (p) (vmul (crndvec) 2)) "p")
//        (pdata-map! (lambda (c) #(1 .5)) "c")))
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-noblend
// Retorna: void
// Descrição:
// Desativa blending. Útil se objetos com blending render estão num pixelprimitive.
// Exemplo:
// (clear)
// (hint-wire)
// (scale #(9.8 8 1))
// (translate #(-1 -.5 0))
// (define p0 (build-pixels 512 512 #t))
// (with-pixels-renderer p0
//    (hint-ignore-depth)
//    (with-primitive (build-particles 2048)
//        (pdata-map! (lambda (p) (vmul (crndvec) 2)) "p")
//        (pdata-map! (lambda (c) #(1 .5)) "c")))
//
// (hint-noblend)
// (translate #(1 0 0))
// (define p1 (build-pixels 512 512 #t))
// (with-pixels-renderer p1
//    (hint-ignore-depth)
//    (with-primitive (build-particles 2048)
//        (pdata-map! (lambda (p) (vmul (crndvec) 2)) "p")
//        (pdata-map! (lambda (c) #(1 .5)) "c")))
// EndFunctionDoc


// StartFunctionDoc-fr
// hint-noblend
// Retour: vide
// Description:
// Désactive les modes de fusion. Utile si les objets sont rendus
// dans une pixelprimitive.
// Exemple:
// (clear)
// (hint-wire)
// (scale #(9.8 8 1))
// (translate #(-1 -.5 0))
// (define p0 (build-pixels 512 512 #t))
// (with-pixels-renderer p0
//    (hint-ignore-depth)
//    (with-primitive (build-particles 2048)
//        (pdata-map! (lambda (p) (vmul (crndvec) 2)) "p")
//        (pdata-map! (lambda (c) #(1 .5)) "c")))
//
// (hint-noblend)
// (translate #(1 0 0))
// (define p1 (build-pixels 512 512 #t))
// (with-pixels-renderer p1
//    (hint-ignore-depth)
//    (with-primitive (build-particles 2048)
//        (pdata-map! (lambda (p) (vmul (crndvec) 2)) "p")
//        (pdata-map! (lambda (c) #(1 .5)) "c")))
// EndFunctionDoc

Scheme_Object *hint_noblend(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_NOBLEND;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-nozwrite
// Returns: void
// Description:
// Disables z writes. Useful for sometimes hacking transparency.
// Example:
// (clear)
// (hint-noblend)
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-noswrite
// Retorna: void
// Descrição:
// Desativa escrita em z. Útil para algumas transparencias hackeadas.
// Exemplo:
// (clear)
// (hint-noblend)
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-nozwrite
// Retour: vide
// Description:
// Désactive l'écriture en z. Utile pour certains hacks de transparence.
// Exemple:
// (clear)
// (hint-noblend)
// EndFunctionDoc

Scheme_Object *hint_nozwrite(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_NOZWRITE;
    return scheme_void;
}

// StartFunctionDoc-en
// line-pattern factor pattern
// Returns: void
// Description:
// Factor specifies a multiplier for each bit in the line stipple pattern.
// Pattern specifies a 16-bit integer whose bit pattern determines which
// fragments of a line will be drawn when the line is rasterized.
// Example:
// (hint-none)
// (hint-wire-stippled)
// (line-pattern 4 #x0aaaa)
// (build-cube) ; make a stippled wirefame cube
// EndFunctionDoc

// StartFunctionDoc-pt
// line-pattern fator padrão
// Retorna: void
// Descrição:
// Fator especifica um multiplicador para cada bit no padrão da linha
// pontilhada. Padrão especifica um inteiro de 16 bits em que o padrão
// do bit determina qual fragmento da linha vai ser desenhado quando a
// linha é rasterizada.
// Exemplo:
// (hint-none)
// (hint-wire-stippled)
// (line-pattern 4 #x0aaaa)
// (build-cube) ; make a stippled wirefame cube
// EndFunctionDoc

// StartFunctionDoc-fr
// line-pattern facteur motif
// Retour: vide
// Description:
// Le facteur spécifie un multiplicateur pour chaque bit du motif de
// la ligne en pointillé. Le motif est un entier de 16-bits, le motif
// des bits détermine quels fragments de la ligne seront dessinés quand
// la ligne est calculée.
// Exemple:
// (hint-none)
// (hint-wire-stippled)
// (line-pattern 4 #x0aaaa)
// (build-cube) ; fabrique un cube au maillage en pointillé
// EndFunctionDoc

Scheme_Object *line_pattern(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("line-pattern", "ii", argc, argv);
	Engine::Get()->State()->StippleFactor=IntFromScheme(argv[0]);
	Engine::Get()->State()->StipplePattern=IntFromScheme(argv[1]);
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// hint-normal
// Returns: void
// Description:
// Sets the render hints to display normals in the current drawing state, or the current
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

// StartFunctionDoc-fr
// hint-normal
// Retour: vide
// Description:
// Active l'indication de rendu d'affichage des normales à l'état en cours de rendu,
// ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Exemple:
// (hint-normal)
// (build-cube) ; affiche les normales du cube
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
// Sets the render hints to display points in the current drawing state, or the current
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

// StartFunctionDoc-fr
// hint-points
// Retour: vide
// Description:
// Active l'indication de rendu d'affichage des points à l'état en cours de rendu,
// ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Exemple:
// (hint-points)
// (build-cube) ; affiche les sommets du cube
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
// Sets the render hints to anti-alias in the current drawing state, or the current
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

// StartFunctionDoc-fr
// hint-anti-alias
// Retour: vide
// Description:
// Active l'indication de rendu avec anti-aliasing à l'état en cours de rendu,
// ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Exemple:
// (hint-anti-alias)
// (build-cube) ; affiche un cube lissé
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
// Sets the render hints to unlit in the current drawing state, or the current
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

// StartFunctionDoc-fr
// hint-unlit
// Retour: vide
// Description:
// Active l'indication de rendu de non-éclairage à l'état en cours de rendu,
// ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Exemple:
// (hint-unlit)
// (build-cube) ; affiche un cube sans-éclairage
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
// current primitive. Render hints change the way that primitives are rendered,
// but may have different effects - or no effect on certain primitive types, hence the
// name hint. Vertex colours override the current (colour) state.
// Example:
// (clear)
// (hint-vertcols)
// (define mycube (build-cube)) ; make a cube with vertcols enabled
//
// (with-primitive mycube
//     (pdata-map!
//         (lambda (c)
//             (rndvec)) ; randomise the vertcols
//         "c"))
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
// (clear)
// (hint-vertcols)
// (define mycube (build-cube)) ; make a cube with vertcols enabled
//
// (with-primitive mycube
//     (pdata-map!
//         (lambda (c)
//             (rndvec)) ; randomise the vertcols
//         "c"))
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-vertcols
// Retour: vide
// Description:
// Active l'indication de rendu d'utilisation des couleurs des vertex
// à l'état en cours de rendu, ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Les couleurs de vertex supplante l'état courant (couleur)
// Exemple:
// (clear)
// (hint-vertcols)
// (define mycube (build-cube)) ; fabrique un cube avec vertcols activé
//
// (with-primitive mycube
//     (pdata-map!
//         (lambda (c)
//             (rndvec)) ; donne des couleurs aléatoires aux sommets
//         "c"))
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
// current primitive. Render hints change the way that primitives are rendered,
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

// StartFunctionDoc-fr
// hint-box
// Retour: vide
// Description:
// Active l'indication de rendu d'affichage des boîtes de rebond
// à l'état en cours de rendu, ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Exemple:
// (hint-box)
// (build-sphere 10 10) ; fabrique une sphère avec la boîte de rebond affichée
// EndFunctionDoc

Scheme_Object *hint_box(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_BOUND;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-none
// Returns: void
// Description:
// Clears the render hints in the current drawing state, or the current primitive.
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

// StartFunctionDoc-fr
// hint-none
// Retour: vide
// Description:
// Efface les indications de rendu de l'état en cours de rendu, ou de la primitive courante.
// Ceci permet principalement de retourner au style solide par défaut, mais siginifie également
// que vous pouvez activer ou désactiver les indication sans utiliser les (push) et (pop).
// Exemple:
// (hint-none)
// (hint-wire)
// (build-cube) ; fabrique un cube dont seul le maillage est visible
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
// current primitive. Render hints change the way that primitives are rendered,
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

// StartFunctionDoc-fr
// hint-points
// Retour: vide
// Description:
// Active l'indication de rendu d'affichage des points à l'état en cours de rendu,
// ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Exemple:
// (hint-points)
// (build-cube) ; affiche les sommets du cube
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-origin
// Retour: vide
// Description:
// Active l'indication de rendu d'affichage des origines spatiales des objets
// à l'état en cours de rendu, ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Exemple:
// (hint-origin)
// (build-sphere 10 10) ; fabrique une sphère avec son origine affichée
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
// current primitive. Render hints change the way that primitives are rendered,
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

// StartFunctionDoc-fr
// hint-cast-shadow
// Retour: vide
// Description:
// (note: Pas encore implémenté)
// Active l'indication de rendu de projection des ombre
// à l'état en cours de rendu, ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Exemple:
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
// current primitive. Render hints change the way that primitives are rendered,
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

// StartFunctionDoc-fr
// hint-depth-sort
// Retour: vide
// Description:
// Active l'indication de rendu d'ordonnancement par profondeur à l'état en cours de rendu,
// ou à la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
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
// current primitive. Render hints change the way that primitives are rendered,
// but may have different effects - or no effect on certain primitive types, hence the
// name hint. This feature is useful for rendering transparent objects, as it means objects
// will be shown behind previously rendered ones.
// Example:
// (clear)
// (with-state
//     (hint-ignore-depth)
//     (opacity 0.6)
//     (with-state
//         (colour (vector 1 0 0))
//         (build-cube))
//     (with-state
//         (colour (vector 0 1 0))
//         (translate (vector 1 0 0))
//         (build-cube)))
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
// (clear)
// (with-state
//     (hint-ignore-depth)
//     (opacity 0.6)
//     (with-state
//         (colour (vector 1 0 0))
//         (build-cube))
//     (with-state
//         (colour (vector 0 1 0))
//         (translate (vector 1 0 0))
//         (build-cube)))
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-ignore-depth
// Retour: vide
// Description:
// Active l'indication de rendu d'ignorer les tests de profondeur pour l'état en cours de rendu,
// ou la primitive courante.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Cette fonctionnalité est utile pour le rendu d'objets transparents, les objets seront
// dessinés derrière les objets déjà précédemment dessinés.
// Exemple:
// (clear)
// (with-state
//     (hint-ignore-depth)
//     (opacity 0.6)
//     (with-state
//         (colour (vector 1 0 0))
//         (build-cube))
//     (with-state
//         (colour (vector 0 1 0))
//         (translate (vector 1 0 0))
//         (build-cube)))
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
// (hint-lazy-parent)
// (build-sphere 10 10) ; make a sphere with the origin displayed
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-lazy-parent
// Retour: vide
// Description:
// Active l'indication de rendu d'annuler le passage des informations de transformation
// aux objets enfants.
// Les indications de rendu changent la façon dont les primitives sont rendues,
// mais peuvent avoir différents effets - ou aucun effet - sur certains types
// de primitives, d'où l'appellation indication (hint).
// Exemple:
// (hint-lazy-parent)
// (build-sphere 10 10)
// EndFunctionDoc

Scheme_Object *hint_lazy_parent(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_LAZY_PARENT;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-cull-ccw
// Returns: void
// Description:
// Flips the faces which get backface culled
// Example:
// (hint-cull-ccw)
// (build-sphere 10 10) ; make an inside out
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-cull-ccw
// Retorna: void
// Descrição:
// Vira as faces das quais a face anterior é excluida(culled).
// Exemplo:
// (hint-cull-ccw)
// (build-sphere 10 10) ; make an inside out
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-cull-ccw
// Retour: vide
// Description:
// Retourne les faces qui sont sont en face arrière.
// Exemple:
// (hint-cull-ccw)
// (build-sphere 10 10) ; met l'interieur à l'exterieur
// EndFunctionDoc

Scheme_Object *hint_cull_ccw(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_CULL_CCW;
    return scheme_void;
}

// StartFunctionDoc-en
// hint-sphere-map
// Returns: void
// Description:
// Sets the render hints to render objects as if they were perfecly reflective
// for the current drawing state, or the current primitive.
// Example:
// (clear)
// (hint-sphere-map)
// (texture (load-texture "test.png"))
// (define p (build-torus 1 2 20 20))
// (every-frame (with-primitive p
//                 (rotate #(.543 .59 .87))))
// EndFunctionDoc

// StartFunctionDoc-pt
// hint-sphere-map
// Retorna: void
// Descrição:
// Ajusta as dicas para renderizar objetos como se eles fossem
// perfeitamente refletivos para o estado ou primitiva atual.
// Exemplo:
// (clear)
// (hint-sphere-map)
// (texture (load-texture "test.png"))
// (define p (build-torus 1 2 20 20))
// (every-frame (with-primitive p
//                 (rotate #(.543 .59 .87))))
// EndFunctionDoc

// StartFunctionDoc-fr
// hint-sphere-map
// Retour: vide
// Description:
// Active l'indication de rendu pour le texturage comme si l'objet
// était totalement réfléchissant pour l'état en cours de rendu,
// ou la primitive courante.
// Exemple:
// (clear)
// (hint-sphere-map)
// (texture (load-texture "test.png"))
// (define p (build-torus 1 2 20 20))
// (every-frame (with-primitive p
//                 (rotate #(.543 .59 .87))))
// EndFunctionDoc

Scheme_Object *hint_sphere_map(int argc, Scheme_Object **argv)
{
    Engine::Get()->State()->Hints|=HINT_SPHERE_MAP;
    return scheme_void;
}

// StartFunctionDoc-en
// texture textureid-number
// Returns: void
// Description:
// Sets the texture of the current drawing state, or the current
// primitive. Texture ids can be generated by the load-texture function.
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

// StartFunctionDoc-fr
// texture textureid-nombre
// Retour: vide
// Description:
// Fixe la texture pour l'état en cours de rendu, ou la primitive courante.
// Les identifiants de texture peuvent être généré par la fonction (load-texture).
// Exemple:
// (texture (load-texture "mytexture.png"))
// (build-sphere 10 10) ; fabrique une sphère texturée avec mytexture.png
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
// Sets the texture of the current drawing state, or the current
// primitive in the same way as the texture function, but allows you to specify
// the texture unit (0-7) to apply the texture to. Multitexturing allows you to apply different
// textures and texture coordinates to the same object at once. Texture unit 0 is the default one
// (which uses the pdata "t" for it's texture coords) texture unit n looks for pdata "tn" - ie
// multitexture 1 looks for "t1". You need to add these yourself using (pdata-add) or (pdata-copy).
// Multitexturing is useful when the textures contain alpha, as they can be overlayed, i.e. decals
// placed on background textures.
// Example:
// (clear)
// (define p (build-torus 1 2 20 20))
//
// (with-primitive p
//     (multitexture 0 (load-texture "refmap.png"))
//     (multitexture 1 (load-texture "transp.png")))
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
// Exemplo:
// (define obj (build-sphere 10 10)) ; make a sphere
// (grab obj)
// (multitexture 0 (load-texture "mytextureA.png"))
// (multitexture 1 (load-texture "mytextureB.png"))
// (pdata-add "t1" "v")   ; make some texture coords for texture B
// (pdata-copy "t" "t1")  ; copy them from the default texture coords
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-fr
// multitexture textureunit-nombre textureid-nombre
// Retour: vide
// Description:
// Fixe la texture pour l'état en cours de rendu, ou la primitive courante de la même
// façon que la fonction (texture), met permet de spécifier l'unité de texture à
// appliquer sur la texture. Le Multitexturing permet d'appliquer différentes textures et
// coordonnées de textures au même objet en une opération. L'unité de texture par défaut est 0
// (qui utilise pdata "t" pour ses coordonnées de texture). L'unité de texture 'n' cherche ses coordonnées
// auprès de pdata "tn".
// Par exemple, la multitexture 1 cherche "t1". Vous devez les ajouter vous-même en utilisant (pdata-add) ou
// (pdata-copy). Le MultiTexturage est utile lorsque les textures contienne un alpha, ainsi elle peuvent
// être superposées, par exemple : des décalcomanies sur une texture de fond.
// Exemple:
// (clear)
// (define p (build-torus 1 2 20 20))
//
// (with-primitive p
//     (multitexture 0 (load-texture "refmap.png"))
//     (multitexture 1 (load-texture "transp.png")))
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

// StartFunctionDoc-fr
// print-scene-graph
// Retour: vide
// Description:
// Imprime le graphique de scène courante, utile pour le débugage.
// Exemple:
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
// Sets the hidden state for the current primitive (also affects all child primitives). Hidden primitives
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

// StartFunctionDoc-fr
// hide hidden-nombre
// Retour: vide
// Description:
// Fixe l'état de dissimulation de la primitive courante (affecte également les primitives enfants).
// Les primitives masquées peuvent être traité comme d'habitude, elles ne seront juste pas affichées.
// Exemple:
// (define obj (build-cube))
// (grab obj)
// (hide 1) ; masque le cube
// (ungrab)
// EndFunctionDoc

Scheme_Object *hide(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
    ArgCheck("hide", "i", argc, argv);
  if (Engine::Get()->Grabbed())
  {
    if (IntFromScheme(argv[0]))
    {
      // hiding...
      Engine::Get()->Grabbed()->SetVisibility(0x00000000);
    }
    else
    {
      // unhiding...
      Engine::Get()->Grabbed()->SetVisibility(0xffffffff);
    }
  }
  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// camera-hide hidden-number
// Returns: void
// Description:
// Sets the hidden state for the current primitive, with the current camera (also affects all child primitives).
// Allows you to turn off rendering for primitives under different cameras
// Example:
// (define obj (build-cube))
// (with-primitive obj
//     (camera-hide 1)) ; hide this cube
// EndFunctionDoc

// StartFunctionDoc-pt
// camera-hide número-escondido
// Retorna: void
// Descrição:
// Ajusta os estado escondido para a primitiva atual, com a camêra
// atual (também afeta todas as primitivas descendentes). Permite que
// você desligue a renderização para as primitivas em camêras diferentes.
// Exemplo:
// (define obj (build-cube))
// (with-primitive obj
//     (camera-hide 1)) ; hide this cube
// EndFunctionDoc

// StartFunctionDoc-fr
// camera-hide hidden-nombre
// Retour: vide
// Description:
// Fixe l'état de dissimulation de la primitive courante, avec la caméra en cours,
// affecte également les primitives enfants.
// Permet de désactiver le rendu de certaines primitives pour différentes caméras.
// Exemple:
// (define obj (build-cube))
// (with-primitive obj
//     (camera-hide 1)) ; masque le cube
// EndFunctionDoc

Scheme_Object *camera_hide(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
    ArgCheck("camera-hide", "i", argc, argv);
  if (Engine::Get()->Grabbed())
  {
    unsigned int currentvis = Engine::Get()->Grabbed()->GetVisibility();
    unsigned int cameracode = 0x00000001<<Engine::Get()->GrabbedCamera();

    if (IntFromScheme(argv[0]))
    {
      // hiding...
      Engine::Get()->Grabbed()->SetVisibility((~cameracode) & currentvis);
    }
    else
    {
      // unhiding...
      Engine::Get()->Grabbed()->SetVisibility(cameracode | currentvis);
    }
  }
  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// selectable selectable-number
// Returns: void
// Description:
// Sets whether the current primitive can be selected or not using the select command.
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
// Ajusta se a primitiva pega pode ser selecionada ou não, usando o
// comando select.
// Exemplo:
// (define obj (build-cube))
// (grab obj)
// (selectable 0) ; agora ela nao vai ser "vista", quando chamar select
// (ungrab)
// EndFunctionDoc

// StartFunctionDoc-fr
// selectable selectable-nombre
// Retour: vide
// Description:
// Indique si la primitive peut être sélectionnées grâce à la commande (select).
// Example:
// (define obj (build-cube))
// (grab obj)
// (selectable 0) ; le cube ne sera pas détecté par la commande (select)
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
// backfacecull setting-number
// Returns: void
// Description:
// Turns backface culling on or off. Backface culling speeds up rendering by removing faces not
// orientated towards the camera. Defaults to on, but this is not always desired, eg for double
// sided polygons.
// Example:
// (backfacecull 0)
// EndFunctionDoc

// StartFunctionDoc-pt
// backfacecull número-ajuste
// Retorna: void
// Descrição:
// Liga ou desliga o corte de face-traseira. Backface culling acelera
// a renderização removendo faces não orientadas em direção da
// câmera. É ligado por padrão, mas isto não é desejado sempre, eg
// para poligonos com dupla face.
// Exemplo:
// (backfacecull 0)
// EndFunctionDoc

// StartFunctionDoc-fr
// backfacecull setting-nombre
// Retour: vide
// Description:
// Activé ou désactive l'élimination des face arrières. Cette fonction accélerre le rendu en supprimant
// les faces non orientées vers la caméra. Activée par défaut, mais pas toujours désirable,
// par exemple pour les polygones doubles faces.
// Exemple:
// (backfacecull 0)
// EndFunctionDoc

Scheme_Object *backfacecull(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("backfacecull", "i", argc, argv);
  Engine::Get()->State()->Cull=IntFromScheme(argv[0]);
  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// shader vertexprogram-string fragmentprogram-string
// Returns: void
// Description:
// Loads, compiles and sets the GLSL hardware shader pair for the current drawing state, or the
// current primitive. Requires OpenGL 2 support.
// The shader's uniform data can be controlled via shader-set! and all the pdata is sent through as
// per-vertex attribute data to the shader.
// Example:
// ; you need to have built fluxus with GLSL=1
// (clear)
// (define s (with-state
//     ; assign the shaders to the surface
//     (shader "simple.vert.glsl" "simple.frag.glsl")
//     (build-sphere 20 20)))
//
// (with-primitive s
//     ; add and set the pdata - this is then picked up in the vertex shader
//     ; as an input attribute called "testcol"
//     (pdata-add "testcol" "v")
//     ; set the testcol pdata with a random colour for every vertex
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "testcol"))
//
// (define (animate)
//     (with-primitive s
//         ; animate the deformamount uniform input parameter
//         (shader-set! (list "deformamount" (cos (time))))))
//
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-pt
// shader vertexprograma-string fragmentprogram-string
// Retorna: void
// Descrição:
// Abre, compila e ajusta o par de GLSL hardware shader para o estado
// de desenho atual, ou a primitiva pega. Requer OpenGL 2.
// Os dados uniformes do shader podem ser controlados via shader-set!
// e todas as pdatas são enviadas como um atributo por vértice ao
// shader.
// Exemplo:
// ; you need to have built fluxus with GLSL=1
// (clear)
// (define s (with-state
//     ; assign the shaders to the surface
//     (shader "simple.vert.glsl" "simple.frag.glsl")
//     (build-sphere 20 20)))
//
// (with-primitive s
//     ; add and set the pdata - this is then picked up in the vertex shader
//     ; as an input attribute called "testcol"
//     (pdata-add "testcol" "v")
//     ; set the testcol pdata with a random colour for every vertex
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "testcol"))
//
// (define (animate)
//     (with-primitive s
//         ; animate the deformamount uniform input parameter
//         (shader-set! (list "deformamount" (cos (time))))))
//
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-fr
// shader vertexprogram-chaine-de-caractère fragmentprogram-chaine-de-caractère
// Retour: vide
// Description:
// Charge, compile et active les paires de shaders GLSL matériels pour l'état de rendu actuel,
// ou la primitive en cours. OpenGL2 requis.
// Les données uniformisées des shaders peuvent être contrôlées via shader-set! et toutes les pdata
// sont envoyées par données d'attributs de vertex au shader.
// Exemple:
// ; vous devez avoir compilé fluxus avec l'option GLSL=1
// (clear)
// (define s (with-state
//     ; assigne les shaders à la surface
//     (shader "simple.vert.glsl" "simple.frag.glsl")
//     (build-sphere 20 20)))
//
// (with-primitive s
//     ; ajoute et fixe les pdata - qui sont ensuite récupérées par le vertex shader
//     ; comme un attribut d'entrée appellé "testcol"
//     (pdata-add "testcol" "v")
//     ; rempli le testcol pdata avec des couleurs aléatoires pour chaque vertex
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "testcol"))
//
// (define (animate)
//     (with-primitive s
//         ; anime le paramètre de gain de transformation uniforme
//         (shader-set! (list "deformamount" (cos (time))))))
//
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *shader(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
    ArgCheck("shader", "ss", argc, argv);

  string vert=StringFromScheme(argv[0]);
  string frag=StringFromScheme(argv[1]);

  if (Engine::Get()->State()->Shader &&
      Engine::Get()->State()->Shader->DecRef())
  {
    delete Engine::Get()->State()->Shader;
  }

  Engine::Get()->State()->Shader = ShaderCache::Get(vert,frag);

  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// shader-source vertexprogram-source-string fragmentprogram-source-string
// Returns: void
// Description:
// Same as shader, but uses the supplied strings as shader sourcecode.
// This allows you to embed GLSL shader source inside your scheme scripts.
// Example:
// ; you need to have built fluxus with GLSL=1
// EndFunctionDoc

// StartFunctionDoc-pt
// shader vertexprograma-string fragmentprogram-string
// Retorna: void
// Descrição:
// Exemplo:
// ; you need to have built fluxus with GLSL=1
// (clear)
// EndFunctionDoc

// StartFunctionDoc-fr
// shader-source vertexprogram-source-chaine-de-caractère fragmentprogram-source-chaine-de-caractère
// Retour: vide
// Description:
// Identique à shader, mais utilise la chaîne de caractère comme code source
// du shader.
// Ceci permet d'embarquer le code GLSL dans les scripts scheme.
// Same as shader, but uses the supplied strings as shader sourcecode.
// Exemple:
// ; vous devez avoir compilé fluxus avec l'option GLSL=1
// EndFunctionDoc

Scheme_Object *shader_source(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("shader-source", "ss", argc, argv);

	string vert=StringFromScheme(argv[0]);
	string frag=StringFromScheme(argv[1]);

	if (Engine::Get()->State()->Shader &&
		Engine::Get()->State()->Shader->DecRef())
	{
		delete Engine::Get()->State()->Shader;
	}

	Engine::Get()->State()->Shader = ShaderCache::Make(vert,frag);

	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// clear-shader-cache
// Returns: void
// Description:
// Clears the shader cache.
// Example:
// (clear-shader-cache)
// EndFunctionDoc

// StartFunctionDoc-pt
// clear-shader-cache
// Retorna: número-id-primitiva
// Descrição:
// Limpa o cache do shader.
// Exemplo:
// (clear-shader-cache)
// EndFunctionDoc

// StartFunctionDoc-fr
// clear-shader-cache
// Retour: vide
// Description:
// Vide le cache des shaders
// Exemple:
// (clear-shader-cache)
// EndFunctionDoc

Scheme_Object *clear_shader_cache(int argc, Scheme_Object **argv)
{
  ShaderCache::Clear();
  return scheme_void;
}

// StartFunctionDoc-en
// shader-set! [parameter-name-keyword parameter-value ...] argument-list
// Returns: void
// Description:
// Sets the uniform shader parameters for the GLSL shader. Parameters are
// keyword value pairs which relate to the corresponding shader parameter
// name and value.
// (shader-set!) also accepts a list consisting of token-string value pairs
// for backward compatibility.
// Example:
// (clear)
// (define s (with-state
//     ; assign the shaders to the surface
//     (shader "simple.vert.glsl" "simple.frag.glsl")
//     (build-sphere 20 20)))
//
// (with-primitive s
//     ; add and set the pdata - this is then picked up in the vertex shader
//     ; as an input attribute called "testcol"
//     (pdata-add "testcol" "v")
//     ; set the testcol pdata with a random colour for every vertex
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "testcol"))
//
// (define (animate)
//     (with-primitive s
//         ; animate the deformamount uniform input parameter
//         (shader-set! #:deformamount (cos (time)))))
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
// ; you need to have built fluxus with GLSL=1
// (clear)
// (define s (with-state
//     ; assign the shaders to the surface
//     (shader "simple.vert.glsl" "simple.frag.glsl")
//     (build-sphere 20 20)))
//
// (with-primitive s
//     ; add and set the pdata - this is then picked up in the vertex shader
//     ; as an input attribute called "testcol"
//     (pdata-add "testcol" "v")
//     ; set the testcol pdata with a random colour for every vertex
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "testcol"))
//
// (define (animate)
//     (with-primitive s
//         ; animate the deformamount uniform input parameter
//         (shader-set! (list "deformamount" (cos (time))))))
//
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-fr
// shader-set! [paramètre-nom-motclef paramètre-valeur ...] argument-liste
// Retour: vide
// Description:
// Détermine les paramètres uniformes pour le shader GLSL. Les paramètres
// sont des paires de mots clés et leur valeurs, qui correspondent aux paramètres
// des shader nome/valeur.
// (shader-set!) accepte aussi un liste constituée de chaîne de caractères marquée
// pour la rétro-compatibilité.
// Exemple:
// (clear)
// (define s (with-state
//     ; assigne les shaders à la surface
//     (shader "simple.vert.glsl" "simple.frag.glsl")
//     (build-sphere 20 20)))
//
// (with-primitive s
//     ; ajoute, et fixe les pdata - récupérées ensuite par le shader de vertex
//     ; comme attribut d'entrée appellé "testcol"
//     (pdata-add "testcol" "v")
//     ; fixe les pdata "testcol" par des couleur aléatoire pour chaque vertex
//     (pdata-map!
//         (lambda (c)
//             (rndvec))
//         "testcol"))
//
// (define (animate)
//     (with-primitive s
//         ; anime le paramètre de gain de transformation uniforme
//         (shader-set! #:deformamount (cos (time)))))
//
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *shader_set(int argc, Scheme_Object **argv)
{
	Scheme_Object *paramvec = NULL;
	Scheme_Object *listvec = NULL;
	MZ_GC_DECL_REG(3);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, paramvec);
	MZ_GC_VAR_IN_REG(2, listvec);
	MZ_GC_REG();

	ArgCheck("shader-set!", "l", argc, argv);

	if (Engine::Get()->State()->Shader!=NULL)
	{
		GLSLShader *shader=Engine::Get()->State()->Shader;

		// vectors seem easier to handle than lists with this api
		paramvec = scheme_list_to_vector(argv[0]);

		// apply to set parameters
		shader->Apply();

		for (int n=0; n<SCHEME_VEC_SIZE(paramvec); n+=2)
		{
			if (SCHEME_CHAR_STRINGP(SCHEME_VEC_ELS(paramvec)[n]) && SCHEME_VEC_SIZE(paramvec)>n+1)
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
					// set vec2f, vec3f, vec4f uniform variables
					listvec = SCHEME_VEC_ELS(paramvec)[n + 1];
					int vecsize = SCHEME_VEC_SIZE(listvec);

					if ((2 <= vecsize) && (vecsize <= 4))
					{
						dVector vec;
						FloatsFromScheme(listvec, vec.arr(), vecsize);
						shader->SetVector(param, vec, vecsize);
					}
					else
					if (vecsize == 16)
					{
						dMatrix m;
						FloatsFromScheme(listvec, m.arr(), vecsize);
						shader->SetMatrix(param, m);
					}
					else
					{
						Trace::Stream << "shader is expecting vector size 2, 3, 4 or 16 but found " << vecsize <<
							" for variable " << param << endl;
					}
				}
				else if (SCHEME_LISTP(SCHEME_VEC_ELS(paramvec)[n+1]))
				{
					listvec = scheme_list_to_vector(SCHEME_VEC_ELS(paramvec)[n+1]);
					unsigned int sz = SCHEME_VEC_SIZE(listvec);
					if (sz>0)
					{
						if (SCHEME_NUMBERP(SCHEME_VEC_ELS(listvec)[0]))
						{
							if (SCHEME_EXACT_INTEGERP(SCHEME_VEC_ELS(listvec)[0]))
							{
								vector<int, FLX_ALLOC(int) > array;
								for (unsigned int i=0; i<sz; i++)
								{
									if (!SCHEME_EXACT_INTEGERP(SCHEME_VEC_ELS(listvec)[i]))
									{
										Trace::Stream<<"found a dodgy element in a uniform array"<<endl;
										break;
									}
									array.push_back(IntFromScheme(SCHEME_VEC_ELS(listvec)[i]));
								}
								shader->SetIntArray(param,array);
							}
							else
							{
								vector<float, FLX_ALLOC(float) > array;
								for (unsigned int i=0; i<sz; i++)
								{
									if (!SCHEME_NUMBERP(SCHEME_VEC_ELS(listvec)[i]))
									{
										Trace::Stream<<"found a dodgy element in a uniform array"<<endl;
										break;
									}
									array.push_back(FloatFromScheme(SCHEME_VEC_ELS(listvec)[i]));
								}
								shader->SetFloatArray(param,array);
							}
						}
						else if (SCHEME_VECTORP(SCHEME_VEC_ELS(listvec)[0]))
						{
							if (SCHEME_VEC_SIZE(SCHEME_VEC_ELS(listvec)[0]) == 3)
							{
								vector<dVector, FLX_ALLOC(dVector) > array;
								for (unsigned int i=0; i<sz; i++)
								{
									if (!SCHEME_VECTORP(SCHEME_VEC_ELS(listvec)[i]) ||
										SCHEME_VEC_SIZE(SCHEME_VEC_ELS(listvec)[i]) != 3)
									{
										Trace::Stream<<"found a dodgy element in a uniform array"<<endl;
										break;
									}
									dVector vec;
									FloatsFromScheme(SCHEME_VEC_ELS(listvec)[i],vec.arr(),3);
									array.push_back(vec);
								}
								shader->SetVectorArray(param,array);
							}
							else if (SCHEME_VEC_SIZE(SCHEME_VEC_ELS(listvec)[0]) == 4)
							{
								vector<dColour, FLX_ALLOC(dColour) > array;
								for (unsigned int i=0; i<sz; i++)
								{
									if (!SCHEME_VECTORP(SCHEME_VEC_ELS(listvec)[i]) ||
										SCHEME_VEC_SIZE(SCHEME_VEC_ELS(listvec)[i]) != 4)
									{
										Trace::Stream<<"found a dodgy element in a uniform array"<<endl;
										break;
									}
									dColour vec;
									FloatsFromScheme(SCHEME_VEC_ELS(listvec)[i],vec.arr(),4);
									array.push_back(vec);
								}
								shader->SetColourArray(param,array);
							}
							else
							{
								Trace::Stream<<"shader has found a vector argument list of a strange size"<<endl;
							}
						}
					}
				}
				else
				{
					Trace::Stream<<"shader has found an argument type it can't send, numbers and vectors, or lists of them only"<<endl;
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

// StartFunctionDoc-en
// texture-params texture-unit-number parameter-list
// Returns: void
// Description:
// Sets the current texture state for the specified texture unit. This state controls how the texture is applied
// to the surface of the object, and how it's filtered in texels, or miplevels. The texture unit is used if you
// are using multitexturing - the default texture unit is 0. You may need to read up a bit on OpenGL or experiment
// to find out more about these options.
// Example:
// ; parameters are the following:
// ; tex-env : [modulate decal blend replace]
// ; min : [nearest linear nearest-mipmap-nearest linear-mipmap-nearest linear-mipmap-linear]
// ; mag : [nearest linear]
// ; wrap-s : [clamp repeat]
// ; wrap-t : [clamp repeat]
// ; wrap-r : [clamp repeat] (for cube maps)
// ; border-colour : (vector of length 4)
// ; priority : real number 0 -> 1
// ; env-colour : (vector of length 4)
// ; min-lod : real number (for mipmap blending - default -1000)
// ; max-lod : real number (for mipmap blending - default 1000)
// (texture-params 0 '(min nearest mag nearest))
// EndFunctionDoc

// StartFunctionDoc-pt
// texture-params número-unidade-textura lista-parametros
// Retorna: void
// Descrição:
// Ajusta o estado de textura atual para a unidade de textura
// especificada. Esse estado controla como a textura é aplicada à
// superfície do objeto, e como é filtrada nos texels, ou miplevels. A
// unidade de textura é usada se você está usando multitextura - o
// padrão da unidade de textura é 0. Você talvez precise ler um pouco
// sobre OpenGL ou experimentar para descubrir mais sobre essas
// opções.
// Exemplo:
// ; parameters are the following:
// ; tex-env : [modulate decal blend replace]
// ; min : [nearest linear nearest-mipmap-nearest linear-mipmap-nearest linear-mipmap-linear]
// ; mag : [nearest linear]
// ; wrap-s : [clamp repeat]
// ; wrap-t : [clamp repeat]
// ; wrap-r : [clamp repeat] (for cube maps)
// ; border-colour : (vector of length 4)
// ; priority : real number 0 -> 1
// ; env-colour : (vector of length 4)
// ; min-lod : real number (for mipmap blending - default -1000)
// ; max-lod : real number (for mipmap blending - default 1000)
// (texture-params 0 '(min nearest mag nearest))
// EndFunctionDoc

// StartFunctionDoc-fr
// texture-params texture-unit-nombre paramètre-liste
// Retour: vide
// Description:
// Détermine l'état de la texture courante pour l'unité de texture spécifiée. Cet état contrôle comme la texture
// est appliquée sur la surface de l'objet, et comment sont filtrer les texels, ou miplevels. L'unité de texture
// est utilisée si vous employez le multitexturing - l'unité de texture par défaut est 0. Vous pourriez avoir
// besoin de lire sur l'OpenGL ou expérimenter pour découvrir la signification de chacune de ces options.
// Exemple:
// ; les paramètres sont les suivants:
// ; tex-env : [modulate decal blend replace]
// ; min : [nearest linear nearest-mipmap-nearest linear-mipmap-nearest linear-mipmap-linear]
// ; mag : [nearest linear]
// ; wrap-s : [clamp repeat]
// ; wrap-t : [clamp repeat]
// ; wrap-r : [clamp repeat] (pour le mapping en cube)
// ; border-colour : (vector of length 4)
// ; priority : real number 0 -> 1
// ; env-colour : (vector of length 4)
// ; min-lod : real number (for mipmap blending - default -1000)
// ; max-lod : real number (for mipmap blending - default 1000)
// (texture-params 0 '(min nearest mag nearest))
// EndFunctionDoc

Scheme_Object *texture_params(int argc, Scheme_Object **argv)
{
  Scheme_Object *paramvec = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, argv);
  MZ_GC_VAR_IN_REG(1, paramvec);
  MZ_GC_REG();

  ArgCheck("texture-params", "il", argc, argv);

  int n = IntFromScheme(argv[0]);
  if (n>=MAX_TEXTURES)
  {
    MZ_GC_UNREG();
      return scheme_void;
  }

  paramvec = scheme_list_to_vector(argv[1]);
  TextureState *state = &Engine::Get()->State()->TextureStates[n];

  for (int n=0; n<SCHEME_VEC_SIZE(paramvec); n+=2)
  {
    if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(paramvec)[n]) && SCHEME_VEC_SIZE(paramvec)>n+1)
    {
      // get the parameter name
      string param = SymbolName(SCHEME_VEC_ELS(paramvec)[n]);
      if (param=="tex-env")
      {
        if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(paramvec)[n+1]))
        {
          string type=SymbolName(SCHEME_VEC_ELS(paramvec)[n+1]);
          if (type=="modulate") state->TexEnv = GL_MODULATE;
          else if (type=="decal") state->TexEnv = GL_DECAL;
          else if (type=="blend") state->TexEnv = GL_BLEND;
          else if (type=="replace") state->TexEnv = GL_REPLACE;
          else Trace::Stream<<"texture-params: unknown parameter for "<<param<<": "<<type<<endl;
        }
        else Trace::Stream<<"texture-params: wrong type for "<<param<<endl;
      }
      else if (param=="min")
      {
        if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(paramvec)[n+1]))
        {
          string type=SymbolName(SCHEME_VEC_ELS(paramvec)[n+1]);
          if (type=="nearest") state->Min = GL_NEAREST;
          else if (type=="linear") state->Min = GL_LINEAR;
          else if (type=="nearest-mipmap-nearest") state->Min = GL_NEAREST_MIPMAP_NEAREST;
          else if (type=="linear-mipmap-nearest") state->Min = GL_LINEAR_MIPMAP_NEAREST;
          else if (type=="linear-mipmap-linear") state->Min = GL_LINEAR_MIPMAP_LINEAR;
          else Trace::Stream<<"texture-params: unknown parameter for "<<param<<": "<<type<<endl;
        }
        else Trace::Stream<<"texture-params: wrong type for "<<param<<endl;
      }
      else if (param=="mag")
      {
        if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(paramvec)[n+1]))
        {
          string type=SymbolName(SCHEME_VEC_ELS(paramvec)[n+1]);
          if (type=="nearest") state->Mag = GL_NEAREST;
          else if (type=="linear") state->Mag = GL_LINEAR;
          else Trace::Stream<<"texture-params: unknown parameter for "<<param<<": "<<type<<endl;
        }
        else Trace::Stream<<"texture-params: wrong type for "<<param<<endl;
      }
      else if (param=="wrap-s")
      {
        if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(paramvec)[n+1]))
        {
          string type=SymbolName(SCHEME_VEC_ELS(paramvec)[n+1]);
          if (type=="clamp") state->WrapS = GL_CLAMP;
          else if (type=="repeat") state->WrapS = GL_REPEAT;
          else Trace::Stream<<"texture-params: unknown parameter for "<<param<<": "<<type<<endl;
        }
        else Trace::Stream<<"texture-params: wrong type for "<<param<<endl;
      }
      else if (param=="wrap-t")
      {
        if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(paramvec)[n+1]))
        {
          string type=SymbolName(SCHEME_VEC_ELS(paramvec)[n+1]);
          if (type=="clamp") state->WrapT = GL_CLAMP;
          else if (type=="repeat") state->WrapT = GL_REPEAT;
          else Trace::Stream<<"texture-params: unknown parameter for "<<param<<": "<<type<<endl;
        }
        else Trace::Stream<<"texture-params: wrong type for "<<param<<endl;
      }
      else if (param=="wrap-r")
      {
        if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(paramvec)[n+1]))
        {
          string type=SymbolName(SCHEME_VEC_ELS(paramvec)[n+1]);
          if (type=="clamp") state->WrapR = GL_CLAMP;
          else if (type=="repeat") state->WrapR = GL_REPEAT;
          else Trace::Stream<<"texture-params: unknown parameter for "<<param<<": "<<type<<endl;
        }
        else Trace::Stream<<"texture-params: wrong type for "<<param<<endl;
      }
      else if (param=="border-colour")
      {
        if (SCHEME_VECTORP(SCHEME_VEC_ELS(paramvec)[n+1]) &&
          SCHEME_VEC_SIZE(SCHEME_VEC_ELS(paramvec)[n+1]) == 4)
        {
          dColour vec;
          FloatsFromScheme(SCHEME_VEC_ELS(paramvec)[n+1],vec.arr(),4);
          state->BorderColour=vec;
        }
        else Trace::Stream<<"texture-params: wrong type for "<<param<<endl;
      }
      else if (param=="priority")
      {
        if (SCHEME_NUMBERP(SCHEME_VEC_ELS(paramvec)[n+1]) &&
            SCHEME_REALP(SCHEME_VEC_ELS(paramvec)[n+1]))
        {
          state->Priority = FloatFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]);
        }
        else Trace::Stream<<"texture-params: wrong type for "<<param<<endl;
      }
      else if (param=="env-colour")
      {
        if (SCHEME_VECTORP(SCHEME_VEC_ELS(paramvec)[n+1]) &&
          SCHEME_VEC_SIZE(SCHEME_VEC_ELS(paramvec)[n+1]) == 4)
        {
          dColour vec;
          FloatsFromScheme(SCHEME_VEC_ELS(paramvec)[n+1],vec.arr(),4);
          state->EnvColour=vec;
        }
        else Trace::Stream<<"texture-params: wrong type for "<<param<<endl;
      }
      else if (param=="min-lod")
      {
        if (SCHEME_NUMBERP(SCHEME_VEC_ELS(paramvec)[n+1]) &&
            SCHEME_REALP(SCHEME_VEC_ELS(paramvec)[n+1]))
        {
          state->MinLOD = FloatFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]);
        }
        else Trace::Stream<<"texture-params: wrong type for "<<param<<endl;
      }
      else if (param=="max-lod")
      {
        if (SCHEME_NUMBERP(SCHEME_VEC_ELS(paramvec)[n+1]) &&
            SCHEME_REALP(SCHEME_VEC_ELS(paramvec)[n+1]))
        {
          state->MaxLOD = FloatFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]);
        }
        else Trace::Stream<<"texture-params: wrong type for "<<param<<endl;
      }
      else Trace::Stream<<"texture-params: unknown parameter "<<param<<endl;

    }
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
	scheme_add_global("apply-transform",scheme_make_prim_w_arity(apply,"apply",0,1), env);
	scheme_add_global("identity",scheme_make_prim_w_arity(flux_identity,"identity",0,0), env);
	scheme_add_global("concat",scheme_make_prim_w_arity(concat,"concat",1,1), env);
	scheme_add_global("translate",scheme_make_prim_w_arity(translate,"translate",1,1), env);
	scheme_add_global("rotate",scheme_make_prim_w_arity(rotate,"rotate",1,1), env);
	scheme_add_global("scale",scheme_make_prim_w_arity(scale,"scale",1,1), env);
	scheme_add_global("get-transform", scheme_make_prim_w_arity(get_transform, "get-transform", 0, 0), env);
	scheme_add_global("get-global-transform", scheme_make_prim_w_arity(get_global_transform, "get-global-transform", 0, 0), env);
	scheme_add_global("colour",scheme_make_prim_w_arity(colour,"colour",1,1), env);
	scheme_add_global("rgb->hsv",scheme_make_prim_w_arity(rgbtohsv,"rgb->hsv",1,1), env);
	scheme_add_global("hsv->rgb",scheme_make_prim_w_arity(hsvtorgb,"hsv->rgb",1,1), env);
	scheme_add_global("colour-mode",scheme_make_prim_w_arity(colour_mode,"colour-mode",1,1), env);
	scheme_add_global("wire-colour",scheme_make_prim_w_arity(wire_colour,"wire-colour",1,1), env);
	scheme_add_global("normal-colour",scheme_make_prim_w_arity(normal_colour,"normal-colour",1,1), env);
	scheme_add_global("opacity",scheme_make_prim_w_arity(opacity,"opacity",1,1), env);
	scheme_add_global("wire-opacity",scheme_make_prim_w_arity(wire_opacity,"wire-opacity",1,1), env);
	scheme_add_global("specular",scheme_make_prim_w_arity(specular,"specular",1,1), env);
	scheme_add_global("ambient",scheme_make_prim_w_arity(ambient,"ambient",1,1), env);
	scheme_add_global("emissive",scheme_make_prim_w_arity(emissive,"emissive",1,1), env);
	scheme_add_global("shinyness",scheme_make_prim_w_arity(shinyness,"shinyness",1,1), env);
	scheme_add_global("texture",scheme_make_prim_w_arity(texture,"texture",1,1), env);
	scheme_add_global("multitexture",scheme_make_prim_w_arity(multitexture,"multitexture",2,2), env);
	scheme_add_global("hint-on",scheme_make_prim_w_arity(hint_on,"hint-on",0,-1), env);
	scheme_add_global("hint-off",scheme_make_prim_w_arity(hint_off,"hint-off",0,-1), env);
	scheme_add_global("hint-solid",scheme_make_prim_w_arity(hint_solid,"hint-solid",0,0), env);
	scheme_add_global("hint-wire",scheme_make_prim_w_arity(hint_wire,"hint-wire",0,0), env);
	scheme_add_global("hint-wire-stippled",scheme_make_prim_w_arity(hint_wire_stippled,"hint-wire-stippled",0,0), env);
	scheme_add_global("hint-normal",scheme_make_prim_w_arity(hint_normal,"hint-normal",0,0), env);
	scheme_add_global("hint-points",scheme_make_prim_w_arity(hint_points,"hint-points",0,0), env);
	scheme_add_global("hint-anti-alias",scheme_make_prim_w_arity(hint_anti_alias,"hint-anti-alias",0,0), env);
	scheme_add_global("hint-none",scheme_make_prim_w_arity(hint_none,"hint-none",0,0), env);
	scheme_add_global("hint-unlit",scheme_make_prim_w_arity(hint_unlit,"hint-unlit",0,0), env);
	scheme_add_global("hint-vertcols",scheme_make_prim_w_arity(hint_vertcols,"hint-vertcols",0,0), env);
	scheme_add_global("hint-box",scheme_make_prim_w_arity(hint_box,"hint-box",0,0), env);
	scheme_add_global("hint-origin",scheme_make_prim_w_arity(hint_origin,"hint-origin",0,0), env);
	scheme_add_global("hint-cast-shadow",scheme_make_prim_w_arity(hint_cast_shadow,"hint-cast-shadow",0,0), env);
	scheme_add_global("hint-ignore-depth",scheme_make_prim_w_arity(hint_ignore_depth,"hint-ignore-depth",0,0), env);
	scheme_add_global("hint-depth-sort",scheme_make_prim_w_arity(hint_depth_sort,"hint-depth-sort",0,0), env);
	scheme_add_global("hint-lazy-parent",scheme_make_prim_w_arity(hint_lazy_parent,"hint-lazy-parent",0,0), env);
	scheme_add_global("hint-cull-ccw",scheme_make_prim_w_arity(hint_cull_ccw,"hint-cull-ccw",0,0), env);
	scheme_add_global("hint-sphere-map",scheme_make_prim_w_arity(hint_sphere_map,"hint-sphere-map",0,0), env);
	scheme_add_global("hint-frustum-cull",scheme_make_prim_w_arity(hint_frustum_cull,"hint-frustum-cull",0,0), env);
	scheme_add_global("hint-normalise",scheme_make_prim_w_arity(hint_normalise,"hint-normalise",0,0), env);
	scheme_add_global("hint-noblend",scheme_make_prim_w_arity(hint_noblend,"hint-noblend",0,0), env);
	scheme_add_global("hint-nozwrite",scheme_make_prim_w_arity(hint_nozwrite,"hint-nozwrite",0,0), env);
	scheme_add_global("line-width",scheme_make_prim_w_arity(line_width,"line-width",1,1), env);
	scheme_add_global("line-pattern",scheme_make_prim_w_arity(line_pattern,"line-pattern",2,2), env);
	scheme_add_global("point-width",scheme_make_prim_w_arity(point_width,"point-width",1,1), env);
	scheme_add_global("blend-mode",scheme_make_prim_w_arity(blend_mode,"blend-mode",2,2), env);
	scheme_add_global("parent",scheme_make_prim_w_arity(parent,"parent",1,1), env);
	scheme_add_global("hide",scheme_make_prim_w_arity(hide,"hide",1,1), env);
	scheme_add_global("camera-hide",scheme_make_prim_w_arity(camera_hide,"camera-hide",1,1), env);
	scheme_add_global("selectable",scheme_make_prim_w_arity(selectable,"selectable",1,1), env);
	scheme_add_global("shader",scheme_make_prim_w_arity(shader,"shader",2,2), env);
	scheme_add_global("shader-source",scheme_make_prim_w_arity(shader_source,"shader-source",2,2), env);
	scheme_add_global("clear-shader-cache",scheme_make_prim_w_arity(clear_shader_cache,"clear-shader-cache",0,0), env);
	scheme_add_global("shader-set!",scheme_make_prim_w_arity(shader_set,"shader-set!",1,1), env);
	scheme_add_global("texture-params",scheme_make_prim_w_arity(texture_params,"texture-params",2,2), env);
	scheme_add_global("backfacecull",scheme_make_prim_w_arity(backfacecull,"backfacecull",1,1), env);
	MZ_GC_UNREG();
}
