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
#include "GlobalStateFunctions.h"
#include "Renderer.h"

using namespace GlobalStateFunctions;
using namespace SchemeHelper;
using namespace Fluxus;

// StartSectionDoc-en
// global-state
// Global state is really anything that controls the renderer globally, so it affects all primitives
// or controls the renderer directly - ie camera control or full screen effects like blurring.
// Example:
// EndSectionDoc

// StartSectionDoc-pt
// estado-global
// Estado global é realmente qualquer coisa que controla o
// renderizador globalmente, então ele afeta todas as primitivas ou
// controla o renderizador diretamente - p.e. controle de câmera ou
// efeitos de tela cheia como "embaçamento".
// Exemplo:
// EndSectionDoc

// StartSectionDoc-fr
// global-state
// L'état global est tout ce qui contrôle l'afficheur vraiment globalement, ceci affecte donc toutes
// les primitives ou contrôle les afficheurs directement. Par exemple le control des caméras ou les
// effets plein-écran comme le flou (blur). 
// Exemple:
// EndSectionDoc

// StartFunctionDoc-en
// clear-engine
// Returns: void
// Description:
// Clears the renderer, and physics system. This command should not be called directly, use clear
// instead, as this clears a few other things, and calls clear-engine itself.
// Example:
// (clear-engine) ; woo hoo!
// EndFunctionDoc

// StartFunctionDoc-pt
// clear-engine
// Retorna: void
// Descrição:
// Limpa o renderizador, e o sistema de física. Este comando não deve
// ser chamado diretamente, use clear ao invés, já que limpa algumas
// outras coisas também, e chama clear-engine ele mesmo.
// Exemplo:
// (clear-engine) ; woo hoo!
// EndFunctionDoc

// StartFunctionDoc-fr
// clear-engine
// Retour: void
// Description:
// Efface l'afficheur et le systême physique. Cette commande ne devrait pas être appellée directement,
// utiliser plutôt clear qui efface plusieurs autres choses, et appelle clear-engine elle-même. 
// Exemple:
// (clear-engine) ; woo hoo!
// EndFunctionDoc

Scheme_Object *clear_engine(int argc, Scheme_Object **argv)
{
  Engine::Get()->Renderer()->Clear();
  Engine::Get()->Physics()->Clear();
  Engine::Get()->Renderer()->ClearLights();
  Engine::Get()->ClearGrabStack();
  Engine::Get()->Renderer()->UnGrab();
  Engine::Get()->GetPFuncContainer()->Clear();
  return scheme_void;
}

// StartFunctionDoc-en
// blur amount-number
// Returns: void
// Description:
// Sets the full screen blur setting. Less is more, but if you set it too low it will make the
// on screen editing impossible to read, so save your script first :)
// Example:
// (blur 0.1) ; for nice trails
// EndFunctionDoc

// StartFunctionDoc-pt
// blur número-quantidade
// Retorna: void
// Descrição:
// Ajusta a opção de blur na tela inteira. Menos é mais, mas se você
// ajustar isto muito baixo vai fazer com que a edição na tela fique
// impossível de ler, então salve seus scripts primeiro :).
// Exemplo:
// (blur 0.1) ; para belos rastros
// EndFunctionDoc

// StartFunctionDoc-fr
// blur quantité-nombre
// Retour: void
// Description:
// Fixe la valeur de l'effet de flou plein-écran. Moins la valeur est grande, plus l'effet est important,
// mais s'il est trop bas, celà rend la lecture à l'écran impossible.
// Donc, penser à sauvegarder le script avant.
// Exemple:
// (blur 0.1) ; pour de jolies trainées
// EndFunctionDoc

Scheme_Object *blur(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("blur", "f", argc, argv);
  float blur=FloatFromScheme(argv[0]);
  if (!blur) Engine::Get()->Renderer()->SetMotionBlur(false);
    else Engine::Get()->Renderer()->SetMotionBlur(true, blur);
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// fog fogcolour-vector amount-number begin-number end-number
// Returns: void
// Description:
// Sets the fogging parameters to give a visual depth cue (aerial perspective in painter's jargon).
// This can obscure the on screen editing, so keep the amount small.
// Example:
// (clear-colour (vector 0 0 1))   ; looks nice if the background matches
// (fog (vector 0 0 1) 0.01 1 100) ; blue fog
// EndFunctionDoc

// StartFunctionDoc-pt
// fog cor-nuvem-vetor número-quantidade número-ínicio númeor-final
// Retorna: void
// Descrição:
// Ajusta os paramêtros da neblina pra dar uma indicação de
// profundidade visual (perspectiva aérea no jargão de pintores). Isto
// pode obscurecer a edição na tela, então mantenha a quantidade baixa.
// Exemplo:
// (clear-colour (vector 0 0 1)) ; fica legal se o fundo de tela bate.
// (fog (vector 0 0 1) 0.01 1 100) ; neblina azul
// EndFunctionDoc

// StartFunctionDoc-fr
// fog couleur-brouillard-vector quantité-nombre début-nombre fin-nombre
// Retour: void
// Description:
// Fixe les paramètres de brouillard pour donner une profondeur optique
// (profondeur de champs en jargon de peintre)
// Ceci peut obsurcir l'édition à l'écran, donc garder la quantité faible. 
// Exemple:
// (clear-colour (vector 0 0 1))   ; rends bien si le fond correspond
// (fog (vector 0 0 1) 0.01 1 100) ; brouillard bleu
// EndFunctionDoc

Scheme_Object *fog(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("fog", "cfff", argc, argv);
  Engine::Get()->Renderer()->SetFog(ColourFromScheme(argv[0]),
    FloatFromScheme(argv[1]),
    FloatFromScheme(argv[2]),
    FloatFromScheme(argv[3]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// show-axis show-number
// Returns: void
// Description:
// Shows the worldspace origin axis used.
// Example:
// (show-axis 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// show-axis número-mostrar
// Retorna: void
// Descrição:
// Mostra os eixos de origem do espaço usado;
// Exemplo:
// (show-axis 1)
// EndFunctionDoc

// StartFunctionDoc-fr
// show-axis montrer-nombre
// Retour: void
// Description:
// Montre les axes d'origine de l'espace.
// Exemple:
// (show-axis 1)
// EndFunctionDoc

Scheme_Object *show_axis(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("show-axis", "i", argc, argv);
    Engine::Get()->Renderer()->ShowAxis(IntFromScheme(argv[0]));
    //Fluxus->ShowLocators(IntFromScheme(argv[0]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// show-fps show-number
// Returns: void
// Description:
// Shows an fps count in the lower left of the screen.
// used.
// Example:
// (show-fps 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// show-fps número-mostrar
// Retorna: void
// Descrição:
// Mostra uma contaem de fps na parte inferior esquerda da tela.
// Exemplo:
// (show-fps 1)
// EndFunctionDoc

// StartFunctionDoc-fr
// show-fps montrer-nombre
// Retour: void
// Description:
// Montre le compte fps dans le coin gauche de l'écran.
// Exemple:
// (show-fps 1)
// EndFunctionDoc

Scheme_Object *show_fps(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("show-fps", "i", argc, argv);
    Engine::Get()->Renderer()->SetFPSDisplay(IntFromScheme(argv[0]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// lock-camera primitiveid-number
// Returns: void
// Description:
// Locks the camera transform onto the specified primitive's transform. It's like parenting the camera
// to the object. This is the easiest way to procedurally drive the camera. Use an id number of 0 to
// unlock the camera.
// Example:
// (clear)
// (define obj (build-cube)) ; make a cube for the camera to lock to
//
// (with-state ; make a background cube so we can tell what's happening
//     (hint-wire)
//     (hint-unlit)
//     (texture (load-texture "test.png"))
//     (colour (vector 0.5 0.5 0.5))
//     (scale (vector -20 -10 -10))
//     (build-cube))
//
// (lock-camera obj) ; lock the camera to our first cube
// (camera-lag 0.1)  ; set the lag amount, this will smooth out the cube jittery movement
//
// (define (animate)
//     (with-primitive obj
//         (identity)
//         (translate (vector (fmod (time) 5) 0 0)))) ; make a jittery movement
//
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-pt
// lock-camera número-id-primitiva
// Retorna: void
// Descrição:
// Trava a transformação da camera em cima da transformação do objeto
// específicado. É como parentear a câmera ao objeto. Esta é a forma
// mais fácil de dirigir a câmera proceduralmente. Use um número id de
// 0 para destravar a câmera.
// Exemplo:
// (clear)
// (define obj (build-cube)) ; make a cube for the camera to lock to
//
// (with-state ; make a background cube so we can tell what's happening
//     (hint-wire)
//     (hint-unlit)
//     (texture (load-texture "test.png"))
//     (colour (vector 0.5 0.5 0.5))
//     (scale (vector -20 -10 -10))
//     (build-cube))
//
// (lock-camera obj) ; lock the camera to our first cube
// (camera-lag 0.1)  ; set the lag amount, this will smooth out the cube jittery movement
//
// (define (animate)
//     (with-primitive obj
//         (identity)
//         (translate (vector (fmod (time) 5) 0 0)))) ; make a jittery movement
//
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-fr
// lock-camera primitiveid-nombre
// Retour: void
// Description:
// Bloque la transformation caméra sur les transformation d'une primitive spécifiée. Celà s'apparente
// a parenter la caméra à un objet. C'est la manière la plus simple de commander la caméra en procédural. 
// Utiliser le nombre identifiant 0 pour débloquer la caméra.
// Exemple:
// (clear)
// (define obj (build-cube)) ; make a cube for the camera to lock to
//
// (with-state ; make a background cube so we can tell what's happening
//     (hint-wire)
//     (hint-unlit)
//     (texture (load-texture "test.png"))
//     (colour (vector 0.5 0.5 0.5))
//     (scale (vector -20 -10 -10))
//     (build-cube))
//
// (lock-camera obj) ; lock the camera to our first cube
// (camera-lag 0.1)  ; set the lag amount, this will smooth out the cube jittery movement
//
// (define (animate)
//     (with-primitive obj
//         (identity)
//         (translate (vector (fmod (time) 5) 0 0)))) ; make a jittery movement
//
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *lock_camera(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("lock-camera", "i", argc, argv);
    Engine::Get()->GetCamera()->LockCamera( IntFromScheme(argv[0]) );
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// camera-lag amount-number
// Returns: void
// Description:
// The camera locking has an inbuilt lagging which means it will smoothly blend the movement relative
// to the primitive it's locked to.
// Example:
// (clear)
// (define obj (build-cube)) ; make a cube for the camera to lock to
//
// (with-state ; make a background cube so we can tell what's happening
//     (hint-wire)
//     (hint-unlit)
//     (texture (load-texture "test.png"))
//     (colour (vector 0.5 0.5 0.5))
//     (scale (vector -20 -10 -10))
//     (build-cube))
//
// (lock-camera obj) ; lock the camera to our first cube
// (camera-lag 0.1)  ; set the lag amount, this will smooth out the cube jittery movement
//
// (define (animate)
//     (with-primitive obj
//         (identity)
//         (translate (vector (fmod (time) 5) 0 0)))) ; make a jittery movement
//
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-pt
// camera-lag número-quantidade
// Retorna: void
// Descrição:
// O travamento da câmera tem um atraso construído junto o que
// significa que o movimento vai ser macio em relativo a primitiva ao
// qual ela está travada.
// Exemplo:
// (clear)
// (define obj (build-cube)) ; make a cube for the camera to lock to
//
// (with-state ; make a background cube so we can tell what's happening
//     (hint-wire)
//     (hint-unlit)
//     (texture (load-texture "test.png"))
//     (colour (vector 0.5 0.5 0.5))
//     (scale (vector -20 -10 -10))
//     (build-cube))
//
// (lock-camera obj) ; lock the camera to our first cube
// (camera-lag 0.1)  ; set the lag amount, this will smooth out the cube jittery movement
//
// (define (animate)
//     (with-primitive obj
//         (identity)
//         (translate (vector (fmod (time) 5) 0 0)))) ; make a jittery movement
//
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-fr
// camera-lag quantité-nombre
// Retour: void
// Description:
// Le verrou de caméra possède un lag intégré, ce qui signifie qu'il va doucement s'adapter au mouvemement
// de  la primitive à laquelle il est rattaché.
// Exemple:
// (clear)
// (define obj (build-cube)) ; make a cube for the camera to lock to
//
// (with-state ; make a background cube so we can tell what's happening
//     (hint-wire)
//     (hint-unlit)
//     (texture (load-texture "test.png"))
//     (colour (vector 0.5 0.5 0.5))
//     (scale (vector -20 -10 -10))
//     (build-cube))
//
// (lock-camera obj) ; lock the camera to our first cube
// (camera-lag 0.1)  ; set the lag amount, this will smooth out the cube jittery movement
//
// (define (animate)
//     (with-primitive obj
//         (identity)
//         (translate (vector (fmod (time) 5) 0 0)))) ; make a jittery movement
//
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *camera_lag(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("camera-lag", "f", argc, argv);
    Engine::Get()->GetCamera()->SetCameraLag(FloatFromScheme(argv[0]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// load-texture pngfilename-string optional-create-params-list
// Returns: textureid-number
// Description:
// Loads a texture from disk, converts it to a texture, and returns the id number. The texture width and height
// should be power of two on older graphics cards. Usually there are no size restriction on newer cards, but if
// you experience unexpected results make the texture size multiple of 4. The texture loading
// is memory cached, so repeatedly calling this will not cause it to load again. The cache can be cleared
// with clear-texture-cache. The png may be RGB or RGBA to use alpha transparency. To get more control how your
// texture is created you can use a list of parameters. See the example for more explanation. Use id for
// adding more texture data to existing textures as mipmap levels, or cube map faces.
// Note: if turning mipmapping off and only specifing one texture it should be set to mip level 0, and you'll
// need to turn the min and mag filter settings to linear or nearest (see texture-params).
//
// Example:
// ; simple usage:
// (texture (load-texture "mytexture.png"))
// (build-cube) ; the cube will be texture mapped with the image
//
// ; complex usages:
//
// ; the options list can contain the following keys and values:
// ; id: texture-id-number (for adding images to existing textures - for mipmapping and cubemapping)
// ; type: [texture-2d cube-map-positive-x cube-map-negative-x cube-map-positive-y
// ;         cube-map-negative-y cube-map-positive-z cube-map-negative-z]
// ; generate-mipmaps: exact integer, 0 or 1
// ; mip-level: exact integer
// ; border: exact integer
// ; compress: exact integer, 0 or 1
//
// ; setup an environment cube map
// (define t (load-texture "cube-left.png" (list 'type 'cube-map-positive-x)))
// (load-texture "cube-right.png" (list 'id t 'type 'cube-map-negative-x))
// (load-texture "cube-top.png" (list 'id t 'type 'cube-map-positive-y))
// (load-texture "cube-bottom.png" (list 'id t 'type 'cube-map-negative-y))
// (load-texture "cube-front.png" (list 'id t 'type 'cube-map-positive-z))
// (load-texture "cube-back.png" (list 'id t 'type 'cube-map-negative-z))
// (texture t)
//
// ; setup a mipmapped texture with our own images
// ; you need as many levels as it takes you to get to 1X1 pixels from your
// ; level 0 texture size
// (define t2 (load-texture "m0.png" (list 'generate-mipmaps 0 'mip-level 0)))
// (load-texture "m1.png" (list 'id t2 'generate-mipmaps 0 'mip-level 1))
// (load-texture "m2.png" (list 'id t2 'generate-mipmaps 0 'mip-level 2))
// (load-texture "m3.png" (list 'id t2 'generate-mipmaps 0 'mip-level 3))
// (texture (load-texture "mytexture.png"
//        (list
//          'generate-mipmaps 0  ; turn mipmapping off
//              'border 2)))          ; add a border to the texture
//
// (build-cube) ; the cube will be texture mapped with the image
// EndFunctionDoc

// StartFunctionDoc-pt
// load-texture pngnomedoarquivo-string
// Retorna: void
// Descrição:
// Carrega uma imagem do disco, converte esta a uma textura e retorna
// o número id. O carregamento da textura se dá no cache de memória,
// então repetidamente chamar esta função não vai causar que carregue
// de novo. Use force-load-texture se você está mudando a textura
// enquanto o script estiver rodando. O png pode ser RGB ou RGBA para
// usar transparência alpha.
// Exemplo:
// ; simple usage:
// (texture (load-texture "mytexture.png"))
// (build-cube) ; the cube will be texture mapped with the image
//
// ; complex usages:
//
// ; the options list can contain the following keys and values:
// ; id: texture-id-number (for adding images to existing textures - for mipmapping and cubemapping)
// ; type: [texture-2d cube-map-positive-x cube-map-negative-x cube-map-positive-y
// ;         cube-map-negative-y cube-map-positive-z cube-map-negative-z]
// ; generate-mipmaps : exact integer, 0 or 1
// ; mip-level : exact integer
// ; border : exact integer
//
// ; setup an environment cube map
// (define t (load-texture "cube-left.png" (list 'type 'cube-map-positive-x)))
// (load-texture "cube-right.png" (list 'id t 'type 'cube-map-negative-x))
// (load-texture "cube-top.png" (list 'id t 'type 'cube-map-positive-y))
// (load-texture "cube-bottom.png" (list 'id t 'type 'cube-map-negative-y))
// (load-texture "cube-front.png" (list 'id t 'type 'cube-map-positive-z))
// (load-texture "cube-back.png" (list 'id t 'type 'cube-map-negative-z))
// (texture t)
//
// ; setup a mipmapped texture with our own images
// ; you need as many levels as it takes you to get to 1X1 pixels from your
// ; level 0 texture size
// (define t2 (load-texture "m0.png" (list 'generate-mipmaps 0 'mip-level 0)))
// (load-texture "m1.png" (list 'id t2 'generate-mipmaps 0 'mip-level 1))
// (load-texture "m2.png" (list 'id t2 'generate-mipmaps 0 'mip-level 2))
// (load-texture "m3.png" (list 'id t2 'generate-mipmaps 0 'mip-level 3))
// (texture (load-texture "mytexture.png"
//        (list
//          'generate-mipmaps 0  ; turn mipmapping off
//              'border 2)))          ; add a border to the texture
//
// (build-cube) ; the cube will be texture mapped with the image
// EndFunctionDoc

// StartFunctionDoc-fr
// load-texture png-nom-de-fichier-string optionel-paramètres-de-création-list
// Retour: textureid-nombre
// Description:
// Charge une image à partir du disque, la convertis en texture, et retourne le numéro identifiant.
// Le chargement de texture est enregistré en mémoire, donc les chargements répétitifs ne ferons pas charger de nouveau.
// Le cache peut être éffacé avec clear-texture-cache. Le png peut être RGB ou RGBA pour utiliser la transparence. Pour 
// mieux controler comment la texture est crée, la liste de paramètres peut-être utlisée. Voir l'exemple pour plus d'explications.
// Utiliser l'identifiant pour ajouter plus de données de textures à des textures existantes, comme les niveau mipmap, ou le texturage
// des faces d'un cube. 
// Note: Si le mipmapping est tourné sur off, et qu'une selue texture est spécifiée, elle devrait être attribuée au nivau mip 0,
// et les réglages es filtres min et mag devront petre mis à linear ou nearest (voir texture-params).  
// Exemple:
// ; simple usage:
// (texture (load-texture "mytexture.png"))
// (build-cube) ; the cube will be texture mapped with the image
//
// ; complex usages:
//
// ; the options list can contain the following keys and values:
// ; id: texture-id-number (for adding images to existing textures - for mipmapping and cubemapping)
// ; type: [texture-2d cube-map-positive-x cube-map-negative-x cube-map-positive-y
// ;         cube-map-negative-y cube-map-positive-z cube-map-negative-z]
// ; generate-mipmaps : exact integer, 0 or 1
// ; mip-level : exact integer
// ; border : exact integer
//
// ; setup an environment cube map
// (define t (load-texture "cube-left.png" (list 'type 'cube-map-positive-x)))
// (load-texture "cube-right.png" (list 'id t 'type 'cube-map-negative-x))
// (load-texture "cube-top.png" (list 'id t 'type 'cube-map-positive-y))
// (load-texture "cube-bottom.png" (list 'id t 'type 'cube-map-negative-y))
// (load-texture "cube-front.png" (list 'id t 'type 'cube-map-positive-z))
// (load-texture "cube-back.png" (list 'id t 'type 'cube-map-negative-z))
// (texture t)
//
// ; setup a mipmapped texture with our own images
// ; you need as many levels as it takes you to get to 1X1 pixels from your
// ; level 0 texture size
// (define t2 (load-texture "m0.png" (list 'generate-mipmaps 0 'mip-level 0)))
// (load-texture "m1.png" (list 'id t2 'generate-mipmaps 0 'mip-level 1))
// (load-texture "m2.png" (list 'id t2 'generate-mipmaps 0 'mip-level 2))
// (load-texture "m3.png" (list 'id t2 'generate-mipmaps 0 'mip-level 3))
// (texture (load-texture "mytexture.png"
//        (list
//          'generate-mipmaps 0  ; turn mipmapping off
//              'border 2)))          ; add a border to the texture
//
// (build-cube) ; the cube will be texture mapped with the image
// EndFunctionDoc

Scheme_Object *load_texture(int argc, Scheme_Object **argv)
{
	Scheme_Object *paramvec = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, paramvec);
	MZ_GC_REG();

	if (argc==2) ArgCheck("load-texture", "pl", argc, argv);
	else ArgCheck("load-texture", "p", argc, argv);

	TexturePainter::CreateParams createparams;

	if (argc==2)
	{
		paramvec = scheme_list_to_vector(argv[1]);

		for (int n=0; n<SCHEME_VEC_SIZE(paramvec); n+=2)
		{
			if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(paramvec)[n]) && SCHEME_VEC_SIZE(paramvec)>n+1)
			{
				// get the parameter name
				string param = SymbolName(SCHEME_VEC_ELS(paramvec)[n]);
				if (param=="id")
				{
					if (SCHEME_NUMBERP(SCHEME_VEC_ELS(paramvec)[n+1]) &&
							SCHEME_EXACT_INTEGERP(SCHEME_VEC_ELS(paramvec)[n+1]))
					{
						createparams.ID = IntFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]);
					}
				}
				else if (param=="type")
				{
					if (SCHEME_SYMBOLP(SCHEME_VEC_ELS(paramvec)[n+1]))
					{
						string type=SymbolName(SCHEME_VEC_ELS(paramvec)[n+1]);
						if (type=="texture-2d") createparams.Type = GL_TEXTURE_2D;
						else if (type=="cube-map-positive-x") createparams.Type = GL_TEXTURE_CUBE_MAP_POSITIVE_X;
						else if (type=="cube-map-negative-x") createparams.Type = GL_TEXTURE_CUBE_MAP_NEGATIVE_X;
						else if (type=="cube-map-positive-y") createparams.Type = GL_TEXTURE_CUBE_MAP_POSITIVE_Y;
						else if (type=="cube-map-negative-y") createparams.Type = GL_TEXTURE_CUBE_MAP_NEGATIVE_Y;
						else if (type=="cube-map-positive-z") createparams.Type = GL_TEXTURE_CUBE_MAP_POSITIVE_Z;
						else if (type=="cube-map-negative-z") createparams.Type = GL_TEXTURE_CUBE_MAP_NEGATIVE_Z;
						else Trace::Stream<<"load-texture: unknown parameter for "<<param<<": "<<type<<endl;
					}
				}
				else if (param=="generate-mipmaps")
				{
					if (SCHEME_NUMBERP(SCHEME_VEC_ELS(paramvec)[n+1]) &&
							SCHEME_EXACT_INTEGERP(SCHEME_VEC_ELS(paramvec)[n+1]))
					{
						createparams.GenerateMipmaps = IntFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]);
					}
				}
				else if (param=="mip-level")
				{
					if (SCHEME_NUMBERP(SCHEME_VEC_ELS(paramvec)[n+1]) &&
							SCHEME_EXACT_INTEGERP(SCHEME_VEC_ELS(paramvec)[n+1]))
					{
						createparams.MipLevel = IntFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]);
					}
				}
				else if (param=="border")
				{
					if (SCHEME_NUMBERP(SCHEME_VEC_ELS(paramvec)[n+1]) &&
							SCHEME_EXACT_INTEGERP(SCHEME_VEC_ELS(paramvec)[n+1]))
					{
						createparams.Border = IntFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]);
					}
				}
				else if (param=="compress")
				{
					if (SCHEME_NUMBERP(SCHEME_VEC_ELS(paramvec)[n+1]) &&
							SCHEME_EXACT_INTEGERP(SCHEME_VEC_ELS(paramvec)[n+1]))
					{
						createparams.Compress = IntFromScheme(SCHEME_VEC_ELS(paramvec)[n+1]);
					}
				}
				else Trace::Stream<<"load-texture: unknown parameter "<<param<<endl;
			}
		}
	}

	int ret=Engine::Get()->Renderer()->GetTexturePainter()->LoadTexture(PathFromScheme(argv[0]),createparams);
	MZ_GC_UNREG();
	return scheme_make_integer_value(ret);
}

// StartFunctionDoc-en
// clear-texture-cache
// Returns: void
// Description:
// Clears the texture cache, meaning changed textures on disk are reloaded.
// Example:
// (clear-texture-cache)
// EndFunctionDoc

// StartFunctionDoc-pt
// clear-texture-cache
// Retorna: void
// Descrição:
// Clears the texture cache, meaning changed textures on disk are reloaded.
// Exemplo:
// (clear-texture-cache)
// EndFunctionDoc

// StartFunctionDoc-fr
// clear-texture-cache
// Retour: void
// Description:
// Vide le cache de textures, signifie que les textures modifiées sur le disque sont rechargées.
// Exemple:
// (clear-texture-cache)
// EndFunctionDoc

Scheme_Object *clear_texture_cache(int argc, Scheme_Object **argv)
{
  Engine::Get()->Renderer()->GetTexturePainter()->ClearCache();
    return scheme_void;
}

// StartFunctionDoc-en
// is-resident? textureid-number
// Returns: boolean
// Description:
// Checks if the texture is in high-performance memory.
// Example:
// (define t (load-texture "test.png"))
// (display (is-resident? t))(newline) ; usually texture is not resident until used
// EndFunctionDoc

// StartFunctionDoc-fr
// is-resident? textureid-nombre
// Retour: boolean
// Description:
// Vérifie si la texture est mémoire haute performance.
// Exemple:
// (define t (load-texture "test.png"))
// (display (is-resident? t))(newline) ; En général, la texture n'est pas résidente jusqu'à ce quelle soit utilisée
// EndFunctionDoc

Scheme_Object *is_resident(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("is-resident?", "i", argc, argv);
	bool r = Engine::Get()->Renderer()->GetTexturePainter()->IsResident((int)IntFromScheme(argv[0]));
	MZ_GC_UNREG();
	return r ? scheme_true : scheme_false;
}

// StartFunctionDoc-en
// set-texture-priority textureid-number priority-number
// Returns: void
// Description:
// You can provide hints to the OpenGL implementation to decide
// texture residency by setting the texture’s priority.
// Priority is between 0.0 and 1.0. A low priority tells the
// that this texture object should be left out of resident memory
// whenever space becomes tight. A higher priority (such as 1.0)
// tells the implementation that you want the texture object to
// remain resident if possible, even if the texture seems to be used
// infrequently.
// Bear in mind that texture priority is only a hint. Some OpenGL
// implementations are known to ignore them completely.
// Example:
// (define t (load-texture "test.png"))
// (set-texture-priority t 1.0)
// (display (is-resident? t))(newline)
// EndFunctionDoc

// StartFunctionDoc-fr
// set-texture-priority textureid-nombre priorité-nombre
// Retour: vide
// Description:
// Vous permet de régler les priorités de résidence des textures
// à l'implentation OpenGl.
// Les priorités sont entre 0.0 et 1.0. Une faible priorité annonce
// que la texture doit être laissée en dehors de la mémoire résidente
// quand l'espace se fait faible. Une plus forte priorité (comme 1.0)
// signale à l'implentation que vous voulez que la texture doit restée
// résidente si possible, même si elle est peu utilisée.
// Gardez à l'esprit que cette priorité de texture est juste un signal.
// Certaines implémentations OpenGl sont connues pour les ignorées totalement.
// Exemple:
// (define t (load-texture "test.png"))
// (set-texture-priority t 1.0)
// (display (is-resident? t))(newline)
// EndFunctionDoc

Scheme_Object *set_texture_priority(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("set-texture-priority", "if", argc, argv);
	Engine::Get()->Renderer()->GetTexturePainter()->SetTexturePriority((int)IntFromScheme(argv[0]),
			FloatFromScheme(argv[1]));
	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// texture-width textureid-number
// Returns: width-number
// Description:
// Returns texture width.
// Example:
// (define t (load-texture "test.png"))
// (display (texture-width t))(newline)
// EndFunctionDoc

// StartFunctionDoc-fr
// texture-width textureid-nombre
// Retour: largeur-nombre
// Description:
// Retourne la largeur de la texture.
// Exemple:
// (define t (load-texture "test.png"))
// (display (texture-width t))(newline)
// EndFunctionDoc

Scheme_Object *texture_width(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("texture-width", "i", argc, argv);
	int w = Engine::Get()->Renderer()->GetTexturePainter()->GetTextureWidth(IntFromScheme(argv[0]));
	MZ_GC_UNREG();
	return scheme_make_integer_value(w);
}

// StartFunctionDoc-en
// texture-height textureid-number
// Returns: height-number
// Description:
// Returns texture height.
// Example:
// (define t (load-texture "test.png"))
// (display (texture-height t))(newline)
// EndFunctionDoc

// StartFunctionDoc-fr
// texture-height textureid-nombre
// Retour: hauteur-nombre
// Description:
// Retourne la hauteur de la texture.
// Exemple:
// (define t (load-texture "test.png"))
// (display (texture-height t))(newline)
// EndFunctionDoc

Scheme_Object *texture_height(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	ArgCheck("texture-height", "i", argc, argv);
	int w = Engine::Get()->Renderer()->GetTexturePainter()->GetTextureHeight(IntFromScheme(argv[0]));
	MZ_GC_UNREG();
	return scheme_make_integer_value(w);
}

// StartFunctionDoc-en
// frustum left-number right-number bottom-number top-number
// Returns: void
// Description:
// Sets the camera frustum, and thus the aspect ratio of the frame.
// Example:
// (frustum -1 1 -0.75 0.75) ; default settings
// EndFunctionDoc

// StartFunctionDoc-pt
// frustum número-esquerda número-direita número-baixo número-topo
// Retorna: void
// Descrição:
// Ajusta o frustum da camera, e portanto o quociente de aspecto do
// frame.
// Exemplo:
// (frustum -1 1 -0.75 0.75) ; definições padrão
// EndFunctionDoc

// StartFunctionDoc-fr
// frustum gauche-nombre droite-nombre bas-nombre haut-nombre
// Retour: void
// Description:
// Fixe le frustrum caméra, et par conséquent les proportions de la frame.
// Exemple:
// (frustum -1 1 -0.75 0.75) ; valeurs par défaut
// EndFunctionDoc

Scheme_Object *frustum(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("frustum", "ffff", argc, argv);
  Engine::Get()->GetCamera()->SetFrustum(FloatFromScheme(argv[0]),
                      FloatFromScheme(argv[1]),
                      FloatFromScheme(argv[2]),
                      FloatFromScheme(argv[3]));
  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// clip front-number back-number
// Returns: void
// Description:
// Sets the front & back clipping planes for the camera frustum, and thus the viewing angle.
// Change the front clipping distance to alter the perspective from telephoto to fisheye.
// Example:
// (clip 1 10000) ; default settings
// EndFunctionDoc

// StartFunctionDoc-pt
// clip número-frente número-trás
// Retorna: void
// Descrição:
// Ajusta os planos de clipagem da frente e de trás para o frustum da
// câmera, portanto o ângulo de visão. Mude a distância da frente do
// clip para alterar a perspectiva de telephoto para fisheye.
// Exemplo:
// (clip 1 10000) ; default settings
// EndFunctionDoc

// StartFunctionDoc-fr
// clip devant-nombre arrière-nombre
// Retour: void
// Description:
// Fixe le devant et l'arrière du découpage des faces pour le frustrum caméra, et donc les angles de vue.
// Changer la distance du découpage de devant modifie la perspective de téléobjectif à à oeil de poisson (fisheye). 
// Exemple:
// (clip 1 10000) ; valeurs par défaut
// EndFunctionDoc

Scheme_Object *clip(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("clip", "ff", argc, argv);
  Engine::Get()->GetCamera()->SetClip(FloatFromScheme(argv[0]),
                    FloatFromScheme(argv[1]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// ortho
// Returns: void
// Description:
// Sets orthographic projection - i.e. no perspective.
// Example:
// (ortho)
// EndFunctionDoc

// StartFunctionDoc-pt
// ortho
// Retorna: void
// Descrição:
// Ajusta a projeção ortográfica - p.e. sem perspectiva.
// Exemplo:
// (ortho)
// EndFunctionDoc

// StartFunctionDoc-fr
// ortho
// Retour: void
// Description:
// Passe en projection orthographique - sans perspective
// Exemple:
// (ortho)
// EndFunctionDoc

Scheme_Object *ortho(int argc, Scheme_Object **argv)
{
  Engine::Get()->GetCamera()->SetOrtho(true);
    return scheme_void;
}

// StartFunctionDoc-en
// persp
// Returns: void
// Description:
// Sets perspective projection (the default) after ortho has been set.
// Example:
// (persp)
// EndFunctionDoc

// StartFunctionDoc-pt
// persp
// Retorna: void
// Descrição:
// Ajusta a projeção como perspectiva (o padrão) depois que ortho foi
// acionada.
// Exemplo:
// (persp)
// EndFunctionDoc

// StartFunctionDoc-fr
// persp
// Retour: void
// Description:
// Passe en projection perspective (par defaut) après qu'une orthographique
// ait été activée.
// Exemple:
// (persp)
// EndFunctionDoc

Scheme_Object *persp(int argc, Scheme_Object **argv)
{
  Engine::Get()->GetCamera()->SetOrtho(false);
    return scheme_void;
}

// StartFunctionDoc-en
// set-ortho-zoom amount-number
// Returns: void
// Description:
// Sets the zoom level for the orthographic projection.
// Example:
// (set-ortho-zoom 2)
// EndFunctionDoc

// StartFunctionDoc-pt
// set-ortho-zoom número-quantidade
// Retorna: void
// Descrição:
// Ajusta o nível de zoom para a projeção ortográfica.
// Exemplo:
// (set-ortho-zoom 2)
// EndFunctionDoc

// StartFunctionDoc-fr
// set-ortho-zoom quantitié-nombre
// Retour: void
// Description:
// Fixe le niveau de zoom de la projection orthographique.
// Exemple:
// (set-ortho-zoom 2)
// EndFunctionDoc

Scheme_Object *set_ortho_zoom(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("set-ortho-zoom", "f", argc, argv);
  Engine::Get()->GetCamera()->SetOrthoZoom(FloatFromScheme(argv[0]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// clear-colour colour-vector
// Returns: void
// Description:
// Sets the colour we clear the renderer with, this forms the background colour for the scene.
// Example:
// (clear-colour (vector 1 0 0)) ; RED!!!
// EndFunctionDoc

// StartFunctionDoc-pt
// clear-colour vetor-cor
// Retorna: void
// Descrição:
// Ajusta a cor que vai limpar o renderizador, isto forma a cor do
// fundo da cena.
// Exemplo:
// (clear-colour (vector 1 0 0)) ; RED!!!
// EndFunctionDoc

// StartFunctionDoc-fr
// clear-colour couleur-vecteur
// Retour: void
// Description:
// Attribue la couleur avec laquelle l'afficheur est effacé, ceci forme la couleur de fond de la scène.
// Exemple:
// (clear-colour (vector 1 0 0)) ; ROUGE!!!
// EndFunctionDoc

Scheme_Object *clear_colour(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("clear-colour", "c", argc, argv);
    Engine::Get()->Renderer()->SetBGColour(ColourFromScheme(argv[0]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// clear-frame setting-number
// Returns: void
// Description:
// Sets the frame clearing on or off.
// Example:
// (clear-frame 0)
// (clear-frame 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// clear-frame número-ajuste
// Retorna: void
// Descrição:
// ajusta a limpeza do frame, desligado ou ligado.
// Exemplo:
// (clear-frame 0)
// (clear-frame 1)
// EndFunctionDoc

// StartFunctionDoc-fr
// clear-frame valeur-nombre
// Retour: void
// Description:
// Fixe l'effacement de frame sur on ou off.
// Exemple:
// (clear-frame 0)
// (clear-frame 1)
// EndFunctionDoc

Scheme_Object *clear_frame(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("clear-frame", "i", argc, argv);
  Engine::Get()->Renderer()->SetClearFrame(IntFromScheme(argv[0]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// clear-zbuffer setting-number
// Returns: void
// Description:
// Sets the zbuffer clearing on or off.
// Example:
// (clear-zbuffer 0)
// (clear-zbuffer 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// clear-zbuffer número-ajuste
// Retorna: void
// Descrição:
// Ajusta a limpeza do zbuffer, desligado ou ligado.
// Exemplo:
// (clear-zbuffer 0)
// (clear-zbuffer 1)
// EndFunctionDoc

// StartFunctionDoc-fr
// clear-zbuffer valeur-nombre
// Retour: void
// Description:
// Fixe l'effacement du zbuffer sur on ou off.
// Exemple:
// (clear-zbuffer 0)
// (clear-zbuffer 1)
// EndFunctionDoc

Scheme_Object *clear_zbuffer(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("clear-zbuffer", "i", argc, argv);
  Engine::Get()->Renderer()->SetClearZBuffer(IntFromScheme(argv[0]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// clear-accum setting-number
// Returns: void
// Description:
// Sets the accumulation buffer clearing on or off.
// Example:
// (clear-accum 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// clear-accum número-ajuste
// Retorna: void
// Descrição:
// Ajusta a limpeza do buffer de acumulação, ligado ou desligado
// Exemplo:
// (clear-accum 1)
// EndFunctionDoc

// StartFunctionDoc-fr
// clear-accum valeur-nombre
// Retour: void
// Description:
// Fixe l'effacement du buffer d'accumulation sur on ou off.
// Exemple:
// (clear-accum 1)
// EndFunctionDoc

Scheme_Object *clear_accum(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("clear-accum", "i", argc, argv);
  Engine::Get()->Renderer()->SetClearAccum(IntFromScheme(argv[0]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// build-camera
// Returns: cameraid-number
// Description:
// Adds a new camera/view and returns it's id
// Example:
// (clear)
// (viewport 0 0.5 0.5 0.5)
//
// (define cam2 (build-camera))
// (current-camera cam2)
// (viewport 0.5 0 0.5 1)
//
// (define cam3 (build-camera))
// (current-camera cam3)
// (set-camera (mmul (mtranslate (vector 0 0 -5))
//         (mrotate (vector 0 45 0))))
// (viewport 0 0 0.5 0.5)
//
// ; render a primitive in one view only
// (define t (with-state
//     (translate (vector 3 0 0))
//     (scale 0.3)
//     (colour (vector 1 0 0))
//     (build-torus 1 2 10 10)))
//
// (with-primitive t
//     (hide 1) ; hide in all
//     (camera-hide 0)) ; unhide in current camera
//
//
// (current-camera 0)
//
// (define c (with-state
//         (hint-cull-ccw)
//         (hint-unlit)
//         (hint-wire)
//         (line-width 2)
//         (colour (vector 0.4 0.3 0.2))
//         (wire-colour (vector 0 0 0))
//         (scale 10)
//         (build-cube)))
//
// (define p (with-state
//         (scale 3)
//         (load-primitive "widget.obj")))
//
// (every-frame
//     (with-primitive p
//         (rotate (vector 0 1 0))))
// EndFunctionDoc

// StartFunctionDoc-pt
// build_camera
// Retorna: numeroid-camera
// Descrição:
// adiciona uma nova camera/view retorna sua id
// Exemplo:
// (clear)
// (viewport 0 0.5 0.5 0.5)
//
// (define cam2 (build-camera))
// (current-camera cam2)
// (viewport 0.5 0 0.5 1)
//
// (define cam3 (build-camera))
// (current-camera cam3)
// (set-camera (mmul (mtranslate (vector 0 0 -5))
//         (mrotate (vector 0 45 0))))
// (viewport 0 0 0.5 0.5)
//
// ; render a primitive in one view only
// (define t (with-state
//     (translate (vector 3 0 0))
//     (scale 0.3)
//     (colour (vector 1 0 0))
//     (build-torus 1 2 10 10)))
//
// (with-primitive t
//     (hide 1) ; hide in all
//     (camera-hide 0)) ; unhide in current camera
//
//
// (current-camera 0)
//
// (define c (with-state
//         (hint-cull-ccw)
//         (hint-unlit)
//         (hint-wire)
//         (line-width 2)
//         (colour (vector 0.4 0.3 0.2))
//         (wire-colour (vector 0 0 0))
//         (scale 10)
//         (build-cube)))
//
// (define p (with-state
//         (scale 3)
//         (load-primitive "widget.obj")))
//
// (every-frame
//     (with-primitive p
//         (rotate (vector 0 1 0))))
// EndFunctionDoc

// StartFunctionDoc-fr
// build-camera
// Retour: cameraid-nombre
// Description:
// Ajoute une nouvelle caméra et retourne son identifiant.
// Exemple:
// (clear)
// (viewport 0 0.5 0.5 0.5)
//
// (define cam2 (build-camera))
// (current-camera cam2)
// (viewport 0.5 0 0.5 1)
//
// (define cam3 (build-camera))
// (current-camera cam3)
// (set-camera (mmul (mtranslate (vector 0 0 -5))
//         (mrotate (vector 0 45 0))))
// (viewport 0 0 0.5 0.5)
//
// ; render a primitive in one view only
// (define t (with-state
//     (translate (vector 3 0 0))
//     (scale 0.3)
//     (colour (vector 1 0 0))
//     (build-torus 1 2 10 10)))
//
// (with-primitive t
//     (hide 1) ; hide in all
//     (camera-hide 0)) ; unhide in current camera
//
//
// (current-camera 0)
//
// (define c (with-state
//         (hint-cull-ccw)
//         (hint-unlit)
//         (hint-wire)
//         (line-width 2)
//         (colour (vector 0.4 0.3 0.2))
//         (wire-colour (vector 0 0 0))
//         (scale 10)
//         (build-cube)))
//
// (define p (with-state
//         (scale 3)
//         (load-primitive "widget.obj")))
//
// (every-frame
//     (with-primitive p
//         (rotate (vector 0 1 0))))
// EndFunctionDoc

Scheme_Object *build_camera(int argc, Scheme_Object **argv)
{
  Camera cam;
  return scheme_make_integer_value(Engine::Get()->Renderer()->AddCamera(cam));
}

// StartFunctionDoc-en
// current-camera cameraid-number
// Returns: void
// Description:
// Sets the current camera to use
// Example:
// (clear)
// (viewport 0 0.5 0.5 0.5)
//
// (define cam2 (build-camera))
// (current-camera cam2)
// (viewport 0.5 0 0.5 1)
//
// (define cam3 (build-camera))
// (current-camera cam3)
// (set-camera (mmul (mtranslate (vector 0 0 -5))
//         (mrotate (vector 0 45 0))))
// (viewport 0 0 0.5 0.5)
//
// ; render a primitive in one view only
// (define t (with-state
//     (translate (vector 3 0 0))
//     (scale 0.3)
//     (colour (vector 1 0 0))
//     (build-torus 1 2 10 10)))
//
// (with-primitive t
//     (hide 1) ; hide in all
//     (camera-hide 0)) ; unhide in current camera
//
//
// (current-camera 0)
//
// (define c (with-state
//         (hint-cull-ccw)
//         (hint-unlit)
//         (hint-wire)
//         (line-width 2)
//         (colour (vector 0.4 0.3 0.2))
//         (wire-colour (vector 0 0 0))
//         (scale 10)
//         (build-cube)))
//
// (define p (with-state
//         (scale 3)
//         (load-primitive "widget.obj")))
//
// (every-frame
//     (with-primitive p
//         (rotate (vector 0 1 0))))
// EndFunctionDoc

// StartFunctionDoc-pt
// current_camera numero-cameraid
// Retorna: void
// Descrição:
// Ajusta a camera atual para usar
// Exemplo:
// (clear)
// (viewport 0 0.5 0.5 0.5)
//
// (define cam2 (build-camera))
// (current-camera cam2)
// (viewport 0.5 0 0.5 1)
//
// (define cam3 (build-camera))
// (current-camera cam3)
// (set-camera (mmul (mtranslate (vector 0 0 -5))
//         (mrotate (vector 0 45 0))))
// (viewport 0 0 0.5 0.5)
//
// ; render a primitive in one view only
// (define t (with-state
//     (translate (vector 3 0 0))
//     (scale 0.3)
//     (colour (vector 1 0 0))
//     (build-torus 1 2 10 10)))
//
// (with-primitive t
//     (hide 1) ; hide in all
//     (camera-hide 0)) ; unhide in current camera
//
//
// (current-camera 0)
//
// (define c (with-state
//         (hint-cull-ccw)
//         (hint-unlit)
//         (hint-wire)
//         (line-width 2)
//         (colour (vector 0.4 0.3 0.2))
//         (wire-colour (vector 0 0 0))
//         (scale 10)
//         (build-cube)))
//
// (define p (with-state
//         (scale 3)
//         (load-primitive "widget.obj")))
//
// (every-frame
//     (with-primitive p
//         (rotate (vector 0 1 0))))
// EndFunctionDoc

// StartFunctionDoc-fr
// current-camera cameraid-nombre
// Retour: void
// Description:
// Choisi quelle caméra utiliser.
// Exemple:
// (clear)
// (viewport 0 0.5 0.5 0.5)
//
// (define cam2 (build-camera))
// (current-camera cam2)
// (viewport 0.5 0 0.5 1)
//
// (define cam3 (build-camera))
// (current-camera cam3)
// (set-camera (mmul (mtranslate (vector 0 0 -5))
//         (mrotate (vector 0 45 0))))
// (viewport 0 0 0.5 0.5)
//
// ; render a primitive in one view only
// (define t (with-state
//     (translate (vector 3 0 0))
//     (scale 0.3)
//     (colour (vector 1 0 0))
//     (build-torus 1 2 10 10)))
//
// (with-primitive t
//     (hide 1) ; hide in all
//     (camera-hide 0)) ; unhide in current camera
//
//
// (current-camera 0)
//
// (define c (with-state
//         (hint-cull-ccw)
//         (hint-unlit)
//         (hint-wire)
//         (line-width 2)
//         (colour (vector 0.4 0.3 0.2))
//         (wire-colour (vector 0 0 0))
//         (scale 10)
//         (build-cube)))
//
// (define p (with-state
//         (scale 3)
//         (load-primitive "widget.obj")))
//
// (every-frame
//     (with-primitive p
//         (rotate (vector 0 1 0))))
// EndFunctionDoc

Scheme_Object *current_camera(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("current-camera", "i", argc, argv);
  Engine::Get()->GrabCamera(IntFromScheme(argv[0]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// viewport x-number y-number width-number height-number
// Returns: void
// Description:
// Sets the viewport on the current camera. This is the area of the window the camera renders to,
// where 0,0 is the bottom left and 1,1 is the top right.
// Example:
// (clear)
// (viewport 0 0.5 0.5 0.5)
//
// (define cam2 (build-camera))
// (current-camera cam2)
// (viewport 0.5 0 0.5 1)
//
// (define cam3 (build-camera))
// (current-camera cam3)
// (set-camera (mmul (mtranslate (vector 0 0 -5))
//         (mrotate (vector 0 45 0))))
// (viewport 0 0 0.5 0.5)
//
// ; render a primitive in one view only
// (define t (with-state
//     (translate (vector 3 0 0))
//     (scale 0.3)
//     (colour (vector 1 0 0))
//     (build-torus 1 2 10 10)))
//
// (with-primitive t
//     (hide 1) ; hide in all
//     (camera-hide 0)) ; unhide in current camera
//
//
// (current-camera 0)
//
// (define c (with-state
//         (hint-cull-ccw)
//         (hint-unlit)
//         (hint-wire)
//         (line-width 2)
//         (colour (vector 0.4 0.3 0.2))
//         (wire-colour (vector 0 0 0))
//         (scale 10)
//         (build-cube)))
//
// (define p (with-state
//         (scale 3)
//         (load-primitive "widget.obj")))
//
// (every-frame
//     (with-primitive p
//         (rotate (vector 0 1 0))))
// EndFunctionDoc

// StartFunctionDoc-pt
// viewport numero-x numero-y numero-largura numero-altura
// Retorna: void
// Descrição:
// Ajusta a viewport na câmera atual. Esta é a altura da janela que a
// câmera renderiza, aonde 0,0 é a ponta inferior esquerda e 1,1 é a
// ponta superior direita.
// Exemplo:
// (clear)
// (viewport 0 0.5 0.5 0.5)
//
// (define cam2 (build-camera))
// (current-camera cam2)
// (viewport 0.5 0 0.5 1)
//
// (define cam3 (build-camera))
// (current-camera cam3)
// (set-camera (mmul (mtranslate (vector 0 0 -5))
//         (mrotate (vector 0 45 0))))
// (viewport 0 0 0.5 0.5)
//
// ; render a primitive in one view only
// (define t (with-state
//     (translate (vector 3 0 0))
//     (scale 0.3)
//     (colour (vector 1 0 0))
//     (build-torus 1 2 10 10)))
//
// (with-primitive t
//     (hide 1) ; hide in all
//     (camera-hide 0)) ; unhide in current camera
//
//
// (current-camera 0)
//
// (define c (with-state
//         (hint-cull-ccw)
//         (hint-unlit)
//         (hint-wire)
//         (line-width 2)
//         (colour (vector 0.4 0.3 0.2))
//         (wire-colour (vector 0 0 0))
//         (scale 10)
//         (build-cube)))
//
// (define p (with-state
//         (scale 3)
//         (load-primitive "widget.obj")))
//
// (every-frame
//     (with-primitive p
//         (rotate (vector 0 1 0))))
// EndFunctionDoc

// StartFunctionDoc-fr
// viewport x-nombre y-nombre largeur-nombre hauteur-nombre
// Retour: void
// Description:
// Fixe la taille d'affichage de la caméra courante. Ceci est l'aire de la fenêtre que la caméra va occuper.
// 0,0 est le coin bas gauche et 1,1 est le coin haut droite. 
// Exemple:
// (clear)
// (viewport 0 0.5 0.5 0.5)
//
// (define cam2 (build-camera))
// (current-camera cam2)
// (viewport 0.5 0 0.5 1)
//
// (define cam3 (build-camera))
// (current-camera cam3)
// (set-camera (mmul (mtranslate (vector 0 0 -5))
//         (mrotate (vector 0 45 0))))
// (viewport 0 0 0.5 0.5)
//
// ; render a primitive in one view only
// (define t (with-state
//     (translate (vector 3 0 0))
//     (scale 0.3)
//     (colour (vector 1 0 0))
//     (build-torus 1 2 10 10)))
//
// (with-primitive t
//     (hide 1) ; hide in all
//     (camera-hide 0)) ; unhide in current camera
//
//
// (current-camera 0)
//
// (define c (with-state
//         (hint-cull-ccw)
//         (hint-unlit)
//         (hint-wire)
//         (line-width 2)
//         (colour (vector 0.4 0.3 0.2))
//         (wire-colour (vector 0 0 0))
//         (scale 10)
//         (build-cube)))
//
// (define p (with-state
//         (scale 3)
//         (load-primitive "widget.obj")))
//
// (every-frame
//     (with-primitive p
//         (rotate (vector 0 1 0))))
// EndFunctionDoc

Scheme_Object *viewport(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("viewport", "ffff", argc, argv);
  Engine::Get()->GetCamera()->SetViewport(
    FloatFromScheme(argv[0]),
    FloatFromScheme(argv[1]),
    FloatFromScheme(argv[2]),
    FloatFromScheme(argv[3]));
  MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// get-camera
// Returns: matrix-vector
// Description:
// Gets the current camera transform matrix. This is the low level function,
// use get-camera-transform instead.
// Example:
// (get-camera)
// EndFunctionDoc

// StartFunctionDoc-pt
// get-camera
// Retorna: vetor-matriz
// Descrição:
// Pega a transformação da camera. Esta é a função de baixo nível, use
// get-camera-transform ao invés.
// Exemplo:
// (get-camera)
// EndFunctionDoc

// StartFunctionDoc-fr
// get-camera
// Retour: matrix-vector
// Description:
// Récupère la matrix de transformation de la caméra courante.
// Ceci est une fonction bas-niveau, utiliser plutôt la fonction get-camera-transform. 
// Exemple:
// (get-camera)
// EndFunctionDoc

Scheme_Object *get_camera(int argc, Scheme_Object **argv)
{
  return FloatsToScheme(Engine::Get()->GetCamera()->GetMatrix()->inverse().arr(),16);
}

// StartFunctionDoc-en
// get-locked-matrix
// Returns: matrix-vector
// Description:
// Gets the current camera lock transform matrix. Takes the lag into account
// Example:
// (get-locked-matrix)
// EndFunctionDoc

// StartFunctionDoc-pt
// get-locked-matrix
// Retorna: vetor-matriz
// Descrição:
// Pega a matriz de tranformação da câmera travada. Leva em
// consideração o atraso.
// Exemplo:
// (get-locked-matrix)
// EndFunctionDoc

// StartFunctionDoc-fr
// get-locked-matrix
// Retour: matrix-vector
// Description:
// Récupère la matrix de transformation de la caméra courante.
// Prend en compte le niveau de lag.
// Exemple:
// (get-locked-matrix)
// EndFunctionDoc

Scheme_Object *get_locked_matrix(int argc, Scheme_Object **argv)
{
  return FloatsToScheme(Engine::Get()->GetCamera()->GetLockedMatrix()->inverse().arr(),16);
}

// StartFunctionDoc-en
// set-camera
// Returns: void
// Description:
// Sets the camera transform matrix. This is the low level interface used by set-camera-transform,
// which you should generally use instead.
// Example:
// (set-camera (mtranslate (vector 0 0 -10)))
// EndFunctionDoc

// StartFunctionDoc-pt
// set-camera
// Retorna: void
// Descrição:
// Ajusta a matriz de transformação da câmera. Esta é a interface de
// baixo nível usada por set-camera-transform, a qual você devia usar
// geralmente ao invés.
// Exemplo:
// (set-camera (mtranslate (vector 0 0 -10)))
// EndFunctionDoc

// StartFunctionDoc-fr
// set-camera
// Retour: void
// Description:
// Fixe la matrix de transformation de la caméra. 
// Ceci est l'interface bas-niveau utilisée par set-camera-transform,
// qui devrait être utilisée à la place.
// Exemple:
// (set-camera (mtranslate (vector 0 0 -10)))
// EndFunctionDoc

Scheme_Object *set_camera(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("set-camera", "m", argc, argv);
  dMatrix m;
  FloatsFromScheme(argv[0],m.arr(),16);
  Engine::Get()->GetCamera()->SetMatrix(m);
  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// get-projection-transform
// Returns: projection-matrix
// Description:
// Gets the current projection matrix.
// Example:
// (get-projection-transform)
// EndFunctionDoc

// StartFunctionDoc-pt
// get-projection-transform
// Retorna: matriz-de-projeção
// Descrição:
// Pega a matriz de projeção atual.
// Exemplo:
// (get-projection-transform)
// EndFunctionDoc

// StartFunctionDoc-fr
// get-projection-transform
// Retour: projection-matrice
// Description:
// Récupère la matrice de projection.
// Exemple:
// (get-projection-transform)
// EndFunctionDoc

Scheme_Object *get_projection_transform(int argc, Scheme_Object **argv)
{
  return FloatsToScheme(Engine::Get()->GetCamera()->GetProjection().arr(),16);
}

// StartFunctionDoc-en
// set-projection-transform matrix-vector
// Returns: void
// Description:
// Sets the projection matrix directly.
// Example:
// (set-projection-transform (vector 1 0 0 0 0 4/3 0 0 0 0 -1 -1 0 0 -2 -0))
// EndFunctionDoc

// StartFunctionDoc-pt
// set-projection-transform vetor-matriz
// Retorna: void
// Descrição:
// Ajusta a matriz de projeção diretamente.
// Exemplo:
// (set-projection-transform (vector 1 0 0 0 0 4/3 0 0 0 0 -1 -1 0 0 -2 -0))
// EndFunctionDoc

// StartFunctionDoc-fr
// set-projection-transform matrice-vecteur
// Retour: void
// Description:
// Fixe directement la matrice de projection.
// Exemple:
// (set-projection-transform (vector 1 0 0 0 0 4/3 0 0 0 0 -1 -1 0 0 -2 -0))
// EndFunctionDoc

Scheme_Object *set_projection_transform(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("set-projection-transform", "m", argc, argv);
  dMatrix m;
  FloatsFromScheme(argv[0],m.arr(),16);
  Engine::Get()->GetCamera()->SetProjection(m);
  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// get-screen-size
// Returns: size-vector
// Description:
// Returns a vector containing the current width and height of the window.
// Example:
// (get-screen-size)
// EndFunctionDoc

// StartFunctionDoc-pt
// get-screen-size
// Retorna: vetor-tamanho
// Descrição:
// Retorna um vetor contendo a atual largura e altura da janela
// Exemplo:
// (get-screen-size)
// EndFunctionDoc

// StartFunctionDoc-fr
// get-screen-size
// Retour: taille-vecteur
// Description:
// Retourne un vecteur contenant la largeur et hauteur actuelle de la fenêtre.
// Exemple:
// (get-screen-size)
// EndFunctionDoc

Scheme_Object *get_screen_size(int argc, Scheme_Object **argv)
{
  float res[2];
  int x=0,y=0;
  Engine::Get()->Renderer()->GetResolution(x,y);
  res[0]=x; res[1]=y;
  return FloatsToScheme(res,2);
}

// StartFunctionDoc-en
// set-screen-size size-vector
// Returns: void
// Description:
// Sets the window width and height.
// Example:
// (set-screen-size (vector 10 10)) ; small window time :)
// (set-screen-size (vector 720 576)) ; and back again!
// EndFunctionDoc

// StartFunctionDoc-pt
// set-screen-size vetor-tamanho
// Retorna: void
// Descrição:
// Ajusta a altura e largura da janela.
// Exemplo:
// (set-screen-size (vector 10 10)) ; small window time :)
// (set-screen-size (vector 720 576)) ; and back again!
// EndFunctionDoc

// StartFunctionDoc-fr
// set-screen-size taille-vecteur
// Retour: void
// Description:
// Fixe la largeur et hauteur de la fenêtre.
// Exemple:
// (set-screen-size (vector 10 10)) ; small window time :)
// (set-screen-size (vector 720 576)) ; and back again!
// EndFunctionDoc

Scheme_Object *set_screen_size(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  if (!SCHEME_VECTORP(argv[0])) scheme_wrong_type("set-screen-size", "vector", 0, argc, argv);
  if (SCHEME_VEC_SIZE(argv[0])!=2) scheme_wrong_type("set-screen-size", "vector size 2", 0, argc, argv);
  float v[2];
  FloatsFromScheme(argv[0],v,2);
  // hmmm, seems a bit wrong, but hey...
  glutReshapeWindow((int)v[0],(int)v[1]);
  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// select screenxpos-number screenypos-number pixelssize-number
// Returns: primitiveid-number
// Description:
// Looks in the region specified and returns the id of the closest primitive to the camera rendered
// there, or 0 if none exist.
// Example:
// (display (select 10 10 2))(newline)
// EndFunctionDoc

// StartFunctionDoc-pt
// select número-janelaposX número-janelaposY número-tamanho-pixel
// Retorna: número-id-primitiva
// Descrição:
// Olha na região específicada e retorna a id da primitiva mais
// próxima à renderização da câmera lá, ou 0 se não existente.
// Exemplo:
// (display (select 10 10 2))(newline)
// EndFunctionDoc

// StartFunctionDoc-fr
// select position-écran-x-nombre position-écran-y-nombre taille-pixel-nombre
// Retour: primitiveid-nombre
// Description:
// Observe une région spécifiée et retourne l'identifiant de la primitive la plus proche
// dans le rendu de la caméra, ou 0 si aucun n'est trouvé.
// Exemple:
// (display (select 10 10 2))(newline)
// EndFunctionDoc

Scheme_Object *select(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("select", "iii", argc, argv);
  int x=IntFromScheme(argv[0]);
  int y=IntFromScheme(argv[1]);
  int s=IntFromScheme(argv[2]);
  MZ_GC_UNREG();
  return scheme_make_integer_value(Engine::Get()->Renderer()->Select(
        Engine::Get()->GrabbedCamera(),x,y,s));
}

// StartFunctionDoc-en
// select-all screenxpos-number screenypos-number pixelssize-number
// Returns: list of primitiveid-numbers
// Description:
// Looks in the region specified and returns all ids rendered there in a
// list, or '() if none exist.
// Example:
// (display (select-all 10 10 2))(newline)
// EndFunctionDoc

// StartFunctionDoc-pt
// select-all numero-telax numero-telay numero-tamanhopixel
// Retorna: lista de numeros-primitivaid
// Descrição:
// Procura na região especificada e retorna todas ids renderizadas lá
// em uma lista, ou '() se não existe nenhuma.
// Exemplo:
// (display (select-all 10 10 2))(newline)
// EndFunctionDoc

// StartFunctionDoc-fr
// select-all position-écran-x-nombre position-écran-y-nombre taille-pixel-nombre
// Retour: liste de primitiveid-nombre
// Description:
// Observe une région spécifiée et retourne tous les identifiants affichés à l'interieur, dans une liste,
// ou '() si aucun n'est trouvé.
// Exemple:
// (display (select-all 10 10 2))(newline)
// EndFunctionDoc

Scheme_Object *select_all(int argc, Scheme_Object **argv)
{
  Scheme_Object *vec = NULL;
  Scheme_Object *tmp = NULL;
  Scheme_Object *ret = NULL;
  MZ_GC_DECL_REG(2);
  MZ_GC_VAR_IN_REG(0, vec);
  MZ_GC_VAR_IN_REG(1, argv);
  MZ_GC_REG();

  ArgCheck("select-all", "iii", argc, argv);
  int x=IntFromScheme(argv[0]);
  int y=IntFromScheme(argv[1]);
  int s=IntFromScheme(argv[2]);

  unsigned int *IDs;
  int num = Engine::Get()->Renderer()->SelectAll(
          Engine::Get()->GrabbedCamera(),
          x, y, s, &IDs);

  vec = scheme_make_vector(num, scheme_void);

  for (int i = 0; i < num; i++)
  {
    tmp = scheme_make_integer_value(IDs[i]);
    SCHEME_VEC_ELS(vec)[i] = tmp;
  }

    ret = scheme_vector_to_list(vec);
  MZ_GC_UNREG();
  return ret;
}

// StartFunctionDoc-en
// desiredfps fps-number
// Returns: void
// Description:
// Throttles the renderer so as to not take 100% cpu. This gives an upper limit on the fps rate, which
// doesn't quite match the given number, but I'm working on it...
// Example:
// (desiredfps 100000) ; makes fluxus render as fast as it can, and take 100% cpu.
// EndFunctionDoc

// StartFunctionDoc-pt
// desiredfps número-fps
// Retorna: void
// Descrição:
// Desacelera o renderizador de forma a não pegar 100% de cpu. Isto dá
// um limite acima na taxa de fps, o que não completamente bate o
// número dado, mas nós estamos trabalhando nisto...
// Exemplo:
// (desiredfps 100000) ; faz fluxus renderizar tão rápido quanto pode
//                     ; e levar 100% de cpu.
// EndFunctionDoc

// StartFunctionDoc-fr
// desiredfps fps-nombre
// Retour: void
// Description:
// Régule l'afficheur pour qu'il ne prenne pas 100% de cpu.
// Ceci donne une limit supérieur sur le taux de fps qui ne correspond pas encore aavec le nombre donné,
// mais le travail est en cours. 
// Exemple:
// (desiredfps 100000) ; makes fluxus render as fast as it can, and take 100% cpu.
// EndFunctionDoc

Scheme_Object *desiredfps(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("desiredfps", "f", argc, argv);
  Engine::Get()->Renderer()->SetDesiredFPS(scheme_real_to_double(argv[0]));
  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// draw-buffer buffer_name
// Returns: void
// Description:
// Select which buffer to draw in
// for stereo mode you'd do 'back-right and 'back-left
// Example:
// (draw-buffer 'back)
// EndFunctionDoc

// StartFunctionDoc-pt
// draw-buffer nome-buffer
// Retorna: void
// Descrição:
// Seleciona qual buffer para desenhar, se em modo estéreo você iria
// fazer 'back-right e 'back-left
// Exemplo:
// (draw-buffer 'back)
// EndFunctionDoc

// StartFunctionDoc-fr
// draw-buffer buffer_nom
// Retour: void
// Description:
// Choisi quel buffer dessiner.
// Pour le mode stéréo, celà devrait être 'back-right et 'back-left
// Exemple:
// (draw-buffer 'back)
// EndFunctionDoc

Scheme_Object *draw_buffer(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("draw-buffer", "S", argc, argv);

  if(IsSymbol(argv[0], "back"))
    Engine::Get()->Renderer()->DrawBuffer(GL_BACK);
  else if(IsSymbol(argv[0],"back-right"))
    Engine::Get()->Renderer()->DrawBuffer(GL_BACK_RIGHT);
  else if(IsSymbol(argv[0],"back-left"))
    Engine::Get()->Renderer()->DrawBuffer(GL_BACK_LEFT);
  else if(IsSymbol(argv[0], "front"))
    Engine::Get()->Renderer()->DrawBuffer(GL_FRONT);
  else if(IsSymbol(argv[0], "front-right"))
    Engine::Get()->Renderer()->DrawBuffer(GL_FRONT_RIGHT);
  else if(IsSymbol(argv[0], "front-left"))
    Engine::Get()->Renderer()->DrawBuffer(GL_FRONT_LEFT);
  else if(IsSymbol(argv[0], "right"))
    Engine::Get()->Renderer()->DrawBuffer(GL_RIGHT);
  else if(IsSymbol(argv[0], "left"))
    Engine::Get()->Renderer()->DrawBuffer(GL_LEFT);
  else if(IsSymbol(argv[0], "front-and-back"))
    Engine::Get()->Renderer()->DrawBuffer(GL_FRONT_AND_BACK);
  else if(IsSymbol(argv[0], "none"))
    Engine::Get()->Renderer()->DrawBuffer(GL_NONE);
  else {
    //XXX should indicate an error
  }

  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// read-buffer buffer_name
// Returns: void
// Description:
// Select which buffer to read from
// Example:
// (read-buffer 'back)
// EndFunctionDoc

// StartFunctionDoc-pt
// read-buffer nome-buffer
// Retorna: void
// Descrição:
// Seleciona qual buffer para ler.
// Exemplo:
// (read-buffer 'back)
// EndFunctionDoc

// StartFunctionDoc-fr
// read-buffer buffer_nom
// Retour: void
// Description:
// Choisi un buffer duquel lire.
// Exemple:
// (read-buffer 'back)
// EndFunctionDoc

Scheme_Object *read_buffer(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("read-buffer", "S", argc, argv);

  if(IsSymbol(argv[0], "back"))
    Engine::Get()->Renderer()->ReadBuffer(GL_BACK);
  else if(IsSymbol(argv[0], "back-right"))
    Engine::Get()->Renderer()->ReadBuffer(GL_BACK_RIGHT);
  else if(IsSymbol(argv[0], "back-left"))
    Engine::Get()->Renderer()->ReadBuffer(GL_BACK_LEFT);
  else if(IsSymbol(argv[0], "front"))
    Engine::Get()->Renderer()->ReadBuffer(GL_FRONT);
  else if(IsSymbol(argv[0], "front-right"))
    Engine::Get()->Renderer()->ReadBuffer(GL_FRONT_RIGHT);
  else if(IsSymbol(argv[0], "front-left"))
    Engine::Get()->Renderer()->ReadBuffer(GL_FRONT_LEFT);
  else if(IsSymbol(argv[0], "right"))
    Engine::Get()->Renderer()->ReadBuffer(GL_RIGHT);
  else if(IsSymbol(argv[0], "left"))
    Engine::Get()->Renderer()->ReadBuffer(GL_LEFT);
  else if(IsSymbol(argv[0], "front-and-back"))
    Engine::Get()->Renderer()->ReadBuffer(GL_FRONT_AND_BACK);
  else if(IsSymbol(argv[0], "none"))
    Engine::Get()->Renderer()->ReadBuffer(GL_NONE);
  else {
    //XXX should indicate an error
  }

  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// set-stereo-mode mode
// Returns: bool
// Description:
// select which stereo mode to use
// currently only 'crystal-eyes and 'no-stereo are supported
// the return indicates if the operation was successful or not
// 'crystal-eyes will return false if you don't have a stereo window
// Example:
// (set-stereo-mode 'crystal-eyes)
// EndFunctionDoc

// StartFunctionDoc-pt
// set-stereo-mode modo
// Retorna: bool
// Descrição:
// seleciona qual modo estéreo a usar, atualmente somente
// 'cristal-eyes e 'no-stereo são suportados o retorno indica se a
// operação foi bem sucedida ou não 'crystal-eyes vai retornar falso
// se você não tem uma janela estéreo.
// Exemplo:
// (set-stereo-mode 'crystal-eyes)
// EndFunctionDoc

// StartFunctionDoc-fr
// set-stereo-mode mode
// Retour: bool
// Description:
// Choisi quel mode séréo utiliser.
// Pour l'instant seul 'crystal-eyes et 'no-stereo sont supportés. 
// Le retour indique si l'opération a réussie ou pas.
// 'crystal-eyes retourne false si ce n'est pas une fenêtre stéréo. 
// Exemple:
// (set-stereo-mode 'crystal-eyes)
// EndFunctionDoc

Scheme_Object *set_stereo_mode(int argc, Scheme_Object **argv)
{
  bool success;
  DECL_ARGV();
  ArgCheck("set-stereo-mode", "S", argc, argv);
  if(IsSymbol(argv[0], "crystal-eyes"))
    success = Engine::Get()->Renderer()->SetStereoMode(Renderer::crystalEyes);
  else if(IsSymbol(argv[0], "colour"))
    success = Engine::Get()->Renderer()->SetStereoMode(Renderer::colourStereo);
  else if(IsSymbol(argv[0], "no-stereo"))
    success = Engine::Get()->Renderer()->SetStereoMode(Renderer::noStereo);
  else {
    Engine::Get()->Renderer()->SetStereoMode(Renderer::noStereo);
    success = false;
  }
  MZ_GC_UNREG();

  if(success)
    return scheme_true;
  else
    return scheme_false;
}

Scheme_Object *get_stereo_mode(int argc, Scheme_Object **argv)
{
  Renderer::stereo_mode_t mode;
  DECL_ARGV();
  mode = Engine::Get()->Renderer()->GetStereoMode();
  MZ_GC_UNREG();

  switch(mode){
    case Renderer::noStereo:
      return scheme_intern_symbol("no-stereo");
    case Renderer::crystalEyes:
      return scheme_intern_symbol("crystal-eyes");
    case Renderer::colourStereo:
      return scheme_intern_symbol("colour");
    default:
      return scheme_intern_symbol("no_stereo");
  }
}

// StartFunctionDoc-en
// set-colour-mask vector
// Returns: void
// Description:
// sets the colour mask
// give it a quat of booleans which correspond to the
// red, green, blue and alpha channels respectively
// after this operation you'll only see those colour which you
// set to true (this is useful for stereo with red-blue glasses)
// Example:
// (set-colour-mask #(#t #f #f #t))
// EndFunctionDoc

// StartFunctionDoc-pt
// set-colour-mask vetor
// Retorna: void
// Descrição:
// Ajusta a máscara de cor dando a esta um quatérnio de booleanos que
// correspondem aos canais vermelho, verde, azul e alpha
// respectivamente depois desta operação você vai ver apenas aquelas
// cores que você ajustar como verdadeiras (isto é útil apenas para
// estéreo com óculos azul-vermelhos)
// Exemplo:
// (set-colour-mask #(#t #f #f #t))
// EndFunctionDoc

// StartFunctionDoc-fr
// set-colour-mask vecteur
// Retour: void
// Description:
// Attribue un masque de couleur en passant un vecteur de 4 booléens
// qui correspondent respectivement aux canaux rouge, vert, bleu et alpha.
// Après cette opération, seules les couleurs mise à true seront visibles.
// Ceci est utile pour la stéréo avec les lunettes rouge et blue.
// Exemple:
// (set-colour-mask #(#t #f #f #t))
// EndFunctionDoc

Scheme_Object *set_colour_mask(int argc, Scheme_Object **argv)
{
  bool rgba[4];
  DECL_ARGV();
  ArgCheck("set-colour-mask", "q", argc, argv);
  for(unsigned int n = 0; n < 4; n++){
    if(!SCHEME_BOOLP(SCHEME_VEC_ELS(argv[0])[n]))
      scheme_wrong_type("set-colour-mask", "quat of booleans", 0, argc, argv);
    rgba[n] = SCHEME_TRUEP(SCHEME_VEC_ELS(argv[0])[n]) ? true : false;
  }
  Engine::Get()->Renderer()->SetColourMask(rgba[0],rgba[1],rgba[2],rgba[3]);
  MZ_GC_UNREG();

  return scheme_void;
}

// StartFunctionDoc-en
// shadow-light number-setting
// Returns: void
// Description:
// Sets the light to use for generating shadows, set to 0 to disable shadow
// rendering.
// Example:
// (shadow-light 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// shadow-light número-ajuste
// Retorna: void
// Descrição:
// Ajusta a luz para usar na geração de sombras, ajuste para 0 para
// desativar renderização de sombras.
// Exemplo:
// (shadow-light 1)
// EndFunctionDoc

// StartFunctionDoc-fr
// shadow-light nombre-valeur
// Retour: void
// Description:
// Choisi la lumière à utiliser pour générer les ombress
// Utiliser 0 pour désactiver le rendu des ombres.
// Exemple:
// (shadow-light 1)
// EndFunctionDoc

Scheme_Object *shadow_light(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("shadow-light", "i", argc, argv);
  Engine::Get()->Renderer()->ShadowLight(IntFromScheme(argv[0]));
  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// shadow-length number-setting
// Returns: void
// Description:
// Sets the length of the shadow volume
// rendering.
// Example:
// (shadow-length 10)
// EndFunctionDoc

// StartFunctionDoc-pt
// shadow-length número-ajuste
// Retorna: void
// Descrição:
// Ajusta o alcance do volume da renderização da sombra.
// Exemplo:
// (shadow-length 10)
// EndFunctionDoc

// StartFunctionDoc-fr
// shadow-length nombre-valeur
// Retour: void
// Description:
// Fixe la longueur du volume d'ombres.
// Exemple:
// (shadow-length 10)
// EndFunctionDoc

Scheme_Object *shadow_length(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("shadow-length", "f", argc, argv);
  Engine::Get()->Renderer()->ShadowLength(FloatFromScheme(argv[0]));
  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// shadow-debug number-setting
// Returns: void
// Description:
// Turns on debug rendering of the shadow volume
// rendering.
// Example:
// (shadow-debug 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// shadow-debug número-ajuste
// Retorna: void
// Descrição:
// Liga debug na renderização do volume da sombra.
// Exemplo:
// (shadow-debug 1)
// EndFunctionDoc

// StartFunctionDoc-fr
// shadow-debug nombre-valeur
// Retour: void
// Description:
// Active le débuguage du rendu de volume d'ombres.
// Exemple:
// (shadow-debug 1)
// EndFunctionDoc

Scheme_Object *shadow_debug(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("shadow-debug", "i", argc, argv);
  Engine::Get()->Renderer()->DebugShadows(IntFromScheme(argv[0]));
  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// accum mode-symbol value-number
// Returns: void
// Description:
// Controls the accumulation buffer (just calls glAccum under the hood).
// Possible symbols are: accum load return add mult
// Example:
// (accum 'add 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// accum simbolo-modo número-valor
// Retorna: void
// Descrição:
// Controla o buffer de acumulação (somente chama glAccum embaixo do
// tapete).
// Símbolos possíveis são: accum load return add mult
// Exemplo:
// (accum 'add 1)
// EndFunctionDoc

// StartFunctionDoc-fr
// accum mode-symbol valeur-nombre
// Retour: void
// Description:
// Controle le buffer d'accumulation (simple appel à glAccum sous le capot).
// Les symboles possibles sont: accum load return add mult
// Exemple:
// (accum 'add 1)
// EndFunctionDoc

Scheme_Object *accum(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("accum", "Sf", argc, argv);

  if(SAME_OBJ(argv[0], scheme_intern_symbol("accum")))
    Engine::Get()->Renderer()->Accum(GL_ACCUM,FloatFromScheme(argv[1]));
  else if(SAME_OBJ(argv[0], scheme_intern_symbol("load")))
    Engine::Get()->Renderer()->Accum(GL_LOAD,FloatFromScheme(argv[1]));
  else if(SAME_OBJ(argv[0], scheme_intern_symbol("return")))
    Engine::Get()->Renderer()->Accum(GL_RETURN,FloatFromScheme(argv[1]));
  else if(SAME_OBJ(argv[0], scheme_intern_symbol("add")))
    Engine::Get()->Renderer()->Accum(GL_ADD,FloatFromScheme(argv[1]));
  else if(SAME_OBJ(argv[0], scheme_intern_symbol("mult")))
    Engine::Get()->Renderer()->Accum(GL_MULT,FloatFromScheme(argv[1]));
  else {
    //XXX should indicate an error
  }

  MZ_GC_UNREG();
  return scheme_void;
}


// StartFunctionDoc-en
// print-info
// Returns: void
// Description:
// Prints out a load of renderer information
// Example:
// (print-info)
// EndFunctionDoc

// StartFunctionDoc-pt
// print-info
// Retorna: void
// Descrição:
// Imprime uma porção de informaçao do renderizador.
// Exemplo:
// (print-info)
// EndFunctionDoc

// StartFunctionDoc-fr
// print-info
// Retour: void
// Description:
// Affiche une quantité d'information sur l'afficheur
// Exemple:
// (print-info)
// EndFunctionDoc

Scheme_Object *print_info(int argc, Scheme_Object **argv)
{
  Engine::Get()->Renderer()->PrintInfo();
  return scheme_void;
}

// StartFunctionDoc-en
// set-cursor image-name-symbol
// Returns: void
// Description:
// Changes the mouse cursor.
// Cursor image name symbols can consist of: 'right-arrow, 'left-arrow, 'info,
// 'destroy, 'help, 'cycle, 'spray, 'wait, 'text, 'crosshair, 'up-down,
// 'left-right, 'top-side, 'bottom-side, 'left-side, 'right-side,
// 'top-left-corner, 'top-right-corner, 'bottom-right-corner,
// 'bottom-left-corner, 'full-crosshair, 'none, 'inherit
// The default cursor image symbol when the window is created is 'inherit.
// Example:
// (set-cursor 'crosshair)
// EndFunctionDoc

// StartFunctionDoc-pt
// set-cursor simbolo-nome-imagem
// Retorna: void
// Descrição:
// Troca o cursor do mouse.
// Simbolos da imagem do cursor consistem em: 'right-arrow, 'left-arrow, 'info,
// 'destroy, 'help, 'cycle, 'spray, 'wait, 'text, 'crosshair, 'up-down,
// 'left-right, 'top-side, 'bottom-side, 'left-side, 'right-side,
// 'top-left-corner, 'top-right-corner, 'bottom-right-corner,
// 'bottom-left-corner, 'full-crosshair, 'none, 'inherit
// O cursor padrão quando a janela é criada é 'inherit.
// Exemplo:
// (set-cursor 'crosshair)
// EndFunctionDoc

// StartFunctionDoc-fr
// set-cursor image-nom-symbol
// Retour: void
// Description:
// Modifie le curseur de la souris.
// Les images de curseur sont des noms de symbol qui peuvent être:
// 'right-arrow, 'left-arrow, 'info, 'destroy, 'help,
// 'cycle, 'spray, 'wait, 'text, 'crosshair, 'up-down,
// 'left-right, 'top-side, 'bottom-side, 'left-side, 'right-side,
// 'top-left-corner, 'top-right-corner, 'bottom-right-corner,
// 'bottom-left-corner, 'full-crosshair, 'none, 'inherit
// Le curseur par défaut quand la fenêtre est créée est 'inherit.
// Exemple:
// (set-cursor 'crosshair)
// EndFunctionDoc

Scheme_Object *set_cursor(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  ArgCheck("set-cursor", "S", argc, argv);
  string cursym = SymbolName(argv[0]);
  int cursor = GLUT_CURSOR_INHERIT;

  string symbols[] = { "right-arrow", "left-arrow", "info", "destroy",
            "help", "cycle", "spray",
            "wait", "text", "crosshair", "up-down",
            "left-right", "top-side", "bottom-side",
            "left-side", "right-side", "top-left-corner",
            "top-right-corner", "bottom-right-corner",
            "bottom-left-corner", "full-crosshair",
            "none", "inherit" };
  int values[] = { GLUT_CURSOR_RIGHT_ARROW, GLUT_CURSOR_LEFT_ARROW,
           GLUT_CURSOR_INFO, GLUT_CURSOR_DESTROY, GLUT_CURSOR_HELP,
           GLUT_CURSOR_CYCLE, GLUT_CURSOR_SPRAY, GLUT_CURSOR_WAIT,
           GLUT_CURSOR_TEXT, GLUT_CURSOR_CROSSHAIR,
           GLUT_CURSOR_UP_DOWN, GLUT_CURSOR_LEFT_RIGHT,
           GLUT_CURSOR_TOP_SIDE, GLUT_CURSOR_BOTTOM_SIDE,
           GLUT_CURSOR_LEFT_SIDE, GLUT_CURSOR_RIGHT_SIDE,
           GLUT_CURSOR_TOP_LEFT_CORNER, GLUT_CURSOR_TOP_RIGHT_CORNER,
           GLUT_CURSOR_BOTTOM_RIGHT_CORNER, GLUT_CURSOR_BOTTOM_LEFT_CORNER,
           GLUT_CURSOR_FULL_CROSSHAIR, GLUT_CURSOR_NONE, GLUT_CURSOR_INHERIT };
  bool found = false;

  for (unsigned i = 0; i < sizeof(values)/sizeof(int); i++)
  {
    if (cursym == symbols[i])
    {
      cursor = values[i];
      found = true;
      break;
    }
  }

  if (found)
    glutSetCursor(cursor);
  else
    Trace::Stream<<"cursor image name is not recognised: "<<cursym<<endl;

  MZ_GC_UNREG();
  return scheme_void;
}

// StartFunctionDoc-en
// set-full-screen
// Returns: void
// Description:
// Requests that the current fluxus window be made full screen.
// Example:
// (set-full-screen)
// EndFunctionDoc

// StartFunctionDoc-pt
// set-full-screen
// Retorna: void
// Descrição:
// Pede que a janela atual do fluxus fique tela cheia.
// Exemplo:
// (set-full-screen)
// EndFunctionDoc

// StartFunctionDoc-fr
// set-full-screen
// Retour: void
// Description:
// Demande à ce que la fenêtre fluxus courante soit mise en plein écran.
// Exemple:
// (set-full-screen)
// EndFunctionDoc

Scheme_Object *set_full_screen(int argc, Scheme_Object **argv)
{
  DECL_ARGV();
  glutFullScreen();
  MZ_GC_UNREG();
  return scheme_void;
}

void GlobalStateFunctions::AddGlobals(Scheme_Env *env)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_REG();

	scheme_add_global("clear-engine", scheme_make_prim_w_arity(clear_engine, "clear-engine", 0, 0), env);
	scheme_add_global("blur", scheme_make_prim_w_arity(blur, "blur", 1, 1), env);
	scheme_add_global("fog", scheme_make_prim_w_arity(fog, "fog", 4, 4), env);
	scheme_add_global("show-axis", scheme_make_prim_w_arity(show_axis, "show-axis", 1, 1), env);
	scheme_add_global("show-fps", scheme_make_prim_w_arity(show_fps, "show-fps", 1, 1), env);
	scheme_add_global("lock-camera", scheme_make_prim_w_arity(lock_camera, "lock-camera", 1, 1), env);
	scheme_add_global("camera-lag", scheme_make_prim_w_arity(camera_lag, "camera-lag", 1, 1), env);
	scheme_add_global("load-texture", scheme_make_prim_w_arity(load_texture, "load-texture", 1, 2), env);
	scheme_add_global("clear-texture-cache", scheme_make_prim_w_arity(clear_texture_cache, "clear-texture-cache", 0, 0), env);
	scheme_add_global("is-resident?",scheme_make_prim_w_arity(is_resident,"is-resident?",1,1), env);
	scheme_add_global("set-texture-priority",scheme_make_prim_w_arity(is_resident,"set-texture-priority",2,2), env);
	scheme_add_global("texture-width",scheme_make_prim_w_arity(texture_width,"texture-width",1,1), env);
	scheme_add_global("texture-height",scheme_make_prim_w_arity(texture_height,"texture-height",1,1), env);
	scheme_add_global("frustum", scheme_make_prim_w_arity(frustum, "frustum", 4, 4), env);
	scheme_add_global("clip", scheme_make_prim_w_arity(clip, "clip", 2, 2), env);
	scheme_add_global("ortho", scheme_make_prim_w_arity(ortho, "ortho", 0, 0), env);
	scheme_add_global("persp", scheme_make_prim_w_arity(persp, "persp", 0, 0), env);
	scheme_add_global("set-ortho-zoom", scheme_make_prim_w_arity(set_ortho_zoom, "set-ortho-zoom", 1, 1), env);
	scheme_add_global("clear-colour", scheme_make_prim_w_arity(clear_colour, "clear-colour", 1, 1), env);
	scheme_add_global("clear-frame", scheme_make_prim_w_arity(clear_frame, "clear-frame", 1, 1), env);
	scheme_add_global("clear-zbuffer", scheme_make_prim_w_arity(clear_zbuffer, "clear-zbuffer", 1, 1), env);
	scheme_add_global("clear-accum", scheme_make_prim_w_arity(clear_accum, "clear-accum", 1, 1), env);
	scheme_add_global("build-camera", scheme_make_prim_w_arity(build_camera, "build-camera", 0, 0), env);
	scheme_add_global("current-camera", scheme_make_prim_w_arity(current_camera, "current-camera", 1, 1), env);
	scheme_add_global("viewport", scheme_make_prim_w_arity(viewport, "viewport", 4, 4), env);
	scheme_add_global("get-locked-matrix", scheme_make_prim_w_arity(get_locked_matrix, "get-locked-matrix", 0, 0), env);
	scheme_add_global("get-camera", scheme_make_prim_w_arity(get_camera, "get-camera", 0, 0), env);
	scheme_add_global("set-camera", scheme_make_prim_w_arity(set_camera, "set-camera", 1, 1), env);
	scheme_add_global("get-projection-transform", scheme_make_prim_w_arity(get_projection_transform, "get-projection-transform", 0, 0), env);
	scheme_add_global("set-projection-transform", scheme_make_prim_w_arity(set_projection_transform, "set-projection-transform", 1, 1), env);
	scheme_add_global("get-screen-size", scheme_make_prim_w_arity(get_screen_size, "get-screen-size", 0, 0), env);
	scheme_add_global("set-screen-size", scheme_make_prim_w_arity(set_screen_size, "set-screen-size", 1, 1), env);
	scheme_add_global("select", scheme_make_prim_w_arity(select, "select", 3, 3), env);
	scheme_add_global("select-all", scheme_make_prim_w_arity(select_all, "select-all", 3, 3), env);
	scheme_add_global("desiredfps", scheme_make_prim_w_arity(desiredfps, "desiredfps", 1, 1), env);
	scheme_add_global("draw-buffer", scheme_make_prim_w_arity(draw_buffer, "draw-buffer", 1, 1), env);
	scheme_add_global("read-buffer", scheme_make_prim_w_arity(read_buffer, "read-buffer", 1, 1), env);
	scheme_add_global("set-stereo-mode", scheme_make_prim_w_arity(set_stereo_mode, "set-stereo-mode", 1, 1), env);
	scheme_add_global("get-stereo-mode", scheme_make_prim_w_arity(get_stereo_mode, "get-stereo-mode", 0, 0), env);
	scheme_add_global("set-colour-mask", scheme_make_prim_w_arity(set_colour_mask, "set-colour-mask", 1, 1), env);
	scheme_add_global("shadow-light", scheme_make_prim_w_arity(shadow_light, "shadow-light", 1, 1), env);
	scheme_add_global("shadow-length", scheme_make_prim_w_arity(shadow_length, "shadow-length", 1, 1), env);
	scheme_add_global("shadow-debug", scheme_make_prim_w_arity(shadow_debug, "shadow-ldebug", 1, 1), env);
	scheme_add_global("accum", scheme_make_prim_w_arity(accum, "accum", 2, 2), env);
	scheme_add_global("print-info", scheme_make_prim_w_arity(print_info, "print-info", 0, 0), env);
	scheme_add_global("set-cursor",scheme_make_prim_w_arity(set_cursor,"set-cursor",1,1), env);
	scheme_add_global("set-full-screen", scheme_make_prim_w_arity(set_full_screen, "set-full-screen", 0, 0), env);

	MZ_GC_UNREG();
}

