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
#include <escheme.h>
#include <GLSLShader.h>
#include "FluxusEngine.h"
#include "Engine.h"
#include "MathsFunctions.h"
#include "GlobalStateFunctions.h"
#include "LocalStateFunctions.h"
#include "PrimitiveFunctions.h"
#include "PDataFunctions.h"
#include "UtilFunctions.h"
#include "TurtleFunctions.h"
#include "LightFunctions.h"
#include "PhysicsFunctions.h"
#include "SchemeHelper.h"
#include "Trace.h"

using namespace SchemeHelper;

// StartSectionDoc-en
// renderer
// These commands are the low level renderer controls. You shouldn't need to 
// deal with these unless you are being wily, or implementing a fluxus 
// renderer outside of the scratchpad interface.
// Example:
// EndSectionDoc 

// StartSectionDoc-pt
// renderer
// Estes comandos são os controladores de baixo nível do
// renderizador. Você não deveria usá-los a menos que esteja
// implementando um renderizador do fluxus fora da interface
// scratchpad. 
// Exemplo:
// EndSectionDoc

// StartFunctionDoc-en
// make-renderer 
// Returns: rendererid-number
// Description:
// Makes a new scenegraph renderer.
// Example:
// (make-renderer) 
// EndFunctionDoc

// StartFunctionDoc-pt
// make-renderer
// Retorna: número-render-id
// Descrição:
// Faz um novo renderizador de gráfico da cena [scenegraph]
// Exemplo:
// (make-renderer) 
// EndFunctionDoc

// base plt bindings & register all other bindings from here

Scheme_Object *make_renderer(int argc, Scheme_Object **argv)
{
	return scheme_make_integer_value(Engine::Get()->MakeRenderer());
}

// StartFunctionDoc-en
// renderer-grab rendererid-number
// Returns: void
// Description:
// Make this renderer the current context for commands.
// Example:
// (renderer-grab renderer) 
// EndFunctionDoc

// StartFunctionDoc-pt
// renderer-grab número-id-renderizador
// Retorna: void
// Descrição:
// Faz este renderizador o atual contexto para comandos.
// Exemplo:
// (renderer-grab renderer) 
// EndFunctionDoc

Scheme_Object *renderer_grab(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	if (!SCHEME_INTP(argv[0])) scheme_wrong_type("renderer-grab", "integer", 0, argc, argv);
	Engine::Get()->PushRenderer(IntFromScheme(argv[0]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// renderer-ungrab
// Returns: void
// Description:
// Pop the renderer context stack.
// Example:
// (renderer-grab renderer) 
// EndFunctionDoc

// StartFunctionDoc-pt
// renderer-ungrab
// Retorna: void
// Descrição:
// Pop o contexto da pilha do renderizador.
// Exemplo:
// (renderer-grab renderer) 
// EndFunctionDoc

Scheme_Object *renderer_ungrab(int argc, Scheme_Object **argv)
{
	Engine::Get()->PopRenderer();
	return scheme_void;
}

// StartFunctionDoc-en
// fluxus-render
// Returns: void
// Description:
// Clears the backbuffer, and renders everything
// Example:
// (fluxus-render) 
// EndFunctionDoc

// StartFunctionDoc-pt
// fluxus-render
// Retorna: void
// Descrição:
// Limpa o buffer de fundo, e renderiza tudo.
// Exemplo:
// (fluxus-render)
// EndFunctionDoc

Scheme_Object *fluxus_render(int argc, Scheme_Object **argv)
{
	Engine::Get()->Render();
	return scheme_void;
}

// StartFunctionDoc-en
// tick-physics
// Returns: void
// Description:
// Update the physics system.
// Example:
// (tick-physics) 
// EndFunctionDoc

// StartFunctionDoc-pt
// tick-physics
// Retorna: void
// Descrição:
// Atualiza o sistema de física.
// Exemplo:
// (tick-physics) 
// EndFunctionDoc

Scheme_Object *tick_physics(int argc, Scheme_Object **argv)
{
	Engine::Get()->Physics()->Tick();
	return scheme_void;
}

// StartFunctionDoc-en
// render-physics
// Returns: void
// Description:
// Render the physics system (for helper graphics).
// Example:
// (render-physics) 
// EndFunctionDoc

// StartFunctionDoc-pt
// render-physics
// Retorna: void
// Descrição:
// Renderiza o sistema de física (para gráficos ajudantes).
// Exemplo:
// (render-physics)
// EndFunctionDoc

Scheme_Object *render_physics(int argc, Scheme_Object **argv)
{
	Engine::Get()->Physics()->Render();
	return scheme_void;
}

// StartFunctionDoc-en
// reset-renderers
// Returns: void
// Description:
// Deletes all the renderers and makes a new default one.
// Example:
// (reset-renderers) 
// EndFunctionDoc

// StartFunctionDoc-pt
// reset-renderers
// Retorna: void
// Descrição:
// Deleta todos os renderizadores e faz um novo como padrão.
// Exemplo:
// (reset-renderers)
// EndFunctionDoc

Scheme_Object *reset_renderers(int argc, Scheme_Object **argv)
{
	Engine::Get()->ResetRenderers();
	return scheme_void;
}

// StartFunctionDoc-en
// reshape width-number height-number
// Returns: void
// Description:
// Calls reshape on the current renderer
// Example:
// (reshape 100 100) 
// EndFunctionDoc

// StartFunctionDoc-pt
// reshape número-largura número-altura
// Retorna: void
// Descrição:
// Chama reshapa no render atual.
// Exemplo:
// (reshape 100 100) 
// EndFunctionDoc

Scheme_Object *reshape(int argc, Scheme_Object **argv)
{
	DECL_ARGV();
	if (!SCHEME_INTP(argv[0])) scheme_wrong_type("reshape", "integer", 0, argc, argv);
	if (!SCHEME_INTP(argv[1])) scheme_wrong_type("reshape", "integer", 1, argc, argv);
	Engine::Get()->Renderer()->SetResolution(IntFromScheme(argv[0]),IntFromScheme(argv[1]));
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// fluxus-init 
// Returns: void
// Description:
// Inits the whole rendering system, only needs calling once.
// Example:
// (fluxus-init) 
// EndFunctionDoc

// StartFunctionDoc-pt
// fluxus-init
// Retorna: void
// Descrição:
// Inicia todo o sistema de renderização, só precisa ser chamado uma vez.
// Exemplo:
// (fluxus-init)
// EndFunctionDoc

Scheme_Object *fluxus_init(int argc, Scheme_Object **argv)
{
	#ifdef GLSL
	if(glewInit() != GLEW_OK)
	{
		Trace::Stream << "ERROR Unable to check OpenGL extensions" << endl;
	}

	Fluxus::GLSLShader::Init();
	#endif
	
	Engine::Get()->Reinitialise();
	
	return scheme_void;
}

// StartFunctionDoc-en
// fluxus-error-log
// Returns: void
// Description:
// Returns a string containing error information for the last frame.
// Example:
// (display (fluxus-error-log))
// EndFunctionDoc

Scheme_Object *fluxus_error_log(int argc, Scheme_Object **argv)
{
	return scheme_make_utf8_string(Trace::Get().c_str());
}

////////////////////////////////////////////////////////////////////

Scheme_Object *scheme_reload(Scheme_Env *env)
{
	Scheme_Env *menv = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_VAR_IN_REG(1, menv);

	MZ_GC_REG();
  	// add all the modules from this extension
	menv=scheme_primitive_module(scheme_intern_symbol("fluxus-engine"), env);
	
	GlobalStateFunctions::AddGlobals(menv);
	LocalStateFunctions::AddGlobals(menv);
	MathsFunctions::AddGlobals(menv); 
	PrimitiveFunctions::AddGlobals(menv);
	PDataFunctions::AddGlobals(menv);
	UtilFunctions::AddGlobals(menv);
	TurtleFunctions::AddGlobals(menv);
	LightFunctions::AddGlobals(menv);
	PhysicsFunctions::AddGlobals(menv);
	
	scheme_add_global("fluxus-init", scheme_make_prim_w_arity(fluxus_init, "fluxus-init", 0, 0), menv);
	scheme_add_global("make-renderer", scheme_make_prim_w_arity(make_renderer, "make-renderer", 0, 0), menv);
	scheme_add_global("reset-renderers", scheme_make_prim_w_arity(reset_renderers, "reset-renderers", 0, 0), menv);
	scheme_add_global("renderer-grab", scheme_make_prim_w_arity(renderer_grab, "renderer-grab", 1, 1), menv);
	scheme_add_global("renderer-ungrab", scheme_make_prim_w_arity(renderer_ungrab, "renderer-ungrab", 0, 0), menv);
	scheme_add_global("fluxus-render", scheme_make_prim_w_arity(fluxus_render, "fluxus-render", 0, 0), menv);
	scheme_add_global("tick-physics", scheme_make_prim_w_arity(tick_physics, "tick-physics", 0, 0), menv);
	scheme_add_global("render-physics", scheme_make_prim_w_arity(render_physics, "render-physics", 0, 0), menv);
	scheme_add_global("reshape", scheme_make_prim_w_arity(reshape, "reshape", 2, 2), menv);
	scheme_add_global("fluxus-error-log", scheme_make_prim_w_arity(fluxus_error_log, "fluxus-error-log", 0, 0), menv);

	scheme_finish_primitive_module(menv);	
	
	#ifdef MZ_PRECISE_GC
	MZ_GC_UNREG();
	#endif
	
	return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
	return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
	return scheme_intern_symbol("fluxus-engine");
}
