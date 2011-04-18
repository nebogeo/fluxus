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

#include <escheme.h>
#include <iostream>
#include "OSCServer.h"

using namespace std;
using namespace fluxus;

Server *OSCServer = NULL;
Client *OSCClient = NULL;

// StartSectionDoc-en
// osc
// OSC stands for Open Sound Control, and is a widely used protocol for passing data 
// between multimedia applications. Fluxus can send or receive messages.
// Example:
// An example of using osc to communicate between pd and fluxus.
// A fluxus script to move a cube based on incoming osc messages.
// -- osc.scm
//
// (define value 0)
//
// (define (test)
//     (push)
//     (when (osc-msg "/zzz")
//         (set! value (osc 0)))
//     (translate (vector 1 0 value))
//     (draw-cube)
//     (pop))
//
// (osc-source "6543")
// (every-frame (test))
//
// --- EOF
//
// A PD patch to send control messages to fluxus:
// --- zzz.pd
// #N canvas 618 417 286 266 10;
// #X obj 58 161 sendOSC;
// #X msg 73 135 connect localhost 6543;
// #X msg 58 82 send /zzz \$1;
// #X floatatom 58 29 5 0 0 0 - - -;
// #X obj 58 54 / 100;
// #X obj 73 110 loadbang;
// #X connect 1 0 0 0;
// #X connect 2 0 0 0;
// #X connect 3 0 4 0;
// #X connect 4 0 2 0;
// #X connect 5 0 1 0;
// EndSectionDoc 

// StartSectionDoc-pt
// osc
// OSC significa Open Sound Control, e é um protocolo amplamente usado
// para passar dados entre aplicações multimidia. Fluxus pode enviar e/ou
// receber mensagens.
// Exemplo:
// Um exemplo de uso de osc para comunicar entre pd e fluxus.
// Um script fluxus para mover um cubo baseado em mensagens osc
// entrando.
// -- osc.scm
//
// (define value 0)
//
// (define (test)
//     (push)
//     (if (osc-msg "/zzz")
//         (set! value (osc 0)))
//     (translate (vector 1 0 value))
//     (draw-cube)
//     (pop))
// 
// (osc-source "6543")
// (every-frame (test))
// 
// --- EOF
//
// Um patch PD para enviar mensagens de controle ao fluxus:
// --- zzz.pd
// #N canvas 618 417 286 266 10;
// #X obj 58 161 sendOSC;
// #X msg 73 135 connect localhost 6543;
// #X msg 58 82 send /zzz \$1;
// #X floatatom 58 29 5 0 0 0 - - -;
// #X obj 58 54 / 100;
// #X obj 73 110 loadbang;
// #X connect 1 0 0 0;
// #X connect 2 0 0 0;
// #X connect 3 0 4 0;
// #X connect 4 0 2 0;
// #X connect 5 0 1 0;
// EndSectionDoc 


// StartFunctionDoc-en
// osc-source port-string
// Returns: void
// Description:
// Starts up the osc server, or changes port. Known bug: seems to fail if you set 
// it back to a port used previously.
// Example:
// (osc-source "4444")	 ; listen to port 4444 for osc messages
// EndFunctionDoc

// StartFunctionDoc-pt
// osc-source string-porta
// Retorna: void
// Descrição:
// Inicia o servidor osc, ou muda a porta. Bug conhecido: parece
// falhar se você usa de volta pra uma mesma porta já usada anteriormente.
// Exemplo:
// (osc-source "4444")	 ; listen to port 4444 for osc messages
// EndFunctionDoc

Scheme_Object *osc_source(int argc, Scheme_Object **argv)
{
	char *port=NULL;
	MZ_GC_DECL_REG(2); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_VAR_IN_REG(1, port); 
	MZ_GC_REG();	
	
	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("osc-source", "string", 0, argc, argv);
	port=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);

	if (!OSCServer)
	{
		OSCServer = new Server(port);
		OSCServer->Run();
	}
	else
	{
		OSCServer->SetPort(port);
	}

	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// osc-msg name-string
// Returns: msgreceived-boolean
// Description:
// Returns true if the message has been received since the last frame, and sets it as the current 
// message for subsequent calls to (osc) for reading the arguments. 
// Example:
// (cond 
//     ((osc-msg "/hello")              ; if a the /hello message is recieved
//         (display (osc 1))(newline)))	; print out the first argument
// EndFunctionDoc

// StartFunctionDoc-pt
// osc-msg string-nome
// Retorna: void
// Descrição:
// Retorna verdadeiro se a mensagem foi recebida desde o último
// quadro, e ajusta ela como a mensagem atual para chamadas
// subsequente ao (osc) para ler argumentos.
// Exemplo:
// (cond 
//     ((osc-msg "/hello")              ; if a the /hello message is recieved
//         (display (osc 1))(newline)))	; print out the first argument
// EndFunctionDoc

Scheme_Object *osc_msg(int argc, Scheme_Object **argv)
{
	char *name=NULL;
	MZ_GC_DECL_REG(2); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_VAR_IN_REG(1, name); 
	MZ_GC_REG();
		
	if (OSCServer!=NULL)
	{
		if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("osc-msg", "string", 0, argc, argv);
		name=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
		if (OSCServer->SetMsg(name))
		{
			MZ_GC_UNREG(); 
			return scheme_make_true();
		}
	}
	MZ_GC_UNREG(); 
	return scheme_make_false();
}

// StartFunctionDoc-en
// osc argument-number
// Returns: oscargument
// Description:
// Returns the argument from the current osc message. 
// Example:
// (cond 
//     ((osc-msg "/hello")              ; if a the /hello message is recieved
//         (display (osc 1))(newline)))	; print out the first argument
// EndFunctionDoc

// StartFunctionDoc-pt
// osc número-argumento
// Retorna: void
// Descrição:
// Retorna o argumento da mensagem osc atual.
// Exemplo:
// (cond 
//     ((osc-msg "/hello")              ; if a the /hello message is recieved
//         (display (osc 1))(newline)))	; print out the first argument
// EndFunctionDoc

Scheme_Object *osc(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret=NULL;
	MZ_GC_DECL_REG(2); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_VAR_IN_REG(1, ret); 
	MZ_GC_REG();	
	
	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("osc", "number", 0, argc, argv);
 
	unsigned int index=(unsigned int)scheme_real_to_double(argv[0]);
	if (OSCServer!=NULL)
	{
		vector<OSCData*> args;
		OSCServer->GetArgs(args);
		if (index<args.size())
		{
			char type = args[index]->Type();
		
			if (type=='f') ret=scheme_make_double(static_cast<OSCFloat*>(args[index])->Value);
			else if (type=='i') ret=scheme_make_integer_value_from_unsigned(static_cast<OSCInt*>(args[index])->Value);
			else if (type=='s') 
			{
				string value=static_cast<OSCString*>(args[index])->Value;
				ret=scheme_make_utf8_string(value.c_str());	
			}
			else ret=scheme_void;
		}
		else 
		{
			cerr<<"osc argument out of range"<<endl;
			ret=scheme_void;
		}

		MZ_GC_UNREG(); 
		return ret;
	}
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// osc-destination port-string
// Returns: void
// Description:
// Specifies the destination for outgoing osc messages. The port name needs to specify the whole
// url and should look something like this "osc.udp://localhost:4444"
// Example:
// (osc-destination "osc.udp://localhost:4444")
// (osc-send "/hello" "s" (list "boo!"))  ; send a message to this destination
// EndFunctionDoc

// StartFunctionDoc-pt
// osc-destination string-porta
// Retorna: void
// Descrição:
// Específica o destino para mensagens osc que estejam saindo. O nome
// da porta precisa ser específicado com todo o url e deve ser algo do
// tipo "osc.udp://localhost:4444"
// Exemplo:
// (osc-destination "osc.udp://localhost:4444")
// (osc-send "/hello" "s" (list "boo!"))  ; send a message to this destination
// EndFunctionDoc

Scheme_Object *osc_destination(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_REG();	
	
	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("osc-destination", "string", 0, argc, argv);
	char *port=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
	if (!OSCClient)
	{
		OSCClient = new Client();
	}
	OSCClient->SetDestination(port);
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// osc-peek 
// Returns: msg-string
// Description:
// This util function returns the name, and format string and number/string arguments
// of the last sent message as a string - for debugging your osc network.
// Example:
// (display (osc-peek))(newline) 
// EndFunctionDoc

// StartFunctionDoc-pt
// osc-peek
// Retorna: msg-string
// Descrição:
// Esta útil função retorna o nome, formato da string e número/string
// dos argumentos da última mensagem enviada como uma string - para
// debugar sua rede osc.
// Exemplo:
// (display (osc-peek))(newline) 
// EndFunctionDoc

Scheme_Object *osc_peek(int argc, Scheme_Object **argv)
{
	if (OSCServer!=NULL)
	{
		return scheme_make_utf8_string(OSCServer->GetLastMsg().c_str());	
	}
	else return scheme_make_utf8_string("");
}

// StartFunctionDoc-en
// osc-send name-string format-string argument-list 
// Returns: void
// Description:
// Sends an osc message with the argument list as the osc data. Only supports 
// floats, ints and strings as data. The format-string should be composed of "i", "f" and "s",
// and must match the types given in the list. This could probably be removed by using the types 
// directly, but doing it this way allows you to explicitly set the typing for the osc message.
// Example:
// (osc-destination "osc.udp://localhost:4444")
// (osc-send "/hello" "sif" (list "boo!" 3 42.3))  ; send a message to this destination
// EndFunctionDoc

// StartFunctionDoc-pt
// osc-send string-nome string-formato lista-argumento
// Retorna: void
// Descrição:
// Envia uma mensagem osc com o argumento da lista como dado
// osc. Somente suporta floats, ints e strings como dados. O
// formato-string deve ser composto de "i", "f" e "s", e deve bater
// com os tipos dados na lista. Isto poderia ser removido
// provavelmente, usando os tipos diretamente, mas fazendo isto desta
// forma permite que você diga o tipo da mensagem osc explicitamente.
// Exemplo:
// (osc-destination "osc.udp://localhost:4444")
// (osc-send "/hello" "sif" (list "boo!" 3 42.3))  ; send a message to this destination
// EndFunctionDoc

Scheme_Object *osc_send(int argc, Scheme_Object **argv)
{
	Scheme_Object *argvec = NULL;
	MZ_GC_DECL_REG(2); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_VAR_IN_REG(1, argvec); 
	MZ_GC_REG();	
	
	if (!OSCClient) 
	{
		MZ_GC_UNREG(); 
		return scheme_void;
	}
	
	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("osc-send", "string", 0, argc, argv);
	if (!SCHEME_CHAR_STRINGP(argv[1])) scheme_wrong_type("osc-send", "string", 1, argc, argv);
	if (!SCHEME_LISTP(argv[2])) scheme_wrong_type("osc-send", "list", 2, argc, argv);

	char *msg=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
	char *types=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[1]),SCHEME_CHAR_STRLEN_VAL(argv[1]),NULL,0);
	
	argvec = scheme_list_to_vector(argv[2]);
	
	vector<OSCData*> oscargs;
	for (unsigned int n=0; n<(unsigned int)SCHEME_VEC_SIZE(argvec); n++)
	{
		if (SCHEME_NUMBERP(SCHEME_VEC_ELS(argvec)[n]))// ||  scm_is_true(scm_exact_p(arg)) || scm_is_true(scm_inexact_p(arg)))
		{
			if (n<strlen(types))
			{
				if (types[n]=='f') oscargs.push_back(new OSCFloat(scheme_real_to_double(SCHEME_VEC_ELS(argvec)[n])));
				else if (types[n]=='i') 
				{
					uintptr_t val=0;
					scheme_get_unsigned_int_val(SCHEME_VEC_ELS(argvec)[n],&val);
					oscargs.push_back(new OSCInt(val));
				}
			}
		}
		else if (SCHEME_CHAR_STRINGP(SCHEME_VEC_ELS(argvec)[n]))
		{
			char *argstring=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(SCHEME_VEC_ELS(argvec)[n]),
														 SCHEME_CHAR_STRLEN_VAL(SCHEME_VEC_ELS(argvec)[n]),NULL,0);
			oscargs.push_back(new OSCString(argstring));
		}
		else
		{
			cerr<<"osc-send has found an argument type it can't send, numbers and strings only"<<endl;
			MZ_GC_UNREG(); 
    		return scheme_void;
		}
	}
	OSCClient->Send(msg,oscargs);
	MZ_GC_UNREG(); 
    return scheme_void;
}

/////////////////////

#ifdef STATIC_LINK
Scheme_Object *osc_scheme_reload(Scheme_Env *env)
#else
Scheme_Object *scheme_reload(Scheme_Env *env)
#endif
{
	Scheme_Env *menv=NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_VAR_IN_REG(1, menv);
	MZ_GC_REG();
	// add all the modules from this extension
	menv=scheme_primitive_module(scheme_intern_symbol("fluxus-osc"), env);

	scheme_add_global("osc-source", scheme_make_prim_w_arity(osc_source, "osc-source", 1, 1), menv);
	scheme_add_global("osc-msg", scheme_make_prim_w_arity(osc_msg, "osc-msg", 1, 1), menv);
	scheme_add_global("osc", scheme_make_prim_w_arity(osc, "osc", 1, 1), menv);
	scheme_add_global("osc-destination", scheme_make_prim_w_arity(osc_destination, "osc-destination", 1, 1), menv);
	scheme_add_global("osc-peek", scheme_make_prim_w_arity(osc_peek, "osc-peek", 0, 0), menv);
	scheme_add_global("osc-send", scheme_make_prim_w_arity(osc_send, "osc-send", 3, 3), menv);

	scheme_finish_primitive_module(menv);	
 	MZ_GC_UNREG(); 
	
	return scheme_void;
}

#ifndef STATIC_LINK
Scheme_Object *scheme_initialize(Scheme_Env *env)
{
	return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
	return scheme_intern_symbol("fluxus-osc");
}
#endif

