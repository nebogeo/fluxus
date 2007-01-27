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

#include <iostream>
#include "Interpreter.h"
#include "Repl.h"

using namespace std;
using namespace fluxus;
	
Interpreter::Interpreter(Scheme_Env *e) : 
m_Scheme(NULL),
m_Repl(NULL)
{
	MZ_REGISTER_STATIC(m_Scheme);
	m_Scheme=e;
}

Interpreter::~Interpreter() 
{
}


void Interpreter::Interpret(const string &str, Scheme_Object **ret, bool abort)
{	
	Scheme_Object *outport=NULL;
	Scheme_Object *errport=NULL;
	
	MZ_GC_DECL_REG(0);

    MZ_GC_VAR_IN_REG(0, outport);
    MZ_GC_VAR_IN_REG(1, errport);
    MZ_GC_REG();
	
	if (m_Repl) 
	{
		outport = scheme_make_byte_string_output_port();
		errport = scheme_make_byte_string_output_port();
		scheme_set_param(scheme_current_config(), MZCONFIG_OUTPUT_PORT, outport);
		scheme_set_param(scheme_current_config(), MZCONFIG_ERROR_PORT, errport);
	}
	
 	mz_jmp_buf * volatile save, fresh;
	
	save = scheme_current_thread->error_buf;
    scheme_current_thread->error_buf = &fresh;
	
    if (scheme_setjmp(scheme_error_buf)) 
	{
		scheme_current_thread->error_buf = save;
		if (abort) exit(-1);
    } 
	else 
	{
		if (ret==NULL)
		{
			scheme_eval_string_all(str.c_str(), m_Scheme, 1);
		}
		else
		{
			*ret = scheme_eval_string_all(str.c_str(), m_Scheme, 1);
		}
		scheme_current_thread->error_buf = save;
    }
		
	// todo: support colour escape codes in the editor for
	// error/output colour formatting...
	if (outport!=NULL)
	{
		long size=0;
		char *msg;
		msg=scheme_get_sized_byte_string_output(outport,&size);	
		if (size>0) m_Repl->Print(string(msg));
	}	
	
	if (errport!=NULL)
	{
		long size=0;
		char *msg;
		msg=scheme_get_sized_byte_string_output(errport,&size);
		if (size>0) m_Repl->Print(string(msg));
	}	
	
	MZ_GC_UNREG();
}

