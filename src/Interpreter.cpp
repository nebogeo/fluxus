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

static const int LOG_SIZE=512;

Interpreter::Interpreter(Scheme_Env *e) : 
m_Scheme(NULL),
m_Repl(NULL),
outrport(NULL),
errrport(NULL),
outwport(NULL),
errwport(NULL)
{
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, e);
	MZ_GC_VAR_IN_REG(1, m_Scheme);
    MZ_GC_REG();
	m_Scheme=e;
	MZ_GC_UNREG();
}

Interpreter::~Interpreter()
{
}

void Interpreter::SetRepl(Repl *s) 
{ 
	Scheme_Config *config;
	MZ_GC_DECL_REG(5);
    MZ_GC_VAR_IN_REG(0, outrport);
    MZ_GC_VAR_IN_REG(1, errrport);
    MZ_GC_VAR_IN_REG(2, outwport);
    MZ_GC_VAR_IN_REG(3, errwport);
    MZ_GC_VAR_IN_REG(4, config);
    MZ_GC_REG();
	m_Repl=s; 
	scheme_pipe_with_limit(&outrport,&outwport,LOG_SIZE);
	scheme_pipe_with_limit(&errrport,&errwport,LOG_SIZE);
	config = scheme_current_config();
	scheme_set_param(config, MZCONFIG_OUTPUT_PORT, outwport);
	scheme_set_param(config, MZCONFIG_ERROR_PORT, errwport);
    MZ_GC_UNREG();
}

void fill_from_port(Scheme_Object* port, char *dest, long size)
{
	MZ_GC_DECL_REG(2);
    MZ_GC_VAR_IN_REG(0, port);
    MZ_GC_VAR_IN_REG(1, dest);
    MZ_GC_REG();
	
	long pos=0;	
	while (scheme_char_ready(port) && pos<size)
	{
		dest[pos++]=scheme_getc(port);
	}
	dest[pos]=0;
		
	MZ_GC_UNREG();
}

bool Interpreter::Interpret(const string &str, Scheme_Object **ret, bool abort)
{	
	char msg[LOG_SIZE];
	mz_jmp_buf * volatile save = NULL, fresh;
	
	MZ_GC_DECL_REG(6);
    MZ_GC_VAR_IN_REG(0, outrport);
    MZ_GC_VAR_IN_REG(1, errrport);
    MZ_GC_VAR_IN_REG(2, outwport);
    MZ_GC_VAR_IN_REG(3, errwport);
	MZ_GC_VAR_IN_REG(4, m_Scheme);
	MZ_GC_VAR_IN_REG(5, msg);
    MZ_GC_REG();
		
	save = scheme_current_thread->error_buf;
    scheme_current_thread->error_buf = &fresh;
	
    if (scheme_setjmp(scheme_error_buf)) 
	{
		scheme_current_thread->error_buf = save;
		if (errrport!=NULL) 
		{
			fill_from_port(errrport, msg, LOG_SIZE);
			if (strlen(msg)>0) m_Repl->Print(string(msg));
		}
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
		//scheme_check_threads();
		scheme_current_thread->error_buf = save;
    }
		
	if (outrport!=NULL)
	{
		fill_from_port(outrport, msg, LOG_SIZE);
		if (strlen(msg)>0) m_Repl->Print(string(msg));
	}	
	
	MZ_GC_UNREG();
	return true;
}

