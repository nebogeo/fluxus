// Copyright (C) 2005 Dave Griffiths
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

#include <guile/gh.h>
#include "Repl.h"

string Repl::m_Banner = string("Welcome to fluxus\n");
string Repl::m_Prompt = string("fluxus> ");

SCM ErrorHandler (void *handler_data, SCM tag, SCM args);

Repl::Repl()
{
	Print(m_Banner + m_Prompt);
}

// FIXME: automatically restore partial command line
void Repl::Print(string what)
{
	m_Text += what;
	m_Position = m_PromptPos = m_Text.length();
}

void Repl::Print(SCM obj)
{
	SCM str_obj = scm_object_to_string(obj, SCM_UNDEFINED);
	Print(string(SCM_STRING_CHARS(str_obj)));
}

void Repl::Handle(int button, int key, int special, int state, 
		  int x, int y, int mod)
{
	if (key!=0) {
		if (m_Position < m_PromptPos ||
		    (m_Position == m_PromptPos && key == GLEDITOR_BACKSPACE))
			return; // read only area
		
	}
	
	GLEditor::Handle(button, key, special, state, 
			 x, y, mod);
	
	// react on return AFTER new line is output
	if (key == GLEDITOR_RETURN) TryEval();
}

// FIXME: skip parens in strings and comments
bool Balanced(string s)
{
	int balance = 0;
	for (string::iterator i = s.begin();
	     i != s.end();
	     i++) {
		switch(*i) {
			case '(':
				balance++;
				break;
			case ')':
				balance--;
				break;
		}
		
		if (balance<0) return false;
	}
	
	return balance==0;
}

// FIXME don't bother evaluating empty strings (which ARE balanced)
bool Repl::TryEval()
{
	string defun = m_Text.substr(m_PromptPos);
	
	if (!Balanced(defun)) return false;
	
	SCM res0 = scm_internal_catch(SCM_BOOL_T,
				      (scm_t_catch_body)scm_c_eval_string,
				      (void*)(defun.c_str()),
				      ErrorHandler,
				      (void*)"fluxus");
	
	if (res0 != SCM_UNDEFINED && res0 != SCM_UNSPECIFIED) {	
		Print(res0);
		Print("\n");
	}
	Print(m_Prompt);
	
	return true;
}

