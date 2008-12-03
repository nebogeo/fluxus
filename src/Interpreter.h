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

#ifndef _FLUXUS_INTERPRETER_H_
#define _FLUXUS_INTERPRETER_H_

#include <deque>
#include <string>
#include <scheme.h>

namespace fluxus 
{

class Repl;

class Interpreter 
{
public:	
	static void Register(Scheme_Env* se);
	static void Initialise();
	static void SetRepl(Repl *s);
	static bool Interpret(const std::string &code, Scheme_Object **ret=NULL, bool abort=false);
	static void SetLanguage(const std::string &lang) { m_Language=lang; }
	
private:
	static std::string SetupLanguage(const std::string &str);

	static Scheme_Env *m_Scheme;
	static Repl *m_Repl;
	static Scheme_Object *m_OutReadPort;
	static Scheme_Object *m_ErrReadPort;
	static Scheme_Object *m_OutWritePort;
	static Scheme_Object *m_ErrWritePort;
	static std::string m_Language;
};

}

#endif
