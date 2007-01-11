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

#ifndef _FLUXUS_REPL_H_
#define _FLUXUS_REPL_H_

#include <deque>
#include <scheme.h>
#include "GLEditor.h"
#include "Interpreter.h"

namespace fluxus 
{

class Repl : public GLEditor
{
public:
	Repl(Interpreter *i);
	virtual ~Repl() {}
	
	virtual void Handle(int button, int key, int special, int state, 
			    int x, int y, int mod);
	
	void Print(string what);
	void Print(Scheme_Object *obj);

protected:
	static string m_Prompt;
	static string m_Banner;
	
	unsigned int m_PromptPos;
	unsigned int m_InsertPos;

	bool TryEval();
	void PrintPrompt();
	void HistoryPrev();
	void HistoryNext();
	void HistoryShow(string what);
	void EnsureCursorVisible();

	// line history
	deque<string> 			m_History;
	deque<string>::iterator m_HistoryIter;
	bool 					m_HistoryNavStarted;
	string 					m_HistoryPresent;
	
	Interpreter*			m_Interpreter;
};

}

#endif
