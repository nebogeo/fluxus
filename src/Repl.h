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

#include <libguile/tags.h>
#include <deque>
#include "GLEditor.h"

class Repl : public GLEditor
{
public:
	Repl();
	virtual ~Repl() {}
	
	virtual void Handle(int button, int key, int special, int state, 
			    int x, int y, int mod);
	
	void Print(string what);
	void Print(SCM obj);
protected:
	static string m_Prompt;
	static string m_Banner;
	
	unsigned int m_PromptPos;
        unsigned int m_InsertPos;

	void TryEval();
        void PrintPrompt();
        void HistoryPrev();
        void HistoryNext();
        void HistoryShow(string what);
        
        // line history
        // temporary (?) solution, it would have been nicer to implement
        // the i/o port correctly and use readline module
        deque<string> m_History;
        deque<string>::iterator m_HistoryIter;
        // time to lear c++, artm :-/
        bool m_HistoryNavStarted;
        string m_HistoryPresent;
};

#endif
