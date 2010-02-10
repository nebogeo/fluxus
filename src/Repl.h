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
#include "GLEditor.h"
#include "Interpreter.h"

namespace fluxus 
{

class Repl : public GLEditor
{
public:
	Repl();
	virtual ~Repl() {}
	
	virtual void Handle(int button, int key, int special, int state, 
			    int x, int y, int mod);
	
	void Print(const wstring &what);
	void Print(Scheme_Object *obj);

protected:
	static wstring m_Prompt;
	static wstring m_Banner;
	static unsigned int MAX_LINE_LENGTH;
	
	unsigned int m_PromptPos;
	unsigned int m_InsertPos;

	bool TryEval();
	void PrintPrompt();
	void HistoryPrev();
	void HistoryNext();
	void HistoryClear();
	void HistoryShow(wstring what);
	void EnsureCursorVisible();

	// line history
	deque<wstring> 			m_History;
	deque<wstring>::iterator m_HistoryIter;
	bool 					m_HistoryNavStarted;
	wstring 					m_HistoryPresent;
	unsigned int			m_LinePos;
};

}

#endif
