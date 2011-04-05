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

#include <iostream>
#include <string>
#ifndef __APPLE__
#include <GL/glut.h>
#else
#include <GLUT/glut.h>
#endif
#include "Repl.h"
#include "Unicode.h"
#include <wchar.h>

using namespace fluxus;

wstring Repl::m_Banner = wstring(L"Welcome to fluxus.\nType (help) for info.\n");
wstring Repl::m_Prompt = wstring(L"fluxus> ");
unsigned int Repl::MAX_LINE_LENGTH = 80;

Repl::Repl() : 
m_InsertPos(0), 
m_History(), 
m_HistoryNavStarted(false),
m_LinePos(0)
{
	Print(m_Banner);
	PrintPrompt();
}

// FIXME: wrap long lines
// how? break before a "(" I would say -Evan

void Repl::Print(const wstring &what)
{
	int l = what.length();
	
	int slop = l;
	int end = slop;
	int start = 0;
	
	wstring to_print;
	
	for (wstring::const_iterator i=what.begin(); i!=what.end(); ++i)
	{
		m_LinePos++;
		if (*i==L'\n') m_LinePos=0;
		if (m_LinePos>=MAX_LINE_LENGTH)
		{
			to_print+=L"\n";
			m_LinePos=0;
		}
		to_print+=*i;
	}
	
	m_Text.insert(m_InsertPos, to_print);
		
	m_Position += to_print.length();
	m_PromptPos += to_print.length();
	m_InsertPos += to_print.length();
	
	#ifdef WIN32
	cout << wstring_to_string(to_print);
	#else
	wcout << to_print;
	#endif
	EnsureCursorVisible();
}

void Repl::PrintPrompt()
{
	m_InsertPos = m_Text.length();
	if (m_Text[m_InsertPos-1]!=L'\n') {
		m_Text += L'\n';
		m_InsertPos++;
	}
	m_Text += m_Prompt;
	m_Position = m_PromptPos = m_Text.length();
}

void Repl::Print(Scheme_Object *obj)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, obj);
	MZ_GC_REG();
	intptr_t length=0;
	if (obj)
	{
		if (obj == scheme_multiple_values)
		{
			for (int i=0; i<scheme_multiple_count; i++)
			{
				char *str = scheme_display_to_string(scheme_multiple_array[i], &length);
				Print(string_to_wstring(string(str)));
				if (i!=scheme_multiple_count-1) Print(L"\n");
			}
		}
		else
		{
			char *str = scheme_display_to_string(obj, &length);
			Print(string_to_wstring(string(str)));
		}
	}
	MZ_GC_UNREG();
}

void Repl::Handle(int button, int key, int special, int state,
		  int x, int y, int mod)
{
	
	if (mod&GLUT_ACTIVE_CTRL && state)
	{
		switch(key)
		{
			case 3: HistoryClear(); break;
		}
	}
	
	if (key!=0) {
		if ((m_Position <= m_PromptPos && key == GLEDITOR_BACKSPACE) ||
			(m_Position < m_PromptPos && key == GLEDITOR_DELETE) ||
			((m_Position < m_PromptPos ||
			m_HighlightStart < m_PromptPos ||
			m_HighlightEnd < m_PromptPos)
			&& key == GLEDITOR_CUT))
			return;

		if (m_Position < m_PromptPos && key != GLEDITOR_COPY) 
			m_Position = m_Text.length();
	}
	
	if (special != 0) {
		if (m_Position >= m_PromptPos) {
			switch(special)
			{
			case GLUT_KEY_UP:
    			HistoryPrev();
    			return;
			case GLUT_KEY_DOWN:
    			HistoryNext();
    			return;
			case GLUT_KEY_END:
    			m_Position = m_Text.length();
    			return;
			case GLUT_KEY_HOME:
    			m_Position = m_PromptPos;
    			return;
			}
		}
	}
		
	EnsureCursorVisible();

	if (key == GLEDITOR_RETURN)
	{
		m_Position = m_Text.length();
        if (!TryEval()) return;
    }

    GLEditor::Handle(button, key, special, state, x, y, mod);
}

void Repl::HistoryClear()
{
	m_HistoryNavStarted=false;
	m_History.clear();
	m_InsertPos = 0;
	m_HistoryIter = m_History.end();
	m_HistoryPresent = m_Text.substr(m_PromptPos);
}


void Repl::HistoryPrev()
{
	if (!m_HistoryNavStarted) {
		m_HistoryIter = m_History.end();
		m_HistoryNavStarted = true;
	}

	if (m_HistoryIter == m_History.end())
		m_HistoryPresent = m_Text.substr(m_PromptPos);

	if (m_HistoryIter == m_History.begin())
		return;

	m_HistoryIter--;
	HistoryShow(*m_HistoryIter);
}

void Repl::HistoryNext()
{
	if (!m_HistoryNavStarted || (m_HistoryIter == m_History.end()))
		return;

	m_HistoryIter++;
	HistoryShow((m_HistoryIter == m_History.end()) ?
		m_HistoryPresent : *m_HistoryIter);
}

void Repl::HistoryShow(wstring what)
{
	m_Text.resize(m_PromptPos,0);
	m_Text += what;
	m_Position = m_Text.length();
}

// FIXME: skip parens in wstrings and comments
bool Balanced(wstring s)
{
	int balance = 0;
	for (wstring::iterator i = s.begin(); i != s.end(); i++) {
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

inline bool Empty(wstring s) {
    const wstring ws=L" \t\n\r";
	for (wstring::iterator i = s.begin(); i != s.end(); i++) {
                if (ws.find(*i) == wstring::npos) {
                        return false;
                }
        }
        return true;
}

bool Repl::TryEval()
{
	Scheme_Object *out=NULL;
	MZ_GC_DECL_REG(1);

	MZ_GC_VAR_IN_REG(0, out);
    MZ_GC_REG();
	
	if (m_PromptPos<m_Text.size())
	{
		wstring defun = m_Text.substr(m_PromptPos);

		if (!Balanced(defun)/* || (m_Text.substr(m_Position).find(')')!=wstring::npos)*/)
		{
			MZ_GC_UNREG();
			return true;
		}
		
		if (!Empty(defun)) {
			m_InsertPos = m_Text.length();
			Print(L"\n");
            
            #ifdef WIN32
            // not sure why this is required on mingw, and it scares me
            // it could well be an issue in utf-32 vs utf-16
            string t = wstring_to_string(defun);
			Interpreter::Interpret(string_to_wstring(t),&out);
            #else
            Interpreter::Interpret(defun,&out);
            #endif

			if (defun[defun.length()-1] == L'\n')
        			defun.resize(defun.length()-1,0); 
			m_History.push_back(defun);
			m_HistoryNavStarted = false;

			if (out != NULL && out != scheme_void) 
			{
        		Print(out);
        		Print(L"\n");
			}
		}
	}
	PrintPrompt();
	
	MZ_GC_UNREG();
	
	return false;
}

void Repl::EnsureCursorVisible()
{
        unsigned int i;
        unsigned int curVisLine = 0;
        // use m_VisibleLines, m_TopTextPosition;
        for (i = m_Position; i>m_TopTextPosition; i--) 
                if (m_Text[i] == L'\n') 
                        curVisLine++;
        while (curVisLine >= m_VisibleLines)
                if (m_Text[m_TopTextPosition++] == L'\n')
                        curVisLine--;				
}

