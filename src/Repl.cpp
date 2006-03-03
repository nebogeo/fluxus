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
#include <guile/gh.h>
#ifndef __APPLE__
#include <GL/glut.h>
#else
#include <GLUT/glut.h>
#endif
#include "Repl.h"

string Repl::m_Banner = string("Welcome to fluxus\n");
string Repl::m_Prompt = string("fluxus> ");

SCM ErrorHandler (void *handler_data, SCM tag, SCM args);

Repl::Repl() : m_InsertPos(0), m_History(), m_HistoryNavStarted(false)
{
	Print(m_Banner);
        PrintPrompt();
}

// FIXME: wrap long lines
void Repl::Print(string what)
{
        unsigned int l = what.length();

        m_Text.insert(m_InsertPos, what);
        
        m_Position += l;
        m_PromptPos += l;
        m_InsertPos += l;

        EnsureCursorVisible();

        cout << what;
}

void Repl::PrintPrompt()
{
        unsigned int where = m_Text.length();
        m_InsertPos = m_Text.length();
        if (m_Text[m_InsertPos-1]!='\n') {
                m_Text += '\n';
                m_InsertPos++;
        }
        m_Text += m_Prompt;
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
        
	if (key == GLEDITOR_RETURN && !TryEval())
                return;

        GLEditor::Handle(button, key, special, state, 
                         x, y, mod);
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

void Repl::HistoryShow(string what)
{
        m_Text.resize(m_PromptPos,0);
        m_Text += what;
        m_Position = m_Text.length();
}

// FIXME: skip parens in strings and comments
bool Balanced(string s)
{
	int balance = 0;
	for (string::iterator i = s.begin(); i != s.end(); i++) {
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

inline bool Empty(string s) {
        const string ws=" \t\n\r";
	for (string::iterator i = s.begin(); i != s.end(); i++) {
                if (ws.find(*i) == string::npos) {
                        return false;
                }
        }
        return true;
}

bool Repl::TryEval()
{
	string defun = m_Text.substr(m_PromptPos);
	
	if (!Balanced(defun)/* || (m_Text.substr(m_Position).find(')')!=string::npos)*/)
                return true;

        if (!Empty(defun)) {
                m_InsertPos = m_Text.length();
                Print("\n");
                SCM res0 = scm_internal_catch(SCM_BOOL_T,
                                              (scm_t_catch_body)scm_c_eval_string,
                                              (void*)(defun.c_str()),
                                              ErrorHandler,
                                              (void*)"fluxus");

                if (defun[defun.length()-1] == '\n')
                        defun.resize(defun.length()-1,0); 
                m_History.push_back(defun);
                m_HistoryNavStarted = false;
        
                if (res0 != SCM_UNDEFINED && res0 != SCM_UNSPECIFIED) {
                        Print(res0);
                        Print("\n");
                }
        }
	PrintPrompt();
	
	return false;
}

void Repl::EnsureCursorVisible()
{
        unsigned int i;
        unsigned int curVisLine = 0;
        // use m_VisibleLines, m_TopTextPosition;
        for (i = m_Position; i>m_TopTextPosition; i--) 
                if (m_Text[i] == '\n') 
                        curVisLine++;
        while (curVisLine >= m_VisibleLines)
                if (m_Text[m_TopTextPosition++] == '\n')
                        curVisLine--;
}
