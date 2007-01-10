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

#ifndef _GL_EDITOR_H_
#define _GL_EDITOR_H_

#include <string>

#ifndef __APPLE__
#define GLEDITOR_DELETE 127
#define GLEDITOR_BACKSPACE 8
#else
#define GLEDITOR_DELETE 8
#define GLEDITOR_BACKSPACE 127
#endif

#define GLEDITOR_TAB 9
#define GLEDITOR_RETURN 13
#define GLEDITOR_CUT 24 
#define GLEDITOR_COPY 3 
#define GLEDITOR_PASTE 22 

using namespace std;

namespace fluxus 
{

class GLEditor
{
public:
	GLEditor();
	virtual ~GLEditor();
	
	void Render();
	virtual void Handle(int button, int key, int special, int state, int x, int y, int mod);
	void Reshape(unsigned int w,unsigned int h);

	string GetText();
	string GetAllText() { return m_Text; }
	void SetText(const string& s) { m_Text=s; ProcessTabs(); }
	void Reset();
	
	float m_PosX,m_PosY,m_PosZ;
	float m_RotX,m_RotY,m_DisY;
	float m_TextWidth;
	float m_TextColourRed;
	float m_TextColourGreen;
	float m_TextColourBlue;
	
protected:

	void DrawCharBlock();
	void DrawCursor();
	void ProcessTabs();
	int OffsetToCurrentLineStart();
	int NextLineLength(int pos);
	int PreviousLineLength(int pos);
	int LineLength(int pos);
	unsigned int LineStart(int pos);
	unsigned int LineEnd(int pos);
	void ParseOpenParentheses(int pos, int type);
	void ParseCloseParentheses(int pos, int type);
	
	string m_Text;
	static string m_CopyBuffer;
	unsigned int m_Position;
	unsigned int m_HighlightStart;
	unsigned int m_HighlightEnd;
	unsigned int m_DesiredXPos;
	bool m_Selection;
	bool m_ShiftState;
	bool m_CtrlState;
	float m_CursorWidth;
	float m_CharWidth;
	int m_ParenthesesHighlight[2];
	string m_OpenChars;
	string m_CloseChars;
	unsigned int m_VisibleLines;
	unsigned int m_TopTextPosition;
	unsigned int m_BottomTextPosition;
	unsigned int m_LineCount;
	
	int m_Width;
	int m_Height;
};

} // namespace fluxus

#endif
