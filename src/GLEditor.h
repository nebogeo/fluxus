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

#include <string>

using namespace std;

class GLEditor
{
public:
	GLEditor();
	~GLEditor();
	
	void Render();
	void Handle(int button, int key, int special, int state, int x, int y);
	void Reshape(unsigned int w,unsigned int h);

	string GetText();
	void SetText(const string& s) { m_Text=s; ProcessTabs(); }

	float m_PosX,m_PosY,m_PosZ;
	float m_RotX,m_RotY,m_DisY;

private:

	void DrawCharBlock();
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
	string m_CopyBuffer;
	unsigned int m_Position;
	unsigned int m_HighlightStart;
	unsigned int m_HighlightEnd;
	unsigned int m_DesiredXPos;
	bool m_Selection;
	bool m_ShiftState;
	bool m_CtrlState;
	float m_CursorWidth;
	int m_ParenthesesHighlight;
	string m_OpenChars;
	string m_CloseChars;

	int m_Width;
	int m_Height;
};
