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

#ifndef __APPLE__
#include <GL/glut.h>
#else
#include <GLUT/glut.h>
#endif
#include <iostream>
#include "GLEditor.h"

#define GLEDITOR_DELETE 127
#define GLEDITOR_BACKSPACE 8
#define GLEDITOR_TAB 9
#define GLEDITOR_RETURN 13
#define GLEDITOR_CUT 24 
#define GLEDITOR_COPY 3 
#define GLEDITOR_PASTE 22 

// static so we share between workspaces
string GLEditor::m_CopyBuffer;

GLEditor::GLEditor():
m_PosX(0),
m_PosY(0),
m_PosZ(0),
m_RotX(0),
m_RotY(0),
m_DisY(-10),
m_TextWidth(1),
m_TextColourRed(1),
m_TextColourGreen(1),
m_TextColourBlue(1),
m_Position(0),
m_HighlightStart(0),
m_HighlightEnd(0),
m_Selection(false),
m_ShiftState(false),
m_OpenChars("([<{"),
m_CloseChars(")]>}"),
m_VisibleLines(40),
m_TopTextPosition(0),
m_BottomTextPosition(0)
{ 
	// mono font - yay!
	m_CursorWidth=glutStrokeWidth(GLUT_STROKE_MONO_ROMAN, ' ')+1;
}

GLEditor::~GLEditor() 
{
}

void GLEditor::Reset()
{
	m_PosX=m_PosY=m_PosZ=m_RotX=m_RotY=0;
	m_DisY=-10;
	m_TextWidth=1;
	m_TextColourRed=1;
	m_TextColourGreen=1;
	m_TextColourBlue=1;
}

void GLEditor::Reshape(unsigned int w,unsigned int h)
{
	m_Width=w;
	m_Height=h;
}

string GLEditor::GetText() 
{
	if (m_Selection) return m_Text.substr(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
	return m_Text;
}

void GLEditor::DrawCharBlock()
{		
	glBegin(GL_QUADS);
	glVertex3f(m_CursorWidth,-30,0);				
	glVertex3f(m_CursorWidth,130,0);
	glVertex3f(0,130,0);
	glVertex3f(0,-30,0);
	glEnd();
}

void GLEditor::Render()
{
	/*glViewport(0,0,m_Width,m_Height);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(40.0, (GLfloat) m_Width/(GLfloat) m_Height, 0.1, 10000.0);
	glMatrixMode(GL_MODELVIEW);*/

	glPushMatrix();
	glDisable(GL_LIGHTING);
	glDisable(GL_DEPTH_TEST);
   	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);	
	glLineWidth(m_TextWidth);
	glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
	glLoadIdentity();
	glTranslatef(-4,3,-5);
	glColor4f(0.7,0.7,0.7,1);
	glScalef(0.001,0.001,0.001);
	
	glTranslatef(m_PosX,m_PosY,m_DisY);
	glRotatef(-m_RotY,0,1,0);
	glRotatef(-m_RotX,1,0,0);
	
	glPushMatrix();
	
	m_ParenthesesHighlight=-1;
	
	int type=0;
	for (string::iterator i=m_OpenChars.begin(); i!=m_OpenChars.end(); i++)
	{
		if (m_Text[m_Position]==*i) ParseOpenParentheses(m_Position,type);
		type++;
	}
		
	type=0;	
	for (string::iterator i=m_CloseChars.begin(); i!=m_CloseChars.end(); i++)
	{
		if (m_Text[m_Position]==*i) ParseCloseParentheses(m_Position,type);
		type++;
	}
	
	float ypos=0;
	float width;
	bool drawncursor=false;
	
	unsigned int n=m_TopTextPosition;
	unsigned int linecount=0;
	while (n<m_Text.size() && linecount<m_VisibleLines)
	{
		width=glutStrokeWidth(GLUT_STROKE_MONO_ROMAN,m_Text[n]);
		if (m_Text[n]=='\n') 
		{
			width=m_CursorWidth;
			linecount++;
		}
		
		if (m_Position==n) // draw cursor
		{ 
			drawncursor=true;
			glColor4f(1,0,0,0.5);
			DrawCharBlock();
			glColor4f(0.7,0.7,0.7,1);
		}
		
		if (m_ParenthesesHighlight==(int)n) // draw parentheses highlight
		{ 
			glColor4f(1,0,0,0.5);
			DrawCharBlock();
			glColor4f(0.7,0.7,0.7,1);
		}
		
		if (m_Selection && n>=m_HighlightStart && n<m_HighlightEnd)
		{ 
			glColor4f(0,1,0,0.5);
			DrawCharBlock();
			glColor4f(0.7,0.7,0.7,1);
		}	
		
		if(m_Text[n]=='\n')
		{
			glPopMatrix();
			glPushMatrix();
			ypos-=150;
			glTranslatef(0,ypos,0);
		}
		else 
		{
			glColor3f(m_TextColourRed,m_TextColourGreen,m_TextColourBlue);
			glutStrokeCharacter(GLUT_STROKE_MONO_ROMAN,m_Text[n]);
		}
		
		n++;
	}
	
	m_BottomTextPosition=n;
	
	// draw cursor if we have no text, or if we're at the end of the buffer
	if (!drawncursor)
	{
		glColor4f(1,0,0,0.5);
		DrawCharBlock();
		glColor4f(0.7,0.7,0.7,1);
	}
	
	glPopMatrix();
	glPopMatrix();
	glEnable(GL_LIGHTING);
	glEnable(GL_DEPTH_TEST);
}

void GLEditor::Handle(int button, int key, int special, int state, int x, int y, int mod)
{	
	unsigned int startpos=m_Position;
	
	if (special!=0)
	{
		switch(special)
		{
			case GLUT_KEY_RIGHT: 
			{
				m_Position++; 
				m_DesiredXPos=OffsetToCurrentLineStart(); 
			}
			break;
			case GLUT_KEY_LEFT: 
			{
				if (m_Position>0) m_Position--;  
				m_DesiredXPos=OffsetToCurrentLineStart(); 
			}
			break;
			case GLUT_KEY_END: 
			{
				m_Position=LineEnd(m_Position);
				m_DesiredXPos=OffsetToCurrentLineStart()+1; 
			}
			break;
			case GLUT_KEY_HOME: 
			{
				m_Position=LineStart(m_Position); 
				m_DesiredXPos=OffsetToCurrentLineStart(); 
			}
			break;
			case GLUT_KEY_UP: 
			{
				if ((int)m_Position>LineLength(0)) // if we're not on the first line
				{
					unsigned int previouslinelength=PreviousLineLength(m_Position);
					if (previouslinelength<m_DesiredXPos) m_Position=LineStart(m_Position)-1; // end of previous
					else m_Position=LineStart(LineStart(m_Position)-1)+m_DesiredXPos; // start of previous+offset	
					
					if (m_Position<m_TopTextPosition) m_TopTextPosition=LineStart(m_Position);
				}
			}
			break;
			case GLUT_KEY_DOWN: 
			{
				if (m_Position+LineLength(m_Position)<m_Text.size()) // if we're not on the last line
				{
					unsigned int nextlinelength=NextLineLength(m_Position);
					if (nextlinelength<m_DesiredXPos) m_Position=LineEnd(LineEnd(m_Position)+1); // end of next
					else m_Position=LineStart(LineEnd(m_Position)+1)+m_DesiredXPos; // start of next+offset

					if (m_Position>m_BottomTextPosition) m_TopTextPosition=LineEnd(m_TopTextPosition)+1;
				}
			}
			break;
			case GLUT_KEY_PAGE_UP: 
			{
				m_Position=0;
				m_TopTextPosition=0;
			}
			break;
		}
	}	
		
	if (mod&GLUT_ACTIVE_CTRL)
	{
		switch (key)
		{
			case GLEDITOR_CUT: // cut
				m_CopyBuffer=m_Text.substr(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
				m_Text.erase(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
				m_Selection=false;
			break;
			case GLEDITOR_COPY: // copy
				if (m_Selection && m_HighlightEnd!=m_HighlightStart) 
				{
					m_CopyBuffer=m_Text.substr(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
				}
			break;
			case GLEDITOR_PASTE: // paste
				m_Text.insert(m_Position,m_CopyBuffer);
				m_Selection=false;
				m_Position+=m_CopyBuffer.size();
			break;
			default: break;
		}
	}
	else
	{
		if (key!=0)
		{	
			switch(key)
			{
				case GLEDITOR_DELETE: m_Text.erase(m_Position,1); break; // delete
				case GLEDITOR_BACKSPACE: // backspace
				{
					if (!m_Text.empty() && m_Position!=0)
					{
						if (m_Selection)
						{
							m_Text.erase(m_HighlightStart,m_HighlightEnd-m_HighlightStart); 
							m_Position-=m_HighlightEnd-m_HighlightStart;						
							m_Selection=false;
						}
						else
						{
							m_Text.erase(m_Position-1,1); 
							m_Position--; 
						}
					}
				}
				break;
				case GLEDITOR_TAB: // tab
				{
					m_Text.insert(m_Position,"    ");
					m_Position+=4;
				}
				break;
				case 172: break; // ignore ¬
				case 163: break; // ignore £
				case GLEDITOR_RETURN: key='\n'; // fallthrough (replacement of newline)
				default:
					char temp[2];
					temp[0]=(char)key;
					temp[1]='\0';
					m_Text.insert(m_Position,string(temp));
					m_Position++;
				break;
			}
		}
	}
			
	if (m_Position<0) m_Position=0;
	if (m_Position>m_Text.size()) m_Position=m_Text.size()-1;
	
	if (key==0 && !m_ShiftState && mod&GLUT_ACTIVE_SHIFT)
	{ 
		m_HighlightStart=startpos;
		m_ShiftState=true;
		m_Selection=true;
	}
	
	if (key==0 && special!=GLUT_KEY_F5 && m_ShiftState && !mod&GLUT_ACTIVE_SHIFT)
	{ 
		m_ShiftState=false;	
		m_Selection=false;
	}
	
	if (m_ShiftState) m_HighlightEnd=m_Position;	
	
	//cerr<<"----------------"<<endl;
	//cerr<<PreviousLineLength(m_Position)<<endl;
	//cerr<<LineLength(m_Position)<<endl;
	//cerr<<NextLineLength(m_Position)<<endl;
	//cerr<<OffsetToCurrentLineStart()<<endl;
	//cerr<<"----------------"<<endl;
	
}

void GLEditor::ProcessTabs()
{
	unsigned int pos=m_Text.find("\t",0);
	while (pos!=string::npos)
	{
		m_Text.erase(pos,1);
		m_Text.insert(pos,"    ");
		pos=m_Text.find("\t",pos);
	}
}
	
	
int GLEditor::OffsetToCurrentLineStart()
{
	return m_Position-LineStart(m_Position);
}

int GLEditor::NextLineLength(int pos)
{
	unsigned int nextlinestart=m_Text.find("\n",m_Position);
	if (nextlinestart!=string::npos)
	{	
		return LineLength(nextlinestart+1);
	}
	return 0;

}

int GLEditor::PreviousLineLength(int pos)
{
	unsigned int previouslineend=string::npos;
	if (pos>0) previouslineend=m_Text.rfind("\n",pos-1);
	if (previouslineend!=string::npos)
	{	
		return LineLength(previouslineend);
	}
	return 0;
}

int GLEditor::LineLength(int pos)
{
	unsigned int linestart=LineStart(pos);
	unsigned int lineend=LineEnd(pos);
		
	// if we're not on the last line
	if (lineend!=string::npos) return lineend-linestart;
	
	// if we are, use the total length position
	return m_Text.size()-linestart;
}

unsigned int GLEditor::LineStart(int pos)
{
	unsigned int linestart=0;
	
	// if it's not the first char, take one off in case we're over the newline
	if (pos>0) linestart=m_Text.rfind("\n",pos-1);
	else linestart=m_Text.rfind("\n",pos);
	
	if (linestart!=string::npos) linestart++; // move the start off the newline
	else linestart=0; // if are on the first line, set the start to 0
	
	return linestart;
}

unsigned int GLEditor::LineEnd(int pos)
{
	return m_Text.find("\n",pos);
}

void GLEditor::ParseOpenParentheses(int pos, int type)
{
	// looking for a close, so search forward
	int stack=0;
	pos++;
	while(stack!=-1 && pos<(int)m_Text.size())
	{
		if (m_Text[pos]==m_OpenChars[type]) stack++;
		if (m_Text[pos]==m_CloseChars[type]) stack--;
		pos++;
	}
	if (stack==-1)
	{
		m_ParenthesesHighlight=pos-1;
	}
}

void GLEditor::ParseCloseParentheses(int pos, int type)
{
	// looking for a open, so search backward
	int stack=0;
	pos--;
	while(stack!=-1 && pos>0)
	{
		if (m_Text[pos]==m_CloseChars[type]) stack++;
		if (m_Text[pos]==m_OpenChars[type]) stack--;
		pos--;
	}
	if (stack==-1)
	{
		m_ParenthesesHighlight=pos+1;
	}	
}
