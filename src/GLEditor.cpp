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

#include <GL/glut.h>
#include <iostream>
#include "GLEditor.h"

GLEditor::GLEditor():
m_Position(0),
m_HighlightStart(0),
m_HighlightEnd(0),
m_Selection(false),
m_ShiftState(false)
{ 
	m_Text="(define (tree d)\n\
	(push)\n\
	(rotate (vector 0 30 0))\n\
	(translate (vector 0 0.6 0))\n\
	(scale (vector 0.8 0.8 0.8))\n\
	(push)\n\
	(scale (vector 0.2 1 0.2))\n\
	(draw_cube)\n\
	(pop)\n\
	(if (eq? 0 d)\n\
		1\n\
		(begin	(rotate (vector 0 0 45))\n\
			(tree (- d 1))\n\
			(rotate (vector 0 0 -90))\n\
			(tree (- d 1))))\n\
	(pop))\n\
\n\
(show_axis 1)\n\
(clear)\n\
\n\
(colour (vector 0.5 0.5 0.5))\n\
(define (loop) (tree 8))\n\
\n\
(engine_callback \"(loop)\")"; 

/*m_Text="(clear)\n\
(show_axis 1)\n\
(colour (vector 1 1 1))\n\
(build_cube)\n";*/

	ProcessTabs();
}

GLEditor::~GLEditor() 
{
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
	glDisable(GL_CULL_FACE);
	glLoadIdentity();
	glTranslatef(-4,3,-5);
	glColor4f(0.7,0.7,0.7,1);
	glScalef(0.001,0.001,0.001);
	
	glTranslatef(m_PosX,m_PosY,m_DisY);
	glRotatef(-m_RotY,0,1,0);
	glRotatef(-m_RotX,1,0,0);
	
	glPushMatrix();
	float ypos=0;
	float width;
	float cursorwidth=glutStrokeWidth(GLUT_STROKE_MONO_ROMAN, ' ')+1;
	for (unsigned int n=0; n<m_Text.size(); n++)
	{
		width=glutStrokeWidth(GLUT_STROKE_MONO_ROMAN,m_Text[n]);
		if (m_Text[n]=='\n') width=cursorwidth;
		
		if (m_Position==n) // draw cursor
		{ 
			glColor4f(1,0,0,0.5);
			glBegin(GL_QUADS);
			glVertex3f(0,-30,0);
			glVertex3f(0,130,0);
			glVertex3f(cursorwidth,130,0);
			glVertex3f(cursorwidth,-30,0);				
			glEnd();
			glColor4f(0.7,0.7,0.7,1);
		}
		
		if (m_Selection && n>=m_HighlightStart && n<m_HighlightEnd)
		{ 
			glColor4f(0,1,0,0.5);
			glBegin(GL_QUADS);
			glVertex3f(0,-30,0);
			glVertex3f(0,130,0);
			glVertex3f(width,130,0);
			glVertex3f(width,-30,0);				
			glEnd();
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
			glutStrokeCharacter(GLUT_STROKE_MONO_ROMAN,m_Text[n]);
		}
	}
	glPopMatrix();
	glPopMatrix();
	glEnable(GL_LIGHTING);
	glEnable(GL_DEPTH_TEST);
	glEnable(GL_CULL_FACE);
}

void GLEditor::Handle(int button, int key, int special, int state, int x, int y)
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
				m_DesiredXPos=OffsetToCurrentLineStart(); 
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
				unsigned int previouslinelength=PreviousLineLength(m_Position);
				if (previouslinelength<m_DesiredXPos) m_Position=LineStart(m_Position)-1; // end of previous
				else m_Position=LineStart(LineStart(m_Position)-1)+m_DesiredXPos; // start of previous+offset	
			}
			break;
			case GLUT_KEY_DOWN: 
			{
				unsigned int nextlinelength=NextLineLength(m_Position);
				if (nextlinelength<m_DesiredXPos) m_Position=LineEnd(LineEnd(m_Position)+1); // end of next
				else m_Position=LineStart(LineEnd(m_Position)+1)+m_DesiredXPos; // start of next+offset
			}
			break;
		}
	}	
		
	if (glutGetModifiers()&GLUT_ACTIVE_CTRL)
	{
		switch (key)
		{
			case 24: // cut
				m_CopyBuffer=m_Text.substr(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
				m_Text.erase(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
				m_Selection=false;
			break;
			case 3: // copy
				m_CopyBuffer=m_Text.substr(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
			break;
			case 22: // paste
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
				case 127: m_Text.erase(m_Position,1); break; // delete
				case 8: // backspace
				{
					if (m_Selection)
					{
						m_Text.erase(m_HighlightStart,m_HighlightEnd-m_HighlightStart); 
						m_Position-=m_HighlightEnd-m_HighlightStart;
					}
					else
					{
						m_Text.erase(m_Position-1,1); 
						m_Position--; 
					}
				}
				break;
				case 9: // tab
				{
					m_Text.insert(m_Position,"    ");
					m_Position+=4;
				}
				break;
				case 13: key='\n'; // fallthrough (replacement of newline)
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
	
	if (!m_ShiftState && glutGetModifiers()&GLUT_ACTIVE_SHIFT)
	{ 
		m_HighlightStart=startpos;
		m_ShiftState=true;
		m_Selection=true;
	}
	
	if (m_ShiftState && !glutGetModifiers()&GLUT_ACTIVE_SHIFT)
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
