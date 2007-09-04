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
#include <vector>
#include <sys/time.h>
#include "GLEditor.h"
#include "PolyGlyph.h"
#include "assert.h"

using namespace fluxus;

// static so we share between workspaces
string GLEditor::m_CopyBuffer;
float GLEditor::m_TextWidth(1);
float GLEditor::m_TextColourRed(1);
float GLEditor::m_TextColourGreen(1);
float GLEditor::m_TextColourBlue(1);
bool GLEditor::m_DoAutoFocus(true);
float GLEditor::m_AutoFocusWidth(70000.0f);
float GLEditor::m_AutoFocusHeight(50000.0f);
float GLEditor::m_AutoFocusError(5000.0f);
float GLEditor::m_AutoFocusDrift(1.0);
float GLEditor::m_AutoFocusScaleDrift(0.3);
float GLEditor::m_AutoFocusMinScale(0.5);
float GLEditor::m_AutoFocusMaxScale(5.0);

PolyGlyph* GLEditor::m_PolyGlyph = NULL;

GLEditor::GLEditor():
m_PosX(0),
m_PosY(0),
m_Scale(1),
m_Position(0),
m_HighlightStart(0),
m_HighlightEnd(0),
m_DesiredXPos(0),
m_Selection(false),
m_ShiftState(false),
m_OpenChars("([<{"),
m_CloseChars(")]>}"),
m_VisibleLines(40),
m_VisibleColumns(60),
m_LeftTextPosition(0),
m_TopTextPosition(0),
m_BottomTextPosition(0),
m_LineCount(0),
m_Delta(0.0)
{ 
	assert(m_PolyGlyph!=NULL);
	
	m_CharWidth=StrokeWidth('#')+1;
	m_CharHeight=m_PolyGlyph->CharacterHeight('#');
	m_CursorWidth=m_CharWidth/3.0f;
	m_Time.tv_sec=0;
	m_Time.tv_usec=0;	
}

void GLEditor::InitFont(const string &ttf)
{
	m_PolyGlyph = new PolyGlyph(ttf);
}

void GLEditor::InitAutoFocus(bool doit, float width, float height, float error, float drift, 
	float scale_drift, float minscale, float maxscale)
{
	m_DoAutoFocus = doit;
	m_AutoFocusWidth = width;
	m_AutoFocusHeight = height;
	m_AutoFocusError = error;
	m_AutoFocusDrift = drift;
	m_AutoFocusScaleDrift = scale_drift;
	m_AutoFocusMinScale = minscale;
	m_AutoFocusMaxScale = maxscale;
}

GLEditor::~GLEditor() 
{
}

void GLEditor::Reset()
{
	m_PosX=m_PosY=0;
	m_Scale=1;
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
	
void GLEditor::StrokeCharacter(wchar_t c)
{
	m_PolyGlyph->Render(c);
}

float GLEditor::StrokeWidth(wchar_t c)
{
	return m_PolyGlyph->CharacterWidth(c);
}

string GLEditor::GetText() 
{
	if (m_Selection) return m_Text.substr(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
	return m_Text;
}

void GLEditor::DrawCharBlock()
{		
	glBegin(GL_QUADS);
	glVertex3f(m_CharWidth,0,0);				
	glVertex3f(m_CharWidth,m_CharHeight,0);
	glVertex3f(0,m_CharHeight,0);
	glVertex3f(0,0,0);
	glEnd();
}

void GLEditor::DrawCursor()
{	
	float half = m_CursorWidth/2.0f;
	glBegin(GL_QUADS);
	glVertex2f(half,0);				
	glVertex2f(half,m_CharHeight);
	glVertex2f(-half,m_CharHeight);
	glVertex2f(-half,0);
	glEnd();
}

void GLEditor::BBExpand(float x, float y)
{
	if (x<m_BBMinX) m_BBMinX=x;
	if (x>m_BBMaxX) m_BBMaxX=x;
	if (y<m_BBMinY) m_BBMinY=y;
	if (y>m_BBMaxY) m_BBMaxY=y;
}

void GLEditor::Render()
{
	glMatrixMode(GL_PROJECTION);
	glPushMatrix();
	glLoadIdentity();
	glOrtho(-100/2,100/2,-75/2,75/2,0,10);
	
	glMatrixMode(GL_MODELVIEW);
	glDisable(GL_TEXTURE_2D);
	
	glPushMatrix();
	glDisable(GL_LIGHTING);
	glDisable(GL_DEPTH_TEST);
   	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);	
	glLineWidth(m_TextWidth);
	glPolygonMode(GL_FRONT,GL_FILL);
	
	glLoadIdentity();
	glTranslatef(-45,0,0);
	glScalef(0.001f*m_Scale,0.001f*m_Scale,1); 
	glTranslatef(m_PosX,m_PosY,0);
	
	glPushMatrix();
	
	m_ParenthesesHighlight[0]=-1;
	m_ParenthesesHighlight[1]=-1;
	
	int type=0;
	for (string::iterator i=m_OpenChars.begin(); i!=m_OpenChars.end(); i++)
	{
		if (m_Text[m_Position]==*i) ParseOpenParentheses(m_Position,type);
		type++;
	}
		
	if (m_Position > 0) 
	{
		type=0;	
		for (string::iterator i=m_CloseChars.begin(); i!=m_CloseChars.end(); i++)
		{
			if (m_Text[m_Position-1]==*i) 
				ParseCloseParentheses(m_Position-1,type);
			type++;
		}
	}
	
	unsigned int xcount=0;
	float xpos=0;
	float ypos=0;
	float width;
	bool drawncursor=false;
	BBClear();
	
	unsigned int n=m_TopTextPosition;
	m_LineCount=0;
	
	while (n<m_Text.size() && m_LineCount<m_VisibleLines)
	{
		width=m_CharWidth; //\todo fix bounding box with non-mono fonts
		
		if (m_Text[n]=='\n') 
		{
			width=m_CursorWidth;
			m_LineCount++;
		}
		
		if (m_Position==n) // draw cursor
		{ 
			if (xcount>m_VisibleColumns) m_LeftTextPosition=xcount-m_VisibleColumns;
			else m_LeftTextPosition=0;
			
			glColor4f(1,1,0,0.5);
			DrawCursor();
			glColor4f(0.7,0.7,0.7,1);
			drawncursor=true;
		}
		
		if (m_ParenthesesHighlight[0]==(int)n ||
		    m_ParenthesesHighlight[1]==(int)n) // draw parentheses highlight
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
			BBExpand(xpos,ypos);
			xpos=0;
			xcount=0;
			ypos-=m_CharHeight;
			glTranslatef(0,ypos,0);
		}
		else 
		{
			if (xcount>=m_LeftTextPosition)
			{
				glColor3f(m_TextColourRed,m_TextColourGreen,m_TextColourBlue);
			
				if ((m_Text[n] & 0xC0) == 0xC0) // two byte utf8
				{
					wchar_t dst[1];
					mbstowcs(dst,&(m_Text[n]),1);
					StrokeCharacter(dst[0]);
					n++;		
				}
				else
				{
					StrokeCharacter(m_Text[n]);			
				}
				BBExpand(xpos,ypos);
				xpos+=width;
			}
			xcount++;
		}
		
		n++;
	}
	
	if (m_LineCount>=m_VisibleLines-1) m_BottomTextPosition=n;
	else m_BottomTextPosition=m_Text.size()+1;
	
	// draw cursor if we have no text, or if we're at the end of the buffer
	if (!drawncursor)
	{
		if (xcount>m_VisibleColumns) m_LeftTextPosition=xcount-m_VisibleColumns;
		else m_LeftTextPosition=0;
		
		glColor4f(1,1,0,0.5);
		DrawCursor();
		glColor4f(0.7,0.7,0.7,1);
	}

	if (m_DoAutoFocus)
	{
		m_PosY=m_PosY*(1-m_AutoFocusDrift*m_Delta) - (m_BBMinY+(m_BBMaxY-m_BBMinY)/2)*m_AutoFocusDrift*m_Delta;
	
		float boxwidth=(m_BBMaxX-m_BBMinX)*m_Scale;
		float boxheight=(m_BBMaxY-m_BBMinY)*m_Scale;

		if (boxwidth > m_AutoFocusWidth+m_AutoFocusError) m_Scale*=1-m_AutoFocusScaleDrift*m_Delta; 
		else if (boxwidth < m_AutoFocusWidth-m_AutoFocusError && 
				 boxheight < m_AutoFocusHeight-m_AutoFocusError) m_Scale*=1+m_AutoFocusScaleDrift*m_Delta;
		else if (boxheight > m_AutoFocusHeight+m_AutoFocusError) m_Scale*=1-m_AutoFocusScaleDrift*m_Delta; 
		else if (boxheight < m_AutoFocusHeight-m_AutoFocusError && 
		    	 boxwidth < m_AutoFocusWidth-m_AutoFocusError) m_Scale*=1+m_AutoFocusScaleDrift*m_Delta;
	}
	
	if (m_Scale>m_AutoFocusMaxScale) m_Scale=m_AutoFocusMaxScale; // clamp
	if (m_Scale<m_AutoFocusMinScale) m_Scale=m_AutoFocusMinScale; // clamp
	
	glPopMatrix();
	glPopMatrix();
	glEnable(GL_LIGHTING);
	glEnable(GL_DEPTH_TEST);

	glMatrixMode(GL_PROJECTION);
	glPopMatrix();
	glMatrixMode(GL_MODELVIEW);
	
	timeval ThisTime;
	// stop valgrind complaining
	ThisTime.tv_sec=0;
	ThisTime.tv_usec=0;
	
	gettimeofday(&ThisTime,NULL);
	m_Delta=(ThisTime.tv_sec-m_Time.tv_sec)+
			(ThisTime.tv_usec-m_Time.tv_usec)*0.000001f;
	m_Time=ThisTime;
	if (m_Delta>1.0f) m_Delta=0.000001f;
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
				if (!m_Text.empty()) m_Position++; 
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
				if ((int)LineStart(m_Position) > 0) // if we're not on the first line
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
				if (LineEnd(m_Position) < m_Text.size()) // if we're not on the last line
				{
					unsigned int nextlinelength=NextLineLength(m_Position);
					if (nextlinelength<m_DesiredXPos) m_Position=LineEnd(LineEnd(m_Position)+1); // end of next
					else m_Position=LineStart(LineEnd(m_Position)+1)+m_DesiredXPos; // start of next+offset
					if (m_Position>=m_BottomTextPosition) m_TopTextPosition=LineEnd(m_TopTextPosition)+1;
				}
			}
			break;
			case GLUT_KEY_PAGE_UP: 
			{
				for (unsigned int n=0; n<m_VisibleLines+1; n++)
				{
					int start=LineStart(m_Position);
					if (start>0)
					{
						m_Position=start-1;
					}
				}
				
				if(m_Position<m_TopTextPosition) 
				{
					m_TopTextPosition=LineStart(m_Position);
				}
			}
			break;
			case GLUT_KEY_PAGE_DOWN: 
			{
				for (unsigned int n=0; n<m_VisibleLines+1; n++)
				{
					m_Position=LineEnd(m_Position)+1;
				}
				
				if(m_Position>=m_BottomTextPosition) 
				{
					m_TopTextPosition=LineStart(m_Position);
				}
			}
			break; 
		}
	}	
		
		
	if (mod&GLUT_ACTIVE_CTRL)
	{	
		switch (key)
		{
			case GLEDITOR_CUT: // cut
				if (m_Selection) 
				{
					m_CopyBuffer=m_Text.substr(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
					m_Text.erase(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
					if (m_Position>=m_HighlightEnd) 
					{
						m_Position-=m_HighlightEnd-m_HighlightStart;
					}	
					m_Selection=false;
				}
			break;
			case GLEDITOR_COPY: // copy
				if (m_Selection) 
				{
					m_CopyBuffer=m_Text.substr(m_HighlightStart,m_HighlightEnd-m_HighlightStart);
				}
			break;
			case GLEDITOR_PASTE: // paste
				m_Text.insert(m_Position,m_CopyBuffer);
				m_Selection=false;
				m_Position+=m_CopyBuffer.size();
			break;
			/*case GLEDITOR_PLUS: // zoom in
				m_Scale*=1.1f;
			break;
			case GLEDITOR_MINUS: // zoom out
				m_Scale/=1.1f;
			break;*/
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
							if (m_Position>=m_HighlightEnd) 
							{
								m_Position-=m_HighlightEnd-m_HighlightStart;
							}						
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
				//case 172: break; // ignore ¬
				//case 163: break; // ignore £
				case 27: // esc - no GLUT_KEY_ESCAPE? 
				{
					// panic editor reset :)
				    m_Position=0;
				    m_TopTextPosition=0;
		    	}
				break;
				case GLEDITOR_RETURN: 
					key='\n'; // fallthrough (replacement of newline)
				default:
					char temp[2];
					temp[0]=(char)key;
					temp[1]='\0';
					m_Text.insert(m_Position,string(temp));
					m_Position++;
					if (key=='\n' && m_Position>=m_BottomTextPosition && m_LineCount+1>=m_VisibleLines) 
					{
						m_TopTextPosition=LineEnd(m_TopTextPosition)+1;
					}
				break;
			}
		}
	}
	
	if (m_Position<0) m_Position=0;
	if (m_Position>m_Text.size()) m_Position=m_Text.size();
	
	if (key==0 && !m_ShiftState && mod&GLUT_ACTIVE_SHIFT)
	{ 
		m_HighlightStart=startpos;
		m_HighlightEnd=startpos;
		m_ShiftState=true;
		m_Selection=true;
	}
	
	if (key==0 && special!=GLUT_KEY_F5 && m_ShiftState && !mod&GLUT_ACTIVE_SHIFT)
	{ 
		m_ShiftState=false;	
		m_Selection=false;
	}
	
	if (m_ShiftState) 
	{
		if (m_Position<m_HighlightStart) m_HighlightStart=m_Position;	
		else m_HighlightEnd=m_Position;
	}
	
	/*cerr<<"----------------"<<endl;
	cerr<<PreviousLineLength(m_Position)<<endl;
	cerr<<LineLength(m_Position)<<endl;
	cerr<<NextLineLength(m_Position)<<endl;
	cerr<<OffsetToCurrentLineStart()<<endl;
	cerr<<"----------------"<<endl;*/
	
}

void GLEditor::ProcessTabs()
{
	size_t pos=m_Text.find("\t",0);
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
	size_t nextlinestart=m_Text.find("\n",m_Position);
	if (nextlinestart!=string::npos)
	{	
		return LineLength(nextlinestart+1);
	}
	return 0;

}

int GLEditor::PreviousLineLength(int pos)
{
	size_t previouslineend=string::npos;
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
	return lineend-linestart;
}

unsigned int GLEditor::LineStart(int pos)
{
	unsigned int linestart=string::npos;

	if (pos>0) 
	{
		// take one off if we're over a newline
		if (m_Text[pos]=='\n') linestart=m_Text.rfind("\n",pos-1);
		else linestart=m_Text.rfind("\n",pos);
	}
	
	if (linestart!=string::npos) linestart++; // move the start off the newline
	else linestart=0; // if are on the first line, set the start to 0
	
	return linestart;
}

unsigned int GLEditor::LineEnd(int pos)
{
	if (m_Text.empty()) return 0;
	size_t end = m_Text.find("\n",pos);
	if (end==string::npos) end=m_Text.size()-1;
	return end;
}

void GLEditor::ParseOpenParentheses(int pos, int type)
{
	// looking for a close, so search forward
	int stack=0, start_pos = pos;
	pos++;
	while(stack!=-1 && pos<(int)m_Text.size())
	{
		if (m_Text[pos]==m_OpenChars[type]) stack++;
		if (m_Text[pos]==m_CloseChars[type]) stack--;
		pos++;
	}
	if (stack==-1)
	{
		m_ParenthesesHighlight[0]=start_pos;
		m_ParenthesesHighlight[1]=pos-1;
	}
}

void GLEditor::ParseCloseParentheses(int pos, int type)
{
	// looking for a open, so search backward
	int stack=0, start_pos = pos;
	pos--;
	while(stack!=-1 && pos>=0)
	{
		if (m_Text[pos]==m_CloseChars[type]) stack++;
		if (m_Text[pos]==m_OpenChars[type]) stack--;
		pos--;
	}
	if (stack==-1)
	{
		m_ParenthesesHighlight[0]=pos+1;
		m_ParenthesesHighlight[1]=start_pos;
	}	
}
