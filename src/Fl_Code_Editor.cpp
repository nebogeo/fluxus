#include "Fl_Code_Editor.h"
#include <iostream>

Fl_Code_Editor::Fl_Code_Editor(int X, int Y, int W, int H, const char* l) :
Fl_Text_Editor(X,Y,W,H,l),
m_OpenChars("([<{"),
m_CloseChars(")]>}")
{	
}

Fl_Code_Editor::~Fl_Code_Editor() 
{
}

int Fl_Code_Editor::handle(int event)
{
	int ret=Fl_Text_Editor::handle(event);
	
	if (event==FL_KEYBOARD || event==FL_PUSH)
	{
		buffer()->unhighlight();
		int pos=insert_position()-1;
		if (pos>=0)
		{
			int type=0;
			for (string::iterator i=m_OpenChars.begin(); i!=m_OpenChars.end(); i++)
			{
				if (buffer()->text()[pos+1]==*i) ParseOpenParentheses(pos+1,type);
				type++;
			}
			
			type=0;
			for (string::iterator i=m_CloseChars.begin(); i!=m_CloseChars.end(); i++)
			{
				if (buffer()->text()[pos]==*i) ParseCloseParentheses(pos,type);
				type++;
			}
		}
	}
	return ret;
}

void Fl_Code_Editor::ParseOpenParentheses(int pos, int type)
{
	// looking for a close, so search forward
	int stack=0;
	pos++;
	while(stack!=-1 && pos<buffer()->length())
	{
		if (buffer()->text()[pos]==m_OpenChars[type]) stack++;
		if (buffer()->text()[pos]==m_CloseChars[type]) stack--;
		pos++;
	}
	if (stack==-1)
	{
		buffer()->highlight(pos-1,pos);
	}
}

void Fl_Code_Editor::ParseCloseParentheses(int pos, int type)
{
	// looking for a open, so search backward
	int stack=0;
	pos--;
	while(stack!=-1 && pos>0)
	{
		if (buffer()->text()[pos]==m_CloseChars[type]) stack++;
		if (buffer()->text()[pos]==m_OpenChars[type]) stack--;
		pos--;
	}
	if (stack==-1)
	{
		buffer()->highlight(pos+1,pos+2);
	}	
}
