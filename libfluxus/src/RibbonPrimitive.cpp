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

#include "Renderer.h"
#include "RibbonPrimitive.h"
#include "State.h"

using namespace Fluxus;

RibbonPrimitive::RibbonPrimitive() 
{	
	AddData("p",new TypedPData<dVector>);
	AddData("w",new TypedPData<float>);
	AddData("c",new TypedPData<dVector>);
	PDataDirty();
}

RibbonPrimitive::RibbonPrimitive(const RibbonPrimitive &other) :
Primitive(other) 
{
	PDataDirty();
}

RibbonPrimitive::~RibbonPrimitive()
{
}

RibbonPrimitive* RibbonPrimitive::Clone() const 
{
	return new RibbonPrimitive(*this); 
}

void RibbonPrimitive::PDataDirty()
{
	// reset pointers
	m_VertData=GetDataVec<dVector>("p");
	m_WidthData=GetDataVec<float>("w");
	m_ColData=GetDataVec<dVector>("c");
}

void RibbonPrimitive::Render()
{
	if (m_VertData->size()<2) return;
	
	dMatrix ModelView;
	glGetFloatv(GL_MODELVIEW_MATRIX,ModelView.arr());

	if (m_State.Hints & HINT_UNLIT) glDisable(GL_LIGHTING);
	if (m_State.Hints & HINT_AALIAS) glEnable(GL_LINE_SMOOTH);		
	
	if (m_State.Hints & HINT_SOLID)
	{
		dVector CameraDir(0,0,1);
		CameraDir=ModelView.inverse().transform_no_trans(CameraDir);
		CameraDir.normalise();

		glBegin(GL_TRIANGLE_STRIP);

		for (unsigned int n=0; n<m_VertData->size()-1; n++)
		{
			float txstart = n/(float)m_VertData->size();
			float txend = (n+1)/(float)m_VertData->size();
		
			dVector line=(*m_VertData)[n+1]-(*m_VertData)[n];
			dVector up=line.cross(CameraDir);
			up.normalise();

			dVector topnorm=up;
			dVector botnorm=-up;

			glTexCoord2f(txstart,0);
			glNormal3fv(botnorm.arr());
			glVertex3fv(((*m_VertData)[n]-(up*(*m_WidthData)[n])).arr());
			glTexCoord2f(txstart,1);
			glNormal3fv(topnorm.arr());
			glVertex3fv(((*m_VertData)[n]+(up*(*m_WidthData)[n])).arr());
			glTexCoord2f(txend,0);
			glNormal3fv(botnorm.arr());
			glVertex3fv(((*m_VertData)[n+1]-(up*(*m_WidthData)[n+1])).arr());
			glTexCoord2f(txend,1);
			glNormal3fv(topnorm.arr());
			glVertex3fv(((*m_VertData)[n+1]+(up*(*m_WidthData)[n+1])).arr());
		}
		glEnd();

	}
	
	if (m_State.Hints & HINT_WIRE)
	{
		if (m_State.Hints & HINT_VERTCOLS)
		{
			glBegin(GL_LINE_STRIP);
			for (unsigned int n=0; n<m_VertData->size()-1; n++)
			{
				float txstart = n/(float)m_VertData->size();
				float txend = (n+1)/(float)m_VertData->size();
				glTexCoord2f(txstart,0);
				glColor3fv((*m_ColData)[n].arr());
				glVertex3fv((*m_VertData)[n].arr());
				glTexCoord2f(txend,0);
				glColor3fv((*m_ColData)[n+1].arr());
				glVertex3fv((*m_VertData)[n+1].arr());
			}
			glEnd();
		}
		else
		{
		    glColor4fv(m_State.WireColour.arr());
			glBegin(GL_LINE_STRIP);
			for (unsigned int n=0; n<m_VertData->size()-1; n++)
			{
				float txstart = n/(float)m_VertData->size();
				float txend = (n+1)/(float)m_VertData->size();
				glTexCoord2f(txstart,0);
				glVertex3fv((*m_VertData)[n].arr());
				glTexCoord2f(txend,0);
				glVertex3fv((*m_VertData)[n+1].arr());
			}
			glEnd();
		}
	}
	
	
	
	if (m_State.Hints & HINT_AALIAS) glDisable(GL_LINE_SMOOTH);		
	if (m_State.Hints & HINT_UNLIT) glEnable(GL_LIGHTING);
}

dBoundingBox RibbonPrimitive::GetBoundingBox()
{
	dBoundingBox box;
	for (unsigned int n=0; n<m_VertData->size()-1; n++)
	{
		box.expand((*m_VertData)[n]);
	}
	return box;
}

void RibbonPrimitive::ApplyTransform(bool ScaleRotOnly)
{
	if (!ScaleRotOnly)
	{
		for (vector<dVector>::iterator i=m_VertData->begin(); i!=m_VertData->end(); ++i)
		{
			*i=GetState()->Transform.transform(*i);
		}
	}
	else
	{
		for (vector<dVector>::iterator i=m_VertData->begin(); i!=m_VertData->end(); ++i)
		{
			*i=GetState()->Transform.transform_no_trans(*i);
		}
	}
	
	GetState()->Transform.init();
}

