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

RibbonPrimitive::RibbonPrimitive() :
    m_InverseNormals(false)
{
	AddData("p",new TypedPData<dVector>);
	AddData("w",new TypedPData<float>);
	AddData("c",new TypedPData<dColour>);
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
	m_ColData=GetDataVec<dColour>("c");
}

void RibbonPrimitive::Render()
{
	if (m_VertData->size()<2) return;

	if (m_State.Hints & HINT_UNLIT) glDisable(GL_LIGHTING);
	if (m_State.Hints & HINT_AALIAS) glEnable(GL_LINE_SMOOTH);

	if (m_State.Hints & HINT_SPHERE_MAP)
	{
		glEnable(GL_TEXTURE_GEN_S);
		glEnable(GL_TEXTURE_GEN_T);
		glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
		glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
	}

	if (m_State.Hints & HINT_SOLID)
	{
		glBegin(GL_TRIANGLE_STRIP);
		if (m_State.Hints & HINT_VERTCOLS)
		{
			for (unsigned int n=0; n<m_VertData->size(); n++)
			{
				float tx = n/(float)m_VertData->size();
				dVector line;
				if (n==m_VertData->size()-1)
				{
					line=(*m_VertData)[n]-(*m_VertData)[n-1];
					tx=1.0f;
				}
				else line=(*m_VertData)[n+1]-(*m_VertData)[n];
				dVector up=line.cross(GetLocalCameraDir());
				up.normalise();

				dVector topnorm=up;
				dVector botnorm=-up;

                if (m_InverseNormals)
                {
                    topnorm=-up;
                    botnorm=up;
                }

				glColor4fv((*m_ColData)[n].arr());
				glTexCoord2f(tx,0);
				glNormal3fv(botnorm.arr());
				glVertex3fv(((*m_VertData)[n]-(up*(*m_WidthData)[n])).arr());
				glColor4fv((*m_ColData)[n].arr());
				glTexCoord2f(tx,1);
				glNormal3fv(topnorm.arr());
				glVertex3fv(((*m_VertData)[n]+(up*(*m_WidthData)[n])).arr());
			}
		}
		else
		{
		    glColor4fv(m_State.Colour.arr());
			for (unsigned int n=0; n<m_VertData->size(); n++)
			{
				float tx = n/(float)m_VertData->size();
				dVector line;
				if (n==m_VertData->size()-1)
				{
					line=(*m_VertData)[n]-(*m_VertData)[n-1];
					tx=1.0f;
				}
				else line=(*m_VertData)[n+1]-(*m_VertData)[n];
				dVector up=line.cross(GetLocalCameraDir());
				up.normalise();

				dVector topnorm=up;
				dVector botnorm=-up;

                if (m_InverseNormals)
                {
                    topnorm=-up;
                    botnorm=up;
                }

				glTexCoord2f(tx,0);
				glNormal3fv(botnorm.arr());
				glVertex3fv(((*m_VertData)[n]-(up*(*m_WidthData)[n])).arr());
				glTexCoord2f(tx,1);
				glNormal3fv(topnorm.arr());
				glVertex3fv(((*m_VertData)[n]+(up*(*m_WidthData)[n])).arr());
			}
		}
		glEnd();

	}

	if (m_State.Hints & HINT_WIRE)
	{
		if ((m_State.Hints & HINT_WIRE_STIPPLED) > HINT_WIRE)
		{
			glEnable(GL_LINE_STIPPLE);
			glLineStipple(m_State.StippleFactor, m_State.StipplePattern);
		}

		if (m_State.Hints & HINT_VERTCOLS)
		{
			glBegin(GL_LINE_STRIP);
			for (unsigned int n=0; n<m_VertData->size(); n++)
			{
				float tx = n/(float)m_VertData->size();
				glTexCoord2f(tx,0);
				glColor4fv((*m_ColData)[n].arr());
				glVertex3fv((*m_VertData)[n].arr());
			}
			glEnd();
		}
		else
		{
		    glColor4fv(m_State.WireColour.arr());
			glBegin(GL_LINE_STRIP);
			for (unsigned int n=0; n<m_VertData->size(); n++)
			{
				float tx = n/(float)m_VertData->size();
				glTexCoord2f(tx,0);
				glVertex3fv((*m_VertData)[n].arr());
			}
			glEnd();
		}

		if ((m_State.Hints & HINT_WIRE_STIPPLED) > HINT_WIRE)
		{
			glDisable(GL_LINE_STIPPLE);
		}
	}

	if (m_State.Hints & HINT_AALIAS) glDisable(GL_LINE_SMOOTH);
	if (m_State.Hints & HINT_UNLIT) glEnable(GL_LIGHTING);
	if (m_State.Hints & HINT_SPHERE_MAP)
	{
		glDisable(GL_TEXTURE_GEN_S);
		glDisable(GL_TEXTURE_GEN_T);
	}
}

dBoundingBox RibbonPrimitive::GetBoundingBox(const dMatrix &space)
{
	dBoundingBox box;
	for (unsigned int n=0; n<m_VertData->size()-1; n++)
	{
		box.expand(space.transform((*m_VertData)[n]));
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

