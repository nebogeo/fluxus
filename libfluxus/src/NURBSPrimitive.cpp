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
#include "NURBSPrimitive.h"
#include "State.h"

using namespace fluxus;
	
NURBSPrimitive::NURBSPrimitive() :
m_Finalised(false),
m_UOrder(0),
m_VOrder(0),
m_UCVCount(0),
m_VCVCount(0),
m_Stride(4)
{
	m_Surface = gluNewNurbsRenderer();
	gluNurbsProperty(m_Surface, GLU_SAMPLING_METHOD, GLU_PARAMETRIC_ERROR);
	gluNurbsProperty(m_Surface, GLU_PARAMETRIC_TOLERANCE, 5.0);
	gluNurbsProperty(m_Surface, GLU_DISPLAY_MODE, GLU_FILL);
	gluNurbsProperty(m_Surface, GLU_CULLING, GLU_TRUE);
}

NURBSPrimitive::~NURBSPrimitive()
{
	gluDeleteNurbsRenderer(m_Surface);
}

void NURBSPrimitive::Finalise()
{
	// should check the vectors are all the right size etc
	m_Finalised=true;
}

void NURBSPrimitive::Render()
{
	if (!m_Finalised) return;
	
	glEnable(GL_AUTO_NORMAL);
	if (m_State.Hints & HINT_SOLID)
	{
		gluNurbsProperty(m_Surface, GLU_DISPLAY_MODE, GLU_FILL);
	
		gluBeginSurface(m_Surface);

		if (!m_STVec.empty())
		{
			gluNurbsSurface(m_Surface,m_UKnotVec.size(),&(*m_UKnotVec.begin()),m_VKnotVec.size(),&(*m_VKnotVec.begin()),
			 						 m_VCVCount*4,4,
									 m_STVec.begin()->arr(),m_UOrder,m_VOrder,GL_MAP2_TEXTURE_COORD_2);
		}
		
		gluNurbsSurface(m_Surface,m_UKnotVec.size(),&(*m_UKnotVec.begin()),m_VKnotVec.size(),&(*m_VKnotVec.begin()),
		 						 m_VCVCount*4,4,
								 m_CVVec.begin()->arr(),m_UOrder,m_VOrder,GL_MAP2_VERTEX_3);

		gluEndSurface(m_Surface);
	}

	if (m_State.Hints & HINT_WIRE)
	{
		glDisable(GL_LIGHTING);
		glColor3f(0,1,0);
		gluNurbsProperty(m_Surface, GLU_DISPLAY_MODE, GLU_OUTLINE_POLYGON);

		gluBeginSurface(m_Surface);
		gluNurbsSurface(m_Surface,m_UKnotVec.size(),&(*m_UKnotVec.begin()),m_VKnotVec.size(),&(*m_VKnotVec.begin()),
		 				m_VCVCount*4,4,
						m_CVVec.begin()->arr(),m_UOrder,m_VOrder,GL_MAP2_VERTEX_3);

		gluEndSurface(m_Surface);
		glEnable(GL_LIGHTING);
	}

	if (m_State.Hints & HINT_POINTS)
	{
		glColor3f(1,1,1);
		glDisable(GL_LIGHTING);
		glBegin(GL_POINTS);
		for (unsigned int n=0; n<m_CVVec.size(); n++)
		{
			glVertex3fv(m_CVVec[n].arr());
		}
		glEnd();
		glEnable(GL_LIGHTING);
	}

	glDisable(GL_AUTO_NORMAL);
}

void NURBSPrimitive::SetData(char t, unsigned int i, dVector v)
{
	switch (t)
	{
		case 'p': if (i<m_CVVec.size()) m_CVVec[i]=v; break;
		case 't': if (i<m_STVec.size()) m_STVec[i]=v; break;
		default: break;
	}
}

dVector NURBSPrimitive::GetData(char t, unsigned int i)
{
	dVector ret;
	switch (t)
	{
		case 'p': if (i<m_CVVec.size()) ret=m_CVVec[i]; break;
		case 't': if (i<m_STVec.size()) ret=m_STVec[i]; break;
		default: break;
	}
	return ret;
}

dBoundingBox NURBSPrimitive::GetBoundingBox()
{	
	dBoundingBox box;
	for (vector<dVector>::iterator i=m_CVVec.begin();	i!=m_CVVec.end(); ++i)
	{
		box.expand(*i);
	}
	return box;
}

void NURBSPrimitive::ApplyTransform(bool ScaleRotOnly)
{
	if (!ScaleRotOnly)
	{
		for (vector<dVector>::iterator i=m_CVVec.begin(); i!=m_CVVec.end(); ++i)
		{
			*i=GetState()->Transform.transform(*i);
		}
	}
	else
	{
		for (vector<dVector>::iterator i=m_CVVec.begin(); i!=m_CVVec.end(); ++i)
		{
			*i=GetState()->Transform.transform_no_trans(*i);
		}
	}
	
	GetState()->Transform.init();
	Finalise();
}
