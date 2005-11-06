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
#include "PolyPrimitive.h"
#include "State.h"

//#define RENDER_NORMALS
//#define RENDER_BBOX

using namespace fluxus;
	
PolyPrimitive::PolyPrimitive(Type t) :
m_Type(t),
m_VertColours(false),
m_NumVerts(0),
m_Finalised(false)
{
	AddData("p",new TypedPData<dVector>);
	AddData("n",new TypedPData<dVector>);
	AddData("c",new TypedPData<dColour>);
	AddData("t",new TypedPData<dVector>);
	
	// direct access for speed
	m_VertData=GetDataVec<dVector>("p");
	m_NormData=GetDataVec<dVector>("n");
	m_ColData=GetDataVec<dColour>("c");
	m_TexData=GetDataVec<dVector>("t");
}

PolyPrimitive::~PolyPrimitive()
{
}

void PolyPrimitive::AddVertex(const dVertex &Vert) 
{ 
	m_VertData->push_back(Vert.point); 
	m_NormData->push_back(Vert.normal); 
	m_ColData->push_back(Vert.col); 	
	m_TexData->push_back(dVector(Vert.s, Vert.t, 0)); 
}	

void PolyPrimitive::Finalise()
{
	// fill the array with loaded verts
	m_NumVerts=m_VertData->size();
	
	if (m_NumVerts==m_NormData->size() &&
		m_NumVerts==m_ColData->size() &&
		m_NumVerts==m_TexData->size()) 
	{
		m_Finalised=true;
	}
}

void PolyPrimitive::Render()
{
	if (!m_Finalised) return;

	int type=0;
	switch (m_Type)
	{
		case TRISTRIP : type=GL_TRIANGLE_STRIP; break;
		case QUADS : type=GL_QUADS; break;
		case TRILIST : type=GL_TRIANGLES; break;
		case TRIFAN : type=GL_TRIANGLE_FAN; break;
		case POLYGON : type=GL_POLYGON; break;
	}
	
	
	if (m_State.Hints & HINT_AALIAS) glEnable(GL_LINE_SMOOTH);		
	else glDisable(GL_LINE_SMOOTH);		

	if (m_VertColours) glEnable(GL_COLOR_MATERIAL);
	if (m_State.Hints & HINT_UNLIT) glDisable(GL_LIGHTING);
	if (m_State.Hints & HINT_POINTS) glPolygonMode(GL_FRONT,GL_POINTS);
	if (m_State.Hints & HINT_BOUND) RenderBoundingBox();
	
	if (m_State.Hints & HINT_NORMAL)
	{
		glColor3f(1,0,0);
		glDisable(GL_LIGHTING);
		glBegin(GL_LINES);
		for (unsigned int i=0; i<m_VertData->size(); i++)
		{
			glVertex3fv((*m_VertData)[i].arr());
			glVertex3fv(((*m_VertData)[i]+(*m_NormData)[i]).arr());
		}
		glEnd();
		glEnable(GL_LIGHTING);
	}
	
	glVertexPointer(3,GL_FLOAT,sizeof(dVector),(void*)m_VertData->begin()->arr());
	glNormalPointer(GL_FLOAT,sizeof(dVector),(void*)m_NormData->begin()->arr());
	glTexCoordPointer(2,GL_FLOAT,sizeof(dVector),(void*)m_TexData->begin()->arr());
				
	if (m_VertColours)
	{
		glEnableClientState(GL_COLOR_ARRAY);
		glColorPointer(3,GL_FLOAT,sizeof(dVector),(void*)m_ColData->begin()->arr());
	}
	else
	{
		glDisableClientState(GL_COLOR_ARRAY);
	}
	
	if (m_State.Hints & HINT_SOLID)
	{
		glDrawArrays(type,0,m_NumVerts);
	}	
	
	if (m_State.Hints & HINT_WIRE)
	{
		glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
		glColor3f(0,0,0);
		glDisable(GL_LIGHTING);	
		glDrawArrays(type,0,m_NumVerts);	
		glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
		glEnable(GL_LIGHTING);
	}
		
	if (m_VertColours) glDisable(GL_COLOR_MATERIAL);
	if (m_State.Hints & HINT_UNLIT) glEnable(GL_LIGHTING);
}

void PolyPrimitive::RecalculateNormals()
{
	// todo - need different aproaches for TRISTRIP,QUADS,TRILIST,TRIFAN
	// to smooth, or not?
}

dBoundingBox PolyPrimitive::GetBoundingBox()
{	
	dBoundingBox box;
	for (vector<dVector>::iterator i=m_VertData->begin(); i!=m_VertData->end(); ++i)
	{
		box.expand(*i);
	}
	return box;
}

void PolyPrimitive::ApplyTransform(bool ScaleRotOnly)
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
		for (unsigned int i=0; i<m_VertData->size(); i++)
		{
			(*m_VertData)[i]=GetState()->Transform.transform_no_trans((*m_VertData)[i]);
			(*m_NormData)[i]=GetState()->Transform.transform_no_trans((*m_NormData)[i]).normalise();
		}
	}
	
	GetState()->Transform.init();
	Finalise();
}

