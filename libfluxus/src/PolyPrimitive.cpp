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
m_VertData(NULL),
m_NormData(NULL),
m_ColData(NULL),
m_TexData(NULL),
m_NumVerts(0)
{
}

PolyPrimitive::~PolyPrimitive()
{
	if (m_VertData!=NULL) delete[] m_VertData;
	if (m_NormData!=NULL) delete[] m_NormData;	
	if (m_ColData!=NULL) delete[] m_ColData;
	if (m_TexData!=NULL) delete[] m_TexData;	
}

void PolyPrimitive::Finalise()
{
	// fill the array with loaded verts
	m_NumVerts=m_VertVec.size();
	
	if (m_VertData!=NULL) delete[] m_VertData;
	if (m_NormData!=NULL) delete[] m_NormData;	
	if (m_ColData!=NULL) delete[] m_ColData;	
	if (m_TexData!=NULL) delete[] m_TexData;	
	m_VertData = new float[m_NumVerts*3];
	m_NormData = new float[m_NumVerts*3];
	m_ColData  = new float[m_NumVerts*4];
	m_TexData  = new float[m_NumVerts*2];
	
	int Count=0,TexCount=0,ColCount=0;
	for (vector<dVertex>::iterator i=m_VertVec.begin();	i!=m_VertVec.end(); ++i)
	{
		m_NormData[Count]=i->normal.x;
		m_VertData[Count]=i->point.x;
		Count++;
		m_NormData[Count]=i->normal.y;
		m_VertData[Count]=i->point.y;
		Count++;
		m_NormData[Count]=i->normal.z;
		m_VertData[Count]=i->point.z;
		Count++;
		
		m_TexData[TexCount++]=i->s;
		m_TexData[TexCount++]=i->t;
		
		m_ColData[ColCount++]=i->col.r;
		m_ColData[ColCount++]=i->col.g;
		m_ColData[ColCount++]=i->col.b;
		m_ColData[ColCount++]=i->col.a;
	}
}

void PolyPrimitive::Render()
{
	int type=0;
	switch (m_Type)
	{
		case TRISTRIP : type=GL_TRIANGLE_STRIP; break;
		case QUADS : type=GL_QUADS; break;
		case TRILIST : type=GL_TRIANGLES; break;
		case TRIFAN : type=GL_TRIANGLE_FAN; break;
	}
	
	if (m_State.Hints & HINT_WIRE)
	{
		glPolygonMode(GL_FRONT,GL_LINE);
		glPolygonMode(GL_BACK,GL_LINE);
		if (m_State.Hints & HINT_AALIAS) glEnable(GL_LINE_SMOOTH);
		glLineWidth(m_State.LineWidth);
	}
		
	if (m_State.Hints & HINT_SOLID)
	{
		glPolygonMode(GL_FRONT,GL_FILL);
	    if (m_State.Hints & HINT_AALIAS) glEnable(GL_POLYGON_SMOOTH);
	}
	
	if (m_VertColours) glEnable(GL_COLOR_MATERIAL);
	if (m_State.Hints & HINT_UNLIT) glDisable(GL_LIGHTING);
	if (m_State.Hints & HINT_POINTS) glPolygonMode(GL_FRONT,GL_POINTS);
	if (m_State.Hints & HINT_BOUND) RenderBoundingBox();
	
	if (m_State.Hints & HINT_NORMAL)
	{
		glBegin(GL_LINES);
		for (vector<dVertex>::iterator i=m_VertVec.begin();	i!=m_VertVec.end(); ++i)
		{
			glNormal3f(i->normal.x,i->normal.y,i->normal.z);
			glVertex3f(i->point.x,i->point.y,i->point.z);
			glVertex3f(i->point.x+i->normal.x,i->point.y+i->normal.y,i->point.z+i->normal.z);
		}
		glEnd();
	}
	
	if (m_VertData!=NULL)
	{
		glVertexPointer(3,GL_FLOAT,0,(void*)m_VertData);
		glNormalPointer(GL_FLOAT,0,(void*)m_NormData);
		glTexCoordPointer(2,GL_FLOAT,0,(void*)m_TexData);
				
		if (m_VertColours)
		{
			glEnableClientState(GL_COLOR_ARRAY);
			glColorPointer(3,GL_FLOAT,0,(void*)m_ColData);
		}
		else
		{
			glDisableClientState(GL_COLOR_ARRAY);
		}
		
		glDrawArrays(type,0,m_NumVerts);	
	}
	else
	{
		glBegin(type);
		for (vector<dVertex>::iterator i=m_VertVec.begin();	i!=m_VertVec.end(); ++i)
		{
			glNormal3f(i->normal.x,i->normal.y,i->normal.z);
			if (m_VertColours) glColor4f(i->col.r,i->col.g,i->col.b,i->col.a);
			glTexCoord2f(i->s,i->t);
			glVertex3f(i->point.x,i->point.y,i->point.z);
		}
		glEnd();
	}

	if (m_VertColours) glDisable(GL_COLOR_MATERIAL);
	if (m_State.Hints & HINT_UNLIT) glEnable(GL_LIGHTING);
}

void PolyPrimitive::SetData(char t, unsigned int i, dVector v)
{
	if (i<m_VertVec.size())
	{
		switch (t)
		{
			case 'p': m_VertVec[i].point=v; break;
			case 'n': m_VertVec[i].normal=v; break;
			case 'c': m_VertVec[i].col=dColour(v.x,v.y,v.z); break;
			case 't': m_VertVec[i].s=v.x; m_VertVec[i].t=v.y; break;
			default: break;
		}
	}
}

dVector PolyPrimitive::GetData(char t, unsigned int i)
{
	/*if (i<m_VertVec.size())
	{
		switch (t)
		{
			case 'p': return m_VertVec[i].point; break;
			case 'n': return m_VertVec[i].normal; break;
			case 'c': return dVector(m_VertVec[i].col.r,m_VertVec[i].col.g,m_VertVec[i].col.b); break;
			case 't': return dVector(m_VertVec[i].s,m_VertVec[i].t,0); break;
			default: break;
		}
	}*/
	
	return dVector();
}

dBoundingBox PolyPrimitive::GetBoundingBox()
{	
	dBoundingBox box;
	for (vector<dVertex>::iterator i=m_VertVec.begin();	i!=m_VertVec.end(); ++i)
	{
		box.expand(i->point);
	}
	return box;
}

void PolyPrimitive::ApplyTransform(bool ScaleRotOnly)
{
	if (!ScaleRotOnly)
	{
		for (vector<dVertex>::iterator i=m_VertVec.begin();	i!=m_VertVec.end(); ++i)
		{
			*i=GetState()->Transform.transform(*i);
		}
	}
	else
	{
		for (vector<dVertex>::iterator i=m_VertVec.begin();	i!=m_VertVec.end(); ++i)
		{
			i->point=GetState()->Transform.transform_no_trans(i->point);
			i->normal=GetState()->Transform.transform_no_trans(i->normal).normalise();
		}
	}
	
	GetState()->Transform.init();
	Finalise();
}

istream &fluxus::operator>>(istream &s, PolyPrimitive &o)
{
	s.ignore(3);
	s>>o.m_State;	
	s.ignore(3);
	int t=0;
	s.read((char*)&t,sizeof(int));
	o.m_Type=(PolyPrimitive::Type)t;
	s.read((char*)&o.m_NumVerts,sizeof(int));
	o.m_VertData = new float[3*o.m_NumVerts];
	s.read((char*)o.m_VertData,sizeof(float)*3*o.m_NumVerts);
	o.m_NormData = new float[3*o.m_NumVerts];
	s.read((char*)o.m_NormData,sizeof(float)*3*o.m_NumVerts);
	o.m_TexData = new float[2*o.m_NumVerts];
	s.read((char*)o.m_TexData,sizeof(float)*2*o.m_NumVerts);
	s.read((char*)&o.m_VertColours,sizeof(bool));
	if (o.m_VertColours)
	{
		o.m_ColData = new float[3*o.m_NumVerts];
		s.read((char*)o.m_ColData,sizeof(float)*3*o.m_NumVerts);	
	}
	return s;
}

ostream &fluxus::operator<<(ostream &s, PolyPrimitive &o)
{
	s.write("pol",3);
	s<<o.m_State;
	s.write("pdt",3);
	int t=o.m_Type;
	s.write((char*)&t,sizeof(int));
	s.write((char*)&o.m_NumVerts,sizeof(int));
	s.write((char*)o.m_VertData,sizeof(float)*3*o.m_NumVerts);
	s.write((char*)o.m_NormData,sizeof(float)*3*o.m_NumVerts);
	s.write((char*)o.m_TexData,sizeof(float)*2*o.m_NumVerts);
	s.write((char*)&o.m_VertColours,sizeof(bool));
	if (o.m_VertColours)
	{
		s.write((char*)o.m_ColData,sizeof(float)*3*o.m_NumVerts);	
	}
	return s;
}
