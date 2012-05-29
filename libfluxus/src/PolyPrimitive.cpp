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

#include <stdio.h>

#include "OpenGL.h"

#include "Renderer.h"
#include "PolyPrimitive.h"
#include "State.h"
#include "TexturePainter.h"

//#define RENDER_NORMALS
//#define RENDER_BBOX

using namespace Fluxus;

PolyPrimitive::PolyPrimitive(Type t) :
m_IndexMode(false),
m_Type(t)
{
	AddData("p",new TypedPData<dVector>);
	AddData("n",new TypedPData<dVector>);
	AddData("c",new TypedPData<dColour>);
	AddData("t",new TypedPData<dVector>);

	// setup the direct access for speed
	PDataDirty();
}

PolyPrimitive::PolyPrimitive(const PolyPrimitive &other) :
Primitive(other),
m_IndexMode(other.m_IndexMode),
m_IndexData(other.m_IndexData),
m_Type(other.m_Type)
{
	PDataDirty();
}

PolyPrimitive::~PolyPrimitive()
{
}

PolyPrimitive *PolyPrimitive::Clone() const
{
	return new PolyPrimitive(*this);
}

void PolyPrimitive::Clear()
{
	Resize(0);
	m_ConnectedVerts.clear();
	m_GeometricNormals.clear();
	m_UniqueEdges.clear();
}

void PolyPrimitive::PDataDirty()
{
	// reset pointers
	m_VertData=GetDataVec<dVector>("p");
	m_NormData=GetDataVec<dVector>("n");
	m_ColData=GetDataVec<dColour>("c");
	m_TexData=GetDataVec<dVector>("t");
}

void PolyPrimitive::AddVertex(const dVertex &Vert) 
{ 
	m_VertData->push_back(Vert.point); 
	m_NormData->push_back(Vert.normal); 
	m_ColData->push_back(Vert.col); 	
	m_TexData->push_back(dVector(Vert.s, Vert.t, 0));
	
	m_ConnectedVerts.clear();
	m_GeometricNormals.clear();
	m_UniqueEdges.clear();
}

void PolyPrimitive::Render()
{
	// some drivers crash if they don't get enough data for a primitive...
	if (m_VertData->size()<3) return;
	if (m_IndexMode && m_IndexData.size()<3) return;

	int type=0;
	switch (m_Type)
	{
		case TRISTRIP : type=GL_TRIANGLE_STRIP; break;
		case QUADS :
			// some drivers crash if they don't get enough data for a primitive...
			if (m_IndexMode)
			{
				if (m_IndexData.size()<4) return;
			}
			else
			{
				if (m_VertData->size()<4) return;
			}
			type=GL_QUADS;
		break;
		case TRILIST : type=GL_TRIANGLES; break;
		case TRIFAN : type=GL_TRIANGLE_FAN; break;
		case POLYGON : type=GL_POLYGON; break;
	}

	if (m_State.Hints & HINT_AALIAS) glEnable(GL_LINE_SMOOTH);
	else glDisable(GL_LINE_SMOOTH);

	if (m_State.Hints & HINT_NORMAL)
	{
		glColor4fv(m_State.NormalColour.arr());
		glDisable(GL_LIGHTING);
		glBegin(GL_LINES);
		for (unsigned int i=0; i<m_VertData->size(); i++)
		{
			glVertex3fv((*m_VertData)[i].arr());
			glVertex3fv(((*m_VertData)[i]+(*m_NormData)[i]).arr());
		}
		glEnd();
		glEnable(GL_LIGHTING);
		glColor4fv(m_State.Colour.arr());
	}
	if (m_State.Hints & HINT_UNLIT) glDisable(GL_LIGHTING);

	glVertexPointer(3,GL_FLOAT,sizeof(dVector),(void*)m_VertData->begin()->arr());
	glNormalPointer(GL_FLOAT,sizeof(dVector),(void*)m_NormData->begin()->arr());
	glTexCoordPointer(3,GL_FLOAT,sizeof(dVector),(void*)m_TexData->begin()->arr());

	if (m_State.Hints & HINT_SPHERE_MAP)
	{
		glEnable(GL_TEXTURE_GEN_S);
		glEnable(GL_TEXTURE_GEN_T);
		glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
		glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
	}

	#ifndef DISABLE_MULTITEXTURE
	if (TexturePainter::Get()->MultitexturingEnabled())
	{
		// possibly a candidate to put in Primitive:PreRender()
		for (int n=1; n<MAX_TEXTURES; n++)
		{
			if (m_State.Textures[n]!=0)
			{
				char name[3];
				snprintf(name,3,"t%d",n);
				TypedPData<dVector> *tex = dynamic_cast<TypedPData<dVector>*>(GetDataRaw(name));
				glClientActiveTexture(GL_TEXTURE0+n);
				glEnableClientState(GL_TEXTURE_COORD_ARRAY);

				if (tex!=NULL)
				{
					glTexCoordPointer(3,GL_FLOAT,sizeof(dVector),(void*)tex->m_Data.begin()->arr());
				}
				else // default to using the normal vertex coordinates
				{
					glTexCoordPointer(3,GL_FLOAT,sizeof(dVector),(void*)m_TexData->begin()->arr());
				}
			}
		}
		glClientActiveTexture(GL_TEXTURE0);
	}
	#endif

	if (m_State.Hints & HINT_VERTCOLS)
	{
		glEnableClientState(GL_COLOR_ARRAY);
		glColorPointer(4,GL_FLOAT,sizeof(dVector),(void*)m_ColData->begin()->arr());
	}
	else
	{
		glDisableClientState(GL_COLOR_ARRAY);
	}

	if (m_State.Hints & HINT_SOLID)
	{
		if (m_IndexMode) glDrawElements(type,m_IndexData.size(),GL_UNSIGNED_INT,&(m_IndexData[0]));
		else glDrawArrays(type,0,m_VertData->size());
	}

	if (m_State.Hints & HINT_WIRE)
	{
		glDisable(GL_TEXTURE_2D);
		glPolygonOffset(1,1);
		glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
		glColor4fv(m_State.WireColour.arr());
		if ((m_State.Hints & HINT_WIRE_STIPPLED) > HINT_WIRE)
		{
			glEnable(GL_LINE_STIPPLE);
			glLineStipple(m_State.StippleFactor, m_State.StipplePattern);
		}

		glDisable(GL_LIGHTING);
		if (m_IndexMode) glDrawElements(type,m_IndexData.size(),GL_UNSIGNED_INT,&(m_IndexData[0]));
		else glDrawArrays(type,0,m_VertData->size());
		glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
		glEnable(GL_LIGHTING);
		glEnable(GL_TEXTURE_2D);
		if ((m_State.Hints & HINT_WIRE_STIPPLED) > HINT_WIRE)
		{
			glDisable(GL_LINE_STIPPLE);
		}
		glColor4fv(m_State.Colour.arr());
	}

	if (m_State.Hints & HINT_POINTS)
	{
		glDisable(GL_TEXTURE_2D);
		glPolygonMode(GL_FRONT_AND_BACK,GL_POINT);
		glColor4fv(m_State.WireColour.arr());
		glDisable(GL_LIGHTING);
		if (m_IndexMode) glDrawElements(type,m_IndexData.size(),GL_UNSIGNED_INT,&(m_IndexData[0]));
		else glDrawArrays(type,0,m_VertData->size());
		glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
		glEnable(GL_LIGHTING);
		glEnable(GL_TEXTURE_2D);
		glColor4fv(m_State.Colour.arr());
	}


	if (m_State.Hints & HINT_UNLIT) glEnable(GL_LIGHTING);
	if (m_State.Hints & HINT_AALIAS) glDisable(GL_LINE_SMOOTH);
	if (m_State.Hints & HINT_SPHERE_MAP)
	{
		glDisable(GL_TEXTURE_GEN_S);
		glDisable(GL_TEXTURE_GEN_T);
	}
}

void PolyPrimitive::RecalculateNormals(bool smooth)
{
	GenerateTopology();
	CalculateGeometricNormals();

	if (!m_GeometricNormals.empty()) 
	{
		if (m_IndexMode) // accumulate
		{
			vector<int> count(m_VertData->size());
						
			// clear the normals
			for (unsigned int i=0; i<m_NormData->size(); i++)
			{
				(*m_NormData)[i]=dVector(0,0,0);
				count[i]=0;
			}
			
			// add all the contributing normals
			for (unsigned int i=0; i<m_IndexData.size(); i++)
			{
				(*m_NormData)[m_IndexData[i]]+=m_GeometricNormals[i];
				count[m_IndexData[i]]++;
			}

			// scale back
			for (unsigned int i=0; i<m_NormData->size(); i++)
			{
				(*m_NormData)[i]/=(float)count[i];
			}
		}
		else
		{
			for (unsigned int i=0; i<m_VertData->size(); i++)
			{
				(*m_NormData)[i]=m_GeometricNormals[i];
			}
		}
		
		if (smooth && !m_IndexMode)
		{
			// smooth the normals
			TypedPData<dVector> *newnorms = new TypedPData<dVector>;
			for (unsigned int i=0; i<m_VertData->size(); i++)
			{
				float count=1;
				dVector n = (*m_NormData)[i];
				for (vector<int>::iterator b=m_ConnectedVerts[i].begin(); 
						b!=m_ConnectedVerts[i].end(); b++)
				{
					n+=(*m_NormData)[*b];
					count+=1;
				}
				newnorms->m_Data.push_back((n/count).normalise());
			}
			SetDataRaw("n", newnorms);
		}
	}
}

void PolyPrimitive::ConvertToIndexed()
{
	if (m_IndexMode) return;

	if (m_ConnectedVerts.empty())
	{
		CalculateConnected();
	}
		
	TypedPData<dVector> *NewVerts = new TypedPData<dVector>;
	TypedPData<dVector> *NewNorms = new TypedPData<dVector>;
	TypedPData<dColour> *NewCols = new TypedPData<dColour>;
	TypedPData<dVector> *NewTex = new TypedPData<dVector>;

	m_IndexData.clear();
	int vert=0;
	int index=0;
	map<int,int> verttoindex;
	for (vector<vector<int> >::iterator i=m_ConnectedVerts.begin();
		 i!=m_ConnectedVerts.end(); i++)
	{
		if (!i->empty() && verttoindex.find(vert)==verttoindex.end())
		{
			// take the first connected as our new point - will trash non-shared
			// normals, texture coords and colours
			NewVerts->m_Data.push_back((*m_VertData)[(*i)[0]]);
			NewNorms->m_Data.push_back((*m_NormData)[(*i)[0]]);
			NewCols->m_Data.push_back((*m_ColData)[(*i)[0]]);
			NewTex->m_Data.push_back((*m_TexData)[(*i)[0]]);
			m_IndexData.push_back(index);
			
			// record all the verts that can point to this index
			for (vector<int>::iterator v=i->begin(); v!=i->end(); v++)
			{
				verttoindex[*v]=index;
			}
			
			index++;
		}
		else
		{
			m_IndexData.push_back(verttoindex[vert]);
		}
		
		vert++;
	}
	
	SetDataRaw("p", NewVerts);
	SetDataRaw("n", NewNorms);
	SetDataRaw("c", NewCols);
	SetDataRaw("t", NewTex);
		
	m_IndexMode=true;
}

void PolyPrimitive::GenerateTopology()
{
	if (m_ConnectedVerts.empty())
	{
		CalculateConnected();
	}
	
	if (m_GeometricNormals.empty())
	{
		CalculateGeometricNormals();
	}
}

void PolyPrimitive::CalculateConnected()
{ 
	
	if (m_IndexMode)
	{		
		for (unsigned int i=0; i<m_IndexData.size(); i++)
		{
			vector<int> connected;
			for (unsigned int b=0; b<m_IndexData.size(); b++)
			{
				// compare index value
				if (i!=b && (m_IndexData[i]==m_IndexData[b] || 
				   // compare positions
				   ((*m_VertData)[m_IndexData[i]].feq((*m_VertData)[m_IndexData[b]]))))
				{
					connected.push_back(b);
				}
			}
			
			m_ConnectedVerts.push_back(connected);
		}
	}
	else
	{
		// cache the connected verts 
		for (unsigned int i=0; i<m_VertData->size(); i++)
		{
			vector<int> connected;
			for (unsigned int b=0; b<m_VertData->size(); b++)
			{
				// find all close verts
				if (i!=b && (*m_VertData)[i].feq((*m_VertData)[b]))
				{
					connected.push_back(b);
				}
			}	

			m_ConnectedVerts.push_back(connected);
		}
	}
}


void PolyPrimitive::CalculateGeometricNormals()
{
	///\todo - need different approach for TRIFAN
	// one face 
	if (m_Type==POLYGON && m_VertData->size()>2) 
	{
		m_GeometricNormals.clear();
		dVector a((*m_VertData)[0]-(*m_VertData)[1]);
		dVector b((*m_VertData)[1]-(*m_VertData)[2]);
		dVector normal(a.cross(b));
		normal.normalise();
		
		for (unsigned int i=0; i<m_VertData->size(); i++)
		{
			m_GeometricNormals.push_back(normal);
		}
		
		return;
	}
	
	int stride=0;
	if (m_Type==TRISTRIP) stride=2;
	if (m_Type==QUADS) stride=4;
	if (m_Type==TRILIST) stride=3;
	if (stride>0)
	{
		m_GeometricNormals.clear();
		
		if (m_IndexMode)
		{
			for (unsigned int i=0; i<m_IndexData.size(); i+=stride)
			{
				if (i+2<m_IndexData.size())
				{
					dVector a((*m_VertData)[m_IndexData[i]]-(*m_VertData)[m_IndexData[i+1]]);
					dVector b((*m_VertData)[m_IndexData[i+1]]-(*m_VertData)[m_IndexData[i+2]]);
					dVector normal(a.cross(b));
					normal.normalise();
					for (int n=0; n<stride; n++)
					{
						m_GeometricNormals.push_back(normal);
					}
				}
			}
		}
		else
		{
			for (unsigned int i=0; i<m_VertData->size(); i+=stride)
			{
				if (i+2<m_VertData->size())
				{
					dVector a((*m_VertData)[i]-(*m_VertData)[i+1]);
					dVector b((*m_VertData)[i+1]-(*m_VertData)[i+2]);
					dVector normal(a.cross(b));
					normal.normalise();
					for (int n=0; n<stride; n++)
					{
						m_GeometricNormals.push_back(normal);
					}
				}
			}
		}
	}
}

void PolyPrimitive::CalculateUniqueEdges()
{
	if (m_UniqueEdges.empty())
	{
		// todo - need different approach for TRIFAN
		int stride=0;
		if (m_Type==TRISTRIP) stride=2;
		if (m_Type==QUADS) stride=4;
		if (m_Type==TRILIST) stride=3;
		if (stride>0)
		{		
			set<pair<int,int> > firstpass;
			
			unsigned int vertcount=m_VertData->size();
			if (m_IndexMode) vertcount=m_IndexData.size();
			
			// first, record all valid edges from the topology type
			for (unsigned int i=0; i<vertcount; i+=stride)
			{
				for (int n=0; n<stride-1; n++)
				{
					firstpass.insert(pair<int,int>(n+i,n+i+1));
				}
				firstpass.insert(pair<int,int>(i+stride-1,i));
			}

			set<pair<int,int> > stored;
			pair<int,int> key;

			// now find all edges which share points and group them together
			for (unsigned int i=0; i<vertcount; i+=stride)
			{
				for (int n=0; n<stride-1; n++)
				{	
					UniqueEdgesFindShared(pair<int,int>(n+i,n+i+1), firstpass, stored);
				}	
				UniqueEdgesFindShared(pair<int,int>(i+stride-1,i), firstpass, stored);	
			}
		}
	}
}

void PolyPrimitive::UniqueEdgesFindShared(pair<int,int> edge, set<pair<int,int> > firstpass, set<pair<int,int> > &stored)
{
	vector<pair<int,int> > edges;
	
	if (stored.find(edge)==stored.end() && stored.find(pair<int,int>(edge.second,edge.first))==stored.end())
	{
		// first, store the test edge
		edges.push_back(edge);
		stored.insert(edge);
		
		//Trace::Stream<<"edge:"<<edge.first<<" "<<edge.second<<endl;
		
		// make all combinations of verts connected to the edge verts
		for (vector<int>::iterator a=m_ConnectedVerts[edge.first].begin();
			a!=m_ConnectedVerts[edge.first].end(); a++)
		{
			for (vector<int>::iterator b=m_ConnectedVerts[edge.second].begin();
					b!=m_ConnectedVerts[edge.second].end(); b++)
			{
				//Trace::Stream<<*a<<" "<<*b<<endl;
				pair<int, int> candidate(*a,*b);
				if (firstpass.find(candidate)!=firstpass.end() && // if this is a real edge
					stored.find(candidate)==stored.end() )        // and we've not stored it already
				{
					edges.push_back(candidate);
					stored.insert(candidate);
					//Trace::Stream<<"^ "<<candidate.first<<" "<<candidate.second<<endl;
				}						

				pair<int, int> rcandidate(*b,*a);
				if (firstpass.find(rcandidate)!=firstpass.end() && // if this is a real edge
					stored.find(rcandidate)==stored.end() )        // and we've not stored it already
				{
					edges.push_back(rcandidate);
					stored.insert(rcandidate);
					//Trace::Stream<<"^ "<<rcandidate.first<<" "<<rcandidate.second<<endl;
				}						
			}
		}

		if (!edges.empty())
		{
			m_UniqueEdges.push_back(edges);
		}
	}
}

dBoundingBox PolyPrimitive::GetBoundingBox(const dMatrix &space)
{	
	dBoundingBox box;
	for (vector<dVector,FLX_ALLOC(dVector) >::iterator i=m_VertData->begin(); i!=m_VertData->end(); ++i)
	{
		box.expand(space.transform(*i));
	}
	return box;
}

void PolyPrimitive::ApplyTransform(bool ScaleRotOnly)
{
	if (!ScaleRotOnly)
	{
		for (vector<dVector,FLX_ALLOC(dVector) >::iterator i=m_VertData->begin(); i!=m_VertData->end(); ++i)
		{
 			*i=GetState()->Transform.transform(*i);
			// why not normals?
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
}


