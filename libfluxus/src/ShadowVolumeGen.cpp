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

#include <algorithm>
#include "ShadowVolumeGen.h"

using namespace fluxus;

ShadowVolumeGen::ShadowVolumeGen() :
m_ShadowVolume(PolyPrimitive::QUADS),
m_LightPosition(5,5,0),
m_Active(false),
m_Length(10)
{
}

ShadowVolumeGen::~ShadowVolumeGen()	
{
}

void ShadowVolumeGen::Generate(Primitive *prim)
{	
	m_Active=true;
	PolyPrimitive *poly = dynamic_cast<PolyPrimitive*>(prim);
	if (poly)
	{
		PolyGen(poly);
	}
	else
	{		
		NURBSPrimitive *nurbs = dynamic_cast<NURBSPrimitive*>(prim);
		if (nurbs)
		{
			NURBSGen(nurbs);
		}
	}
}

void ShadowVolumeGen::Clear()
{ 
	m_Active=false;
	m_ShadowVolume.Resize(0);
}

PolyPrimitive *ShadowVolumeGen::GetVolume() 
{ 
	return &m_ShadowVolume; 
}

void ShadowVolumeGen::PolyGen(PolyPrimitive *src)
{	
	TypedPData<dVector> *points = dynamic_cast<TypedPData<dVector>* >(src->GetDataRaw("p"));
	const vector<dVector> &normals = src->GetGeometricNormals();
	const vector<vector<int> > &connected=src->GetConnectedVerts();
	
	dMatrix &transform = src->GetState()->Transform;
	
	int stride=0;
	if (src->GetType()==PolyPrimitive::TRISTRIP) stride=2;
	if (src->GetType()==PolyPrimitive::QUADS) stride=4;
	if (src->GetType()==PolyPrimitive::TRILIST) stride=3;
	if (stride>0)
	{
		SharedEdgeContainer edges = src->GetUniqueEdges();
		vector<pair<dVector,dVector> > silhouette;
			
		if (src->IsIndexed())
		{
			vector<unsigned int> index = src->GetIndex();
			// loop over all the edges
			for (SharedEdgeContainer::iterator i=edges.begin(); i!=edges.end(); ++i)
			{
				EdgeContainer sharededges = *i;

				dVector lightdir = transform.transform(points->m_Data[index[sharededges.begin()->first]])-m_LightPosition;
				lightdir.normalise();

				if (sharededges.size()==2)
				{
					bool backface=false;
					bool frontface=false;

					EdgeContainer::iterator frontedge;

					// loop over the edges shared by this one
					for (EdgeContainer::iterator edge=sharededges.begin(); edge!=sharededges.end(); ++edge)
					{			
						// only check the first vert, as they should have 
						// the same geometric normal if the poly is planar..
						if (lightdir.dot(transform.transform_no_trans(normals[edge->first]))>0) 
						{
							frontedge=edge;
							frontface=true;
						}
						else backface=true;
					}

					// if we contain both front and back facing normals (from the light's pov) 
					if (backface && frontface) 
					{			
						silhouette.push_back(pair<dVector,dVector>(transform.transform(points->m_Data[index[frontedge->first]]),
					                                    		   transform.transform(points->m_Data[index[frontedge->second]])));
					}
				}
			}
		}
		else
		{
			// loop over all the edges
			for (SharedEdgeContainer::iterator i=edges.begin(); i!=edges.end(); ++i)
			{
				EdgeContainer sharededges = *i;

				dVector lightdir = transform.transform(points->m_Data[sharededges.begin()->first])-m_LightPosition;
				lightdir.normalise();


				if (sharededges.size()==2)
				{
					bool backface=false;
					bool frontface=false;

					EdgeContainer::iterator frontedge;

					// loop over the edges shared by this one
					for (EdgeContainer::iterator edge=sharededges.begin(); edge!=sharededges.end(); ++edge)
					{			
						// only check the first vert, as they should have 
						// the same geometric normal if the poly is planar..
						if (lightdir.dot(transform.transform_no_trans(normals[edge->first]))>0) 
						{
							frontedge=edge;
							frontface=true;
						}
						else backface=true;
					}

					// if we contain both front and back facing normals (from the light's pov) 
					if (backface && frontface) 
					{			
						silhouette.push_back(pair<dVector,dVector>(transform.transform(points->m_Data[frontedge->first]),
					                                    		   transform.transform(points->m_Data[frontedge->second])));
					}
				}
			}
		}
		
		for (vector<pair<dVector,dVector> >::iterator i=silhouette.begin();
		     i!=silhouette.end(); i++)
		{
			AddEdge(i->first,i->second);
		}
	}
}

void ShadowVolumeGen::AddEdge(dVector start, dVector end)
{
	if (m_Debug)
	{
		glDisable(GL_LIGHTING);
		glLineWidth(3);
		glBegin(GL_LINES);					
			glColor3f(1,0,0);
			glVertex3fv(start.arr());
			glColor3f(0,0,1);
			glVertex3fv(end.arr());
		glEnd();
		glEnable(GL_LIGHTING);
	}

	m_ShadowVolume.AddVertex(dVertex(start,dVector(0,0,0),0,0));
	m_ShadowVolume.AddVertex(dVertex(end,dVector(0,0,0),0,0));

	dVector proj = end-m_LightPosition;
	m_ShadowVolume.AddVertex(dVertex(end+proj*m_Length,dVector(0,0,0),0,0));

	proj = start-m_LightPosition;
	m_ShadowVolume.AddVertex(dVertex(start+proj*m_Length,dVector(0,0,0),0,0));
}

void ShadowVolumeGen::NURBSGen(NURBSPrimitive *src)
{	
	TypedPData<dVector> *points = dynamic_cast<TypedPData<dVector>* >(src->GetDataRaw("p"));
	TypedPData<dVector> *normals = dynamic_cast<TypedPData<dVector>* >(src->GetDataRaw("n"));
	
	dMatrix &transform = src->GetState()->Transform;
	
	int stride=4;
	
	// loop over all the faces
	for (unsigned int vert=0; vert<points->Size(); vert+=stride)
	{
		// find out if we are on an edge or not
		vector<int> edgeverts;

		for (int facevert=0; facevert<stride; facevert++)
		{
			dVector lightdir = transform.transform(points->m_Data[vert+facevert])-m_LightPosition;
			lightdir.normalise();

			bool backface=false;
			bool frontface=false;

			// todo - figure out the neighbouring faces from the implicit topology
			
			// loop over the verts connected to this one
			/*for (vector<int>::const_iterator cv=connected[vert+facevert].begin(); 
				cv!=connected[vert+facevert].end(); cv++)
			{			
				if (lightdir.dot(transform.transform_no_trans(normals->m_Data[*cv]))>0) frontface=true;
				else backface=true;
			}*/
			
			// if we contain both front and back facing normals (from the light's pov) 
			if (backface && frontface) 
			{
				edgeverts.push_back(vert+facevert);
			}
		}

		if (edgeverts.size()>=2)
		{
			for (unsigned int i=0; i<edgeverts.size()-1; i++)
			{	
				dVector worldpoint1 = transform.transform(points->m_Data[edgeverts[i]]);
				dVector worldpoint2 = transform.transform(points->m_Data[edgeverts[i+1]]);

				glPushMatrix();
				glDisable(GL_LIGHTING);
				glBegin(GL_LINES);					
					glColor3f(1,0,0);
					glVertex3fv(worldpoint1.arr());
					glColor3f(0,0,1);
					glVertex3fv(worldpoint2.arr());
				glEnd();
				glEnable(GL_LIGHTING);
				glPopMatrix();

				m_ShadowVolume.AddVertex(dVertex(worldpoint1,dVector(0,0,0),0,0));
				m_ShadowVolume.AddVertex(dVertex(worldpoint2,dVector(0,0,0),0,0));

				dVector proj = worldpoint2-m_LightPosition;
				m_ShadowVolume.AddVertex(dVertex(worldpoint1+proj*100,dVector(0,0,0),0,0));

				proj = worldpoint1-m_LightPosition;
				m_ShadowVolume.AddVertex(dVertex(worldpoint2+proj*100,dVector(0,0,0),0,0));
			}
		}
	}
}


