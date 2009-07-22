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
#include "BlobbyPrimitive.h"
#include "State.h"
#include "ImplicitSurface.h"

using namespace Fluxus;
	
BlobbyPrimitive::BlobbyPrimitive(int dimx, int dimy, int dimz, dVector size) 
{
	AddData("p",new TypedPData<dVector>);
	AddData("c",new TypedPData<dColour>);
	AddData("s",new TypedPData<float>);
	
	// setup the direct access for speed
	PDataDirty();
	
	float sx=size.x/(float)dimx;
	float sy=size.y/(float)dimy;
	float sz=size.z/(float)dimz;
		
	for (int x=0; x<dimx; x++)
	{
		for (int y=0; y<dimy; y++)
		{
			for (int z=0; z<dimz; z++)
			{
				Cell cell;
			
				cell.p[0]=dVector(sx*x,sy*y+sy,sz*z);
				cell.p[1]=dVector(sx*x,sy*y+sy,sz*z+sz);
				cell.p[2]=dVector(sx*x,sy*y,sz*z+sz);
				cell.p[3]=dVector(sx*x,sy*y,sz*z);
				
				cell.p[4]=dVector(sx*x+sx,sy*y+sy,sz*z);
				cell.p[5]=dVector(sx*x+sx,sy*y+sy,sz*z+sz);
				cell.p[6]=dVector(sx*x+sx,sy*y,sz*z+sz);
				cell.p[7]=dVector(sx*x+sx,sy*y,sz*z);
			
				cell.val[0]=0;
				cell.val[1]=0;
				cell.val[2]=0;
				cell.val[3]=0;
				cell.val[4]=0;
				cell.val[5]=0;
				cell.val[6]=0;
				cell.val[7]=0;
			
				m_Voxels.push_back(cell);
			}
		}
	}
}

BlobbyPrimitive::BlobbyPrimitive(const BlobbyPrimitive &other) :
Primitive(other), 
m_Voxels(other.m_Voxels)
{
	PDataDirty();
}

BlobbyPrimitive* BlobbyPrimitive::Clone() const 
{
	return new BlobbyPrimitive(*this); 
}

BlobbyPrimitive::~BlobbyPrimitive()
{
}

void BlobbyPrimitive::PDataDirty()
{
	// reset pointers
	m_PosData=GetDataVec<dVector>("p");
	m_StrengthData=GetDataVec<float>("s");
	m_ColData=GetDataVec<dColour>("c");
}

void BlobbyPrimitive::AddInfluence(const dVector &Vert, float Strength)
{ 
	m_PosData->push_back(Vert); 
	m_StrengthData->push_back(Strength); 
	m_ColData->push_back(dColour(1,1,1)); 
}	

float BlobbyPrimitive::Sample(const dVector &pos)
{
	float val=0;
	for (unsigned int n=0; n<m_PosData->size(); n++)
	{
		float distance=fabs((*m_PosData)[n].distsq(pos));
		if (distance>0)
		{
			val+=(*m_StrengthData)[n]*(1/distance);
		}
	}		
	return val;	
}

float BlobbyPrimitive::SampleCol(const dVector &pos, dColour &col)
{
	float val=0;
	col=dColour(0,0,0);
	
	for (unsigned int n=0; n<m_PosData->size(); n++)
	{
		float distance=fabs((*m_PosData)[n].distsq(pos));
		if (distance>0)
		{
			float mul=1/distance;
			val+=(*m_StrengthData)[n]*mul;
			
			//col+=(*m_ColData)[n]*mul;

			col.r+=(*m_ColData)[n].r*mul;
			col.g+=(*m_ColData)[n].g*mul;
			col.b+=(*m_ColData)[n].b*mul;
		}
	}		
	return val;	
}

void BlobbyPrimitive::Render()
{
	for (unsigned int i=0; i<m_Voxels.size(); i++)
	{
		for (int c=0; c<8; c++)
		{
			dVector pos = m_Voxels[i].p[c];

			if (m_State.Hints & HINT_VERTCOLS)
			{
				m_Voxels[i].val[c]=SampleCol(pos,m_Voxels[i].col[c]);
			}
			else
			{
				m_Voxels[i].val[c]=Sample(pos);
			}
		}
	}

	if (m_State.Hints & HINT_SPHERE_MAP)
	{
		glEnable(GL_TEXTURE_GEN_S);
		glEnable(GL_TEXTURE_GEN_T);
		glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
		glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
	}

	if (m_State.Hints & HINT_SOLID)
	{
		glBegin(GL_TRIANGLES);
		Draw(1, true, m_State.Hints & HINT_VERTCOLS);
		glEnd();
	}

	if (m_State.Hints & HINT_WIRE)
	{
		glPolygonOffset(1,1);
		glColor4fv(m_State.WireColour.arr());
		glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
		glDisable(GL_LIGHTING);
		if ((m_State.Hints & HINT_WIRE_STIPPLED) > HINT_WIRE)
		{
			glEnable(GL_LINE_STIPPLE);
			glLineStipple(m_State.StippleFactor, m_State.StipplePattern);
		}
		glBegin(GL_TRIANGLES);
		Draw(1, false, false);
		glEnd();
		glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
		glEnable(GL_LIGHTING);
		if ((m_State.Hints & HINT_WIRE_STIPPLED) > HINT_WIRE)
		{
			glDisable(GL_LINE_STIPPLE);
		}
	}

	if (m_State.Hints & HINT_SPHERE_MAP)
	{
		glDisable(GL_TEXTURE_GEN_S);
		glDisable(GL_TEXTURE_GEN_T);
	}
}

void BlobbyPrimitive::RecalculateNormals(bool smooth)
{

}

dBoundingBox BlobbyPrimitive::GetBoundingBox(const dMatrix &space)
{	
	dBoundingBox box;
	for (vector<dVector>::iterator i=m_PosData->begin(); i!=m_PosData->end(); ++i)
	{
		box.expand(space.transform(*i));
	}
	return box;
}

void BlobbyPrimitive::ApplyTransform(bool ScaleRotOnly)
{
	if (!ScaleRotOnly)
	{
		for (vector<dVector>::iterator i=m_PosData->begin(); i!=m_PosData->end(); ++i)
		{
			*i=GetState()->Transform.transform(*i);
			// why not normals?
		}
	}
	else
	{
		for (unsigned int i=0; i<m_PosData->size(); i++)
		{
			(*m_PosData)[i]=GetState()->Transform.transform_no_trans((*m_PosData)[i]);
		}
	}
	
	GetState()->Transform.init();
}

// this implicit surface implementation is modified from Paul Bourke's which can be found here:
// http://astronomy.swin.edu.au/~pbourke/modelling/polygonise/

void BlobbyPrimitive::Draw(float isolevel, bool calcnormals, bool colour)
{
   int i;
   int cubeindex;
   dVertex vertlist[12];

	for (unsigned int cell=0; cell<m_Voxels.size(); cell++)
	{ 
		//  Determine the index into the edge table which
		//  tells us which vertices are inside of the surface
		cubeindex = 0;
		if (m_Voxels[cell].val[0] < isolevel) cubeindex |= 1;
		if (m_Voxels[cell].val[1] < isolevel) cubeindex |= 2;
		if (m_Voxels[cell].val[2] < isolevel) cubeindex |= 4;
		if (m_Voxels[cell].val[3] < isolevel) cubeindex |= 8;
		if (m_Voxels[cell].val[4] < isolevel) cubeindex |= 16;
		if (m_Voxels[cell].val[5] < isolevel) cubeindex |= 32;
		if (m_Voxels[cell].val[6] < isolevel) cubeindex |= 64;
		if (m_Voxels[cell].val[7] < isolevel) cubeindex |= 128;

		// Cube is entirely in/out of the surface 
		if (ImplicitSurfaceEdges[cubeindex] != 0)
		{

			// Find the vertices where the surface intersects the cube
			if (ImplicitSurfaceEdges[cubeindex] & 1) Interpolate(vertlist[0],isolevel,cell,0,1);
			if (ImplicitSurfaceEdges[cubeindex] & 2) Interpolate(vertlist[1],isolevel,cell,1,2);
			if (ImplicitSurfaceEdges[cubeindex] & 4) Interpolate(vertlist[2],isolevel,cell,2,3);
			if (ImplicitSurfaceEdges[cubeindex] & 8) Interpolate(vertlist[3],isolevel,cell,3,0);
			if (ImplicitSurfaceEdges[cubeindex] & 16) Interpolate(vertlist[4],isolevel,cell,4,5);
			if (ImplicitSurfaceEdges[cubeindex] & 32) Interpolate(vertlist[5],isolevel,cell,5,6);
			if (ImplicitSurfaceEdges[cubeindex] & 64) Interpolate(vertlist[6],isolevel,cell,6,7);
			if (ImplicitSurfaceEdges[cubeindex] & 128) Interpolate(vertlist[7],isolevel,cell,7,4);
			if (ImplicitSurfaceEdges[cubeindex] & 256) Interpolate(vertlist[8],isolevel,cell,0,4);
			if (ImplicitSurfaceEdges[cubeindex] & 512) Interpolate(vertlist[9],isolevel,cell,1,5);
			if (ImplicitSurfaceEdges[cubeindex] & 1024) Interpolate(vertlist[10],isolevel,cell,2,6);
			if (ImplicitSurfaceEdges[cubeindex] & 2048) Interpolate(vertlist[11],isolevel,cell,3,7);

			// Create the triangle 
			for (i=0; ImplicitSurfaceTriangles[cubeindex][i]!=-1; i+=3) 
			{
				dVertex a=vertlist[ImplicitSurfaceTriangles[cubeindex][i]];
				dVertex b=vertlist[ImplicitSurfaceTriangles[cubeindex][i+1]];
				dVertex c=vertlist[ImplicitSurfaceTriangles[cubeindex][i+2]];
			
				dVector normal;
				float val=0;
				
				if (calcnormals)
   				{
					val=Sample(a.point);
					normal.x=val-Sample(a.point+dVector(0.001,0,0));
					normal.y=val-Sample(a.point+dVector(0,0.001,0));
					normal.z=val-Sample(a.point+dVector(0,0,0.001));
					normal.normalise();
					glNormal3fv(normal.arr());
				}
				
				if (colour) glColor3fv(a.col.arr());
   				glVertex3fv(a.point.arr());
				
				if (calcnormals)
   				{
					val=Sample(b.point);
					normal.x=val-Sample(b.point+dVector(0.001,0,0));
					normal.y=val-Sample(b.point+dVector(0,0.001,0));
					normal.z=val-Sample(b.point+dVector(0,0,0.001));
					normal.normalise();
					glNormal3fv(normal.arr());
				}
				
 				if (colour) glColor3fv(b.col.arr());
				glVertex3fv(b.point.arr());
				
				if (calcnormals)
   				{
					val=Sample(c.point);
					normal.x=val-Sample(c.point+dVector(0.001,0,0));
					normal.y=val-Sample(c.point+dVector(0,0.001,0));
					normal.z=val-Sample(c.point+dVector(0,0,0.001));
					normal.normalise();
					glNormal3fv(normal.arr());
				}
				
				if (colour) glColor3fv(c.col.arr());
				glVertex3fv(c.point.arr());
			}
		}
	}
}

// generate a poly mesh
void BlobbyPrimitive::ConvertToPoly(PolyPrimitive &poly, float isolevel)
{
	// evaluate the field
	for (unsigned int i=0; i<m_Voxels.size(); i++)
	{
		for (int c=0; c<8; c++)
		{
			dVector pos = m_Voxels[i].p[c];
			
			if (m_State.Hints & HINT_VERTCOLS)
			{
				m_Voxels[i].val[c]=SampleCol(pos,m_Voxels[i].col[c]);
			}
			else
			{
				m_Voxels[i].val[c]=Sample(pos);
			}	
		}
	}
	
   int i;
   int cubeindex;
   dVertex vertlist[12];

	for (unsigned int cell=0; cell<m_Voxels.size(); cell++)
	{ 
		//  Determine the index into the edge table which
		//  tells us which vertices are inside of the surface
		cubeindex = 0;
		if (m_Voxels[cell].val[0] < isolevel) cubeindex |= 1;
		if (m_Voxels[cell].val[1] < isolevel) cubeindex |= 2;
		if (m_Voxels[cell].val[2] < isolevel) cubeindex |= 4;
		if (m_Voxels[cell].val[3] < isolevel) cubeindex |= 8;
		if (m_Voxels[cell].val[4] < isolevel) cubeindex |= 16;
		if (m_Voxels[cell].val[5] < isolevel) cubeindex |= 32;
		if (m_Voxels[cell].val[6] < isolevel) cubeindex |= 64;
		if (m_Voxels[cell].val[7] < isolevel) cubeindex |= 128;

		// Cube is entirely in/out of the surface 
		if (ImplicitSurfaceEdges[cubeindex] != 0)
		{

			// Find the vertices where the surface intersects the cube
			if (ImplicitSurfaceEdges[cubeindex] & 1) Interpolate(vertlist[0],isolevel,cell,0,1);
			if (ImplicitSurfaceEdges[cubeindex] & 2) Interpolate(vertlist[1],isolevel,cell,1,2);
			if (ImplicitSurfaceEdges[cubeindex] & 4) Interpolate(vertlist[2],isolevel,cell,2,3);
			if (ImplicitSurfaceEdges[cubeindex] & 8) Interpolate(vertlist[3],isolevel,cell,3,0);
			if (ImplicitSurfaceEdges[cubeindex] & 16) Interpolate(vertlist[4],isolevel,cell,4,5);
			if (ImplicitSurfaceEdges[cubeindex] & 32) Interpolate(vertlist[5],isolevel,cell,5,6);
			if (ImplicitSurfaceEdges[cubeindex] & 64) Interpolate(vertlist[6],isolevel,cell,6,7);
			if (ImplicitSurfaceEdges[cubeindex] & 128) Interpolate(vertlist[7],isolevel,cell,7,4);
			if (ImplicitSurfaceEdges[cubeindex] & 256) Interpolate(vertlist[8],isolevel,cell,0,4);
			if (ImplicitSurfaceEdges[cubeindex] & 512) Interpolate(vertlist[9],isolevel,cell,1,5);
			if (ImplicitSurfaceEdges[cubeindex] & 1024) Interpolate(vertlist[10],isolevel,cell,2,6);
			if (ImplicitSurfaceEdges[cubeindex] & 2048) Interpolate(vertlist[11],isolevel,cell,3,7);

			// Create the triangle 
			for (i=0; ImplicitSurfaceTriangles[cubeindex][i]!=-1; i+=3) 
			{
				dVertex a=vertlist[ImplicitSurfaceTriangles[cubeindex][i]];
				dVertex b=vertlist[ImplicitSurfaceTriangles[cubeindex][i+1]];
				dVertex c=vertlist[ImplicitSurfaceTriangles[cubeindex][i+2]];
			
				dVector normal;
				float val=0;
				
				val=Sample(a.point);
				normal.x=val-Sample(a.point+dVector(0.001,0,0));
				normal.y=val-Sample(a.point+dVector(0,0.001,0));
				normal.z=val-Sample(a.point+dVector(0,0,0.001));
				normal.normalise();
				
				poly.AddVertex(dVertex(a.point,normal,a.col));
				
				val=Sample(b.point);
				normal.x=val-Sample(b.point+dVector(0.001,0,0));
				normal.y=val-Sample(b.point+dVector(0,0.001,0));
				normal.z=val-Sample(b.point+dVector(0,0,0.001));
				normal.normalise();
				
				poly.AddVertex(dVertex(b.point,normal,b.col));
							
				val=Sample(c.point);
				normal.x=val-Sample(c.point+dVector(0.001,0,0));
				normal.y=val-Sample(c.point+dVector(0,0.001,0));
				normal.z=val-Sample(c.point+dVector(0,0,0.001));
				normal.normalise();
				
				poly.AddVertex(dVertex(c.point,normal,c.col));
			}
		}
	}
}

//   Linearly interpolate the position where an isosurface cuts
//   an edge between two vertices, each with their own scalar value
void BlobbyPrimitive::Interpolate(dVertex &vert, float isolevel, int cell, int a, int b)
{
	float mu;

	dVector posa = m_Voxels[cell].p[a];
	dVector posb = m_Voxels[cell].p[b];
	dColour cola = m_Voxels[cell].col[a];
	dColour colb = m_Voxels[cell].col[b];
	float vala = m_Voxels[cell].val[a];
	float valb = m_Voxels[cell].val[b];


	mu = (isolevel - vala) / (valb - vala);
	vert.point.x = posa.x + mu * (posb.x - posa.x);
	vert.point.y = posa.y + mu * (posb.y - posa.y);
	vert.point.z = posa.z + mu * (posb.z - posa.z);

	vert.col.r = cola.r + mu * (colb.r - cola.r);
	vert.col.g = cola.g + mu * (colb.g - cola.g);
	vert.col.b = cola.b + mu * (colb.b - cola.b);
}
