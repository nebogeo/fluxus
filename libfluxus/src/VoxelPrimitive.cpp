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
#include "VoxelPrimitive.h"
#include "State.h"

using namespace Fluxus;

VoxelPrimitive::VoxelPrimitive(unsigned int w, unsigned int h, unsigned int d)
{
	AddData("c",new TypedPData<dColour>(w*h*d));
	AddData("g",new TypedPData<dColour>(w*h*d));
	m_Width=w;
	m_Height=h;
	m_Depth=d;
	// direct access for speed
	PDataDirty();
}

VoxelPrimitive::VoxelPrimitive(const VoxelPrimitive &other) :
Primitive(other)
{
	PDataDirty();
}

VoxelPrimitive::~VoxelPrimitive()
{
}

VoxelPrimitive* VoxelPrimitive::Clone() const
{
	return new VoxelPrimitive(*this);
}

void VoxelPrimitive::PDataDirty()
{
	m_ColData=GetDataVec<dColour>("c");
	m_GradData=GetDataVec<dColour>("g");
}

unsigned int VoxelPrimitive::Index(unsigned int x, unsigned int y, unsigned int z)
{
	return x + y*m_Width + z*m_Width*m_Height;
}

dVector VoxelPrimitive::Position(unsigned int index)
{
	return dVector(index%m_Width, 
				   (index/m_Width)%m_Height, 
				   index/(m_Width*m_Height))/m_Width;
}

dColour VoxelPrimitive::SafeRef(unsigned int x, unsigned int y, unsigned int z)
{
	if (x>0 && x<m_Width && y>0 && y<m_Height && z>0 && z<m_Depth)
	{
		return (*m_ColData)[Index(x,y,z)];
	}
	return dColour(0,0,0);
}

void VoxelPrimitive::CalcGradient()
{
	for (unsigned int x=0; x<m_Width; x++)
	{
		for (unsigned int y=0; y<m_Height; y++)
		{
			for (unsigned int z=0; z<m_Depth; z++)
			{
				(*m_GradData)[Index(x,y,z)]=dColour(SafeRef(x-1,y,z).r-SafeRef(x+1,y,z).r,
					SafeRef(x,y-1,z).g-SafeRef(x,y+1,z).g,
					SafeRef(x,y,z-1).b-SafeRef(x,y,z+1).b);
			}	
		}
	}
}

void VoxelPrimitive::SphereInfluence(const dVector &pos, const dColour &col, float pow)
{
	for (unsigned int i=0; i<m_Width*m_Height*m_Depth; i++)
	{
		(*m_ColData)[i]+=col*powf(1/Position(i).dist(pos),pow);
	}
}

void VoxelPrimitive::SphereSolid(const dVector &pos, const dColour &col, float radius)
{
	for (unsigned int i=0; i<m_Width*m_Height*m_Depth; i++)
	{
		if (Position(i).dist(pos)<radius) (*m_ColData)[i]=col;
	}
}

void VoxelPrimitive::BoxSolid(const dVector &topleft, const dVector &botright, const dColour &col)
{
	for (unsigned int i=0; i<m_Width*m_Height*m_Depth; i++)
	{
		dVector pos=Position(i);
		if (pos>topleft && pos<botright) (*m_ColData)[i]=col;
	}
}

void VoxelPrimitive::Threshold(float value)
{
	for (unsigned int i=0; i<m_Width*m_Height*m_Depth; i++)
	{
		if ((*m_ColData)[i].mag()<value)
		{
			(*m_ColData)[i]=dColour(0,0,0,0);
		}
		else
		{
			(*m_ColData)[i]=dColour(1,1,1,1);
		}
	}
}

void VoxelPrimitive::PointLight(dVector lightpos, dColour col)
{
	for (unsigned int i=0; i<m_Width*m_Height*m_Depth; i++)
	{
		dVector *n=reinterpret_cast<dVector*>(&(*m_GradData)[i]);
		float lambert = n->dot(lightpos-Position(i));
		if (lambert>0) (*m_ColData)[i]+=col*lambert;
		else (*m_ColData)[i]*=0.1; // ambient...
	}	
}
	
void VoxelPrimitive::Render()
{
	glDisable(GL_LIGHTING);

	if (m_State.Hints & HINT_SOLID)
	{
		dVector cameradir=GetLocalCameraDir();
		dVector across=GetLocalCameraUp().cross(cameradir);
		across.normalise();
		dVector down=across.cross(cameradir);
		down.normalise();
		across/=m_Width;
		down/=m_Width;
		
		glBegin(GL_QUADS);
		for (unsigned int n=0; n<m_ColData->size(); n++)
		{
			if ((*m_ColData)[n].a>0.001)
			{		
				dVector p(n%m_Width,n/m_Width%m_Height,n/(m_Width*m_Height));
				p/=m_Width;
				glColor4fv((*m_ColData)[n].arr());
				glTexCoord2f(0,0);
				glVertex3fv((p-across-down).arr());
				glTexCoord2f(0,1);
				glVertex3fv((p-across+down).arr());
				glTexCoord2f(1,1);
				glVertex3fv((p+across+down).arr());
				glTexCoord2f(1,0);
				glVertex3fv((p+across-down).arr());
			}
		}
		glEnd();
	}
	glEnable(GL_LIGHTING);
}

dBoundingBox VoxelPrimitive::GetBoundingBox(const dMatrix &space)
{
	dBoundingBox box;
	//for (vector<dVector>::iterator i=m_VertData->begin(); i!=m_VertData->end(); ++i)
	//{
	//	box.expand(space.transform(*i));
	//}
	return box;
}

void VoxelPrimitive::ApplyTransform(bool ScaleRotOnly)
{
	/*if (!ScaleRotOnly)
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
	}*/

	GetState()->Transform.init();
}
