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
#include "ParticlePrimitive.h"
#include "State.h"

using namespace fluxus;
	
ParticlePrimitive::ParticlePrimitive() 
{
}

ParticlePrimitive::~ParticlePrimitive()
{
}

void ParticlePrimitive::Finalise()
{
}

void ParticlePrimitive::Render()
{
	glDisable(GL_LIGHTING);
	if (m_State.Hints & HINT_BOUND) RenderBoundingBox();

	if (m_State.Hints & HINT_AALIAS) glEnable(GL_POINT_SMOOTH);		
	else glDisable(GL_POINT_SMOOTH);	
	
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_TEXTURE_COORD_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	
	glVertexPointer(3,GL_FLOAT,sizeof(dVector),(void*)m_VertData.begin()->arr());
	glColorPointer(3,GL_FLOAT,sizeof(dVector),(void*)m_ColData.begin()->arr());

	glDrawArrays(GL_POINTS,0,m_VertData.size());

	glDisableClientState(GL_COLOR_ARRAY);
	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_TEXTURE_COORD_ARRAY);
	
	glEnable(GL_LIGHTING);
}

void ParticlePrimitive::SetData(char t, unsigned int i, dVector v)
{
	switch (t)
	{
		case 'p': if (i<m_VertData.size()) m_VertData[i]=v; break;
		case 'c': 
		{
			if (i<m_ColData.size()) 
			{
				m_ColData[i].r=v.x;
				m_ColData[i].g=v.y;
				m_ColData[i].b=v.z;
			}
		}
		break;
		default: break;
	}
}

dVector ParticlePrimitive::GetData(char t, unsigned int i)
{
	dVector ret;
	switch (t)
	{
		case 'p': if (i<m_VertData.size()) ret=m_VertData[i]; break;
		case 'c': 
		{
			if (i<m_ColData.size())
			{
				ret.x=m_ColData[i].r; 
				ret.y=m_ColData[i].g; 
				ret.z=m_ColData[i].b; 
			}
		}
		break;
		default: break;
	}
	return ret;
}

dBoundingBox ParticlePrimitive::GetBoundingBox()
{	
	dBoundingBox box;
	for (vector<dVector>::iterator i=m_VertData.begin();	i!=m_VertData.end(); ++i)
	{
		box.expand(*i);
	}
	return box;
}

void ParticlePrimitive::ApplyTransform(bool ScaleRotOnly)
{
	if (!ScaleRotOnly)
	{
		for (vector<dVector>::iterator i=m_VertData.begin(); i!=m_VertData.end(); ++i)
		{
			*i=GetState()->Transform.transform(*i);
		}
	}
	else
	{
		for (vector<dVector>::iterator i=m_VertData.begin(); i!=m_VertData.end(); ++i)
		{
			*i=GetState()->Transform.transform_no_trans(*i);
		}
	}
	
	GetState()->Transform.init();
	Finalise();
}
