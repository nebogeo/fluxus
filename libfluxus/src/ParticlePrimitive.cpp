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
	AddData("p",new TypedPData<dVector>);
	AddData("c",new TypedPData<dColour>);
	
	// direct access for speed
	m_VertData=GetDataVec<dVector>("p");
	m_ColData=GetDataVec<dColour>("c");
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

	glDisableClientState(GL_NORMAL_ARRAY);
	glDisableClientState(GL_TEXTURE_COORD_ARRAY);
	glEnableClientState(GL_COLOR_ARRAY);
	
	glVertexPointer(3,GL_FLOAT,sizeof(dVector),(void*)m_VertData->begin()->arr());
	glColorPointer(4,GL_FLOAT,sizeof(dColour),(void*)m_ColData->begin()->arr());
	
	//glEnable(GL_BLEND);
    //glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	//glHint(GL_POINT_SMOOTH_HINT,GL_NICEST);	
	
	if (m_State.Hints & HINT_AALIAS) glEnable(GL_POINT_SMOOTH);	
	else glDisable(GL_POINT_SMOOTH);	

	glDrawArrays(GL_POINTS,0,m_VertData->size());

	glDisableClientState(GL_COLOR_ARRAY);
	glEnableClientState(GL_NORMAL_ARRAY);
	glEnableClientState(GL_TEXTURE_COORD_ARRAY);
	
	glEnable(GL_LIGHTING);
}

dBoundingBox ParticlePrimitive::GetBoundingBox()
{	
	dBoundingBox box;
	for (vector<dVector>::iterator i=m_VertData->begin(); i!=m_VertData->end(); ++i)
	{
		box.expand(*i);
	}
	return box;
}

void ParticlePrimitive::ApplyTransform(bool ScaleRotOnly)
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
	Finalise();
}
