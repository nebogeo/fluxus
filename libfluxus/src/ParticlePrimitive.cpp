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
	AddData("s",new TypedPData<dVector>);
	
	// direct access for speed
	PDataDirty();
}

ParticlePrimitive::~ParticlePrimitive()
{
}

void ParticlePrimitive::PDataDirty()
{	
	m_VertData=GetDataVec<dVector>("p");
	m_ColData=GetDataVec<dColour>("c");
	m_SizeData=GetDataVec<dVector>("s");
}

void ParticlePrimitive::Render()
{
	glDisable(GL_LIGHTING);
	if (m_State.Hints & HINT_BOUND) RenderBoundingBox();
	
	if (m_State.Hints & HINT_POINTS)
	{
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
	}
	
	if (m_State.Hints & HINT_SOLID)
	{
		dMatrix ModelView;
		glGetFloatv(GL_MODELVIEW_MATRIX,ModelView.arr());
		ModelView = ModelView.inverse();
		
		dVector CameraDir(0,0,1);
		CameraDir=ModelView.transform_no_trans(CameraDir);
		CameraDir.normalise();
		
		dVector up(0,1,0);
		up = ModelView.transform(up);
		dVector across=up.cross(CameraDir);
		across.normalise();
		dVector down=across.cross(CameraDir);
		down.normalise();
		
		glBegin(GL_QUADS);
		for (unsigned int n=0; n<m_VertData->size(); n++)
		{
			dVector scaledacross(across*(*m_SizeData)[n].x);
			dVector scaledown(down*(*m_SizeData)[n].y);
			glColor3fv((*m_ColData)[n].arr());
			glTexCoord2f(0,0);
			glVertex3fv((*m_VertData)[n].arr());
			glTexCoord2f(0,1);
			glVertex3fv(((*m_VertData)[n]+scaledown).arr());
			glTexCoord2f(1,1);
			glVertex3fv(((*m_VertData)[n]+scaledacross+scaledown).arr());
			glTexCoord2f(1,0);
			glVertex3fv(((*m_VertData)[n]+scaledacross).arr());
		}
		glEnd();
	}
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
}
