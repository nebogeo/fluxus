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
#include "LinePrimitive.h"
#include "State.h"

using namespace fluxus;

LinePrimitive::LinePrimitive() 
{
}

LinePrimitive::~LinePrimitive()
{
}

void LinePrimitive::Render()
{
	dMatrix ModelView;
	glGetFloatv(GL_MODELVIEW_MATRIX,ModelView.arr());

	if (m_State.Hints & HINT_UNLIT) glDisable(GL_LIGHTING);
	
	dVector CameraDir(0,0,1);
	CameraDir=ModelView.inverse().transform_no_trans(CameraDir);
	CameraDir.normalise();
	
	dVector line=m_End.point-m_Start.point;
	dVector up=line.cross(CameraDir);
	up.normalise();
	
	dVector topnorm=up;
	dVector botnorm=-up;
	
	glBegin(GL_TRIANGLE_STRIP);
	glNormal3fv(CameraDir.arr());
	glTexCoord2f(0,0);
	glNormal3fv(botnorm.arr());
	glVertex3fv((m_Start.point-(up*m_StartWidth)).arr());
	glTexCoord2f(0,1);
	glNormal3fv(topnorm.arr());
	glVertex3fv((m_Start.point+(up*m_StartWidth)).arr());
	glTexCoord2f(1,0);
	glNormal3fv(botnorm.arr());
	glVertex3fv((m_End.point-(up*m_EndWidth)).arr());
	glTexCoord2f(1,1);
	glNormal3fv(topnorm.arr());
	glVertex3fv((m_End.point+(up*m_EndWidth)).arr());
	glEnd();

	if (m_State.Hints & HINT_UNLIT) glEnable(GL_LIGHTING);
}

void LinePrimitive::Finalise()
{
}

dBoundingBox LinePrimitive::GetBoundingBox()
{
	dBoundingBox box;
	box.expand(m_Start.point);
	box.expand(m_End.point);
	return box;
}

void LinePrimitive::ApplyTransform(bool ScaleRotOnly)
{
}

void LinePrimitive::SetStart(const dVertex &Vert, float width)
{
	m_Start=Vert;
	m_StartWidth=width;
}

void LinePrimitive::SetEnd(const dVertex &Vert, float width)
{
	m_End=Vert;
	m_EndWidth=width;
}
