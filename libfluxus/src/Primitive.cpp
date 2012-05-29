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

#include "Primitive.h"

using namespace Fluxus;

Primitive::SceneInfo Primitive::m_SceneInfo;

Primitive::Primitive() :
m_Visibility(0xffffffff),
m_Selectable(true)
{
}

Primitive::Primitive(const Primitive &other) :
PDataContainer(other),
m_State(other.m_State),
m_Visibility(other.m_Visibility),
m_Selectable(other.m_Selectable)
{
}

Primitive::~Primitive()
{
}

void Primitive::Prerender()
{
	///\todo put other common state things here...
	// (not all, as they are often primitive dependant)
	if (m_State.Hints & HINT_ORIGIN) RenderAxes();
	if (m_State.Hints & HINT_VERTCOLS) glEnable(GL_COLOR_MATERIAL);
	else glDisable(GL_COLOR_MATERIAL);
	if (m_State.Hints & HINT_IGNORE_DEPTH) glDisable(GL_DEPTH_TEST);
	else glEnable(GL_DEPTH_TEST);
	if (m_State.Hints & HINT_BOUND) RenderBoundingBox();

	if (m_State.Shader!=NULL)
	{
		for (map<string,PData*>::iterator i=m_PData.begin(); i!=m_PData.end(); i++)
		{
			TypedPData<dVector> *data = dynamic_cast<TypedPData<dVector>*>(i->second);
			if (data) m_State.Shader->SetVectorAttrib(i->first,data->m_Data);
			else
			{
				TypedPData<dColour> *data = dynamic_cast<TypedPData<dColour>*>(i->second);
				if (data) m_State.Shader->SetColourAttrib(i->first,data->m_Data);
				else
				{
					TypedPData<float> *data = dynamic_cast<TypedPData<float>*>(i->second);
					if (data) m_State.Shader->SetFloatAttrib(i->first,data->m_Data);
				}
			}
		}
	}

}

void Primitive::RenderAxes()
{
	glDisable(GL_LIGHTING);
	glBegin(GL_LINES);
		glColor3f(1,0,0);
		glVertex3f(0,0,0);
		glVertex3f(1,0,0);

		glColor3f(0,1,0);
		glVertex3f(0,0,0);
		glVertex3f(0,1,0);

		glColor3f(0,0,1);
		glVertex3f(0,0,0);
		glVertex3f(0,0,1);
	glEnd();

	/*glColor3f(1, 0, 0);
	glRasterPos3f(1.1, 0.0, 0.0);
	glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, 'x');
	glColor3f(0, 1, 0);
	glRasterPos3f(0.0, 1.1, 0.0);
	glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, 'y');
	glColor3f(0, 0, 1);
	glRasterPos3f(0.0, 0.0, 1.1);
	glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, 'z');*/
	glEnable(GL_LIGHTING);
	glColor4fv(m_State.Colour.arr());
}

void Primitive::RenderBoundingBox()
{
	dMatrix m;
	dBoundingBox b = GetBoundingBox(m);
	glDisable(GL_LIGHTING);
	glBegin(GL_LINES);
	glVertex3f(b.min.x,b.min.y,b.min.z);
	glVertex3f(b.max.x,b.min.y,b.min.z);
	glVertex3f(b.max.x,b.min.y,b.min.z);
	glVertex3f(b.max.x,b.max.y,b.min.z);
	glVertex3f(b.max.x,b.max.y,b.min.z);
	glVertex3f(b.min.x,b.max.y,b.min.z);
	glVertex3f(b.min.x,b.max.y,b.min.z);
	glVertex3f(b.min.x,b.min.y,b.min.z);
	glVertex3f(b.min.x,b.min.y,b.max.z);
	glVertex3f(b.max.x,b.min.y,b.max.z);
	glVertex3f(b.max.x,b.min.y,b.max.z);
	glVertex3f(b.max.x,b.max.y,b.max.z);
	glVertex3f(b.max.x,b.max.y,b.max.z);
	glVertex3f(b.min.x,b.max.y,b.max.z);
	glVertex3f(b.min.x,b.max.y,b.max.z);
	glVertex3f(b.min.x,b.min.y,b.max.z);
	glVertex3f(b.min.x,b.min.y,b.min.z);
	glVertex3f(b.min.x,b.min.y,b.max.z);
	glVertex3f(b.min.x,b.max.y,b.min.z);
	glVertex3f(b.min.x,b.max.y,b.max.z);
	glVertex3f(b.max.x,b.min.y,b.min.z);
	glVertex3f(b.max.x,b.min.y,b.max.z);
	glVertex3f(b.max.x,b.max.y,b.min.z);
	glVertex3f(b.max.x,b.max.y,b.max.z);
	glEnd();
	glEnable(GL_LIGHTING);
}
	
void Primitive::SetSceneInfo(const dVector &dir, const dVector &up) 
{ 
	m_SceneInfo.m_CameraVec=dir; 
	m_SceneInfo.m_CameraUp=up; 
}
			
dVector Primitive::GetLocalCameraDir()
{
	return m_State.Transform.inverse().transform_no_trans(m_SceneInfo.m_CameraVec);
}

dVector Primitive::GetLocalCameraUp()
{
	return m_State.Transform.inverse().transform_no_trans(m_SceneInfo.m_CameraUp);
}
