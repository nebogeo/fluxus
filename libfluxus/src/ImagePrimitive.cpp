// Copyright (C) 2009 Dave Griffiths
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

#include "OpenGL.h"
#include "Renderer.h"
#include "State.h"
#include "ImagePrimitive.h"

using namespace Fluxus;

ImagePrimitive::ImagePrimitive(Renderer *renderer, unsigned texture,
		float x, float y, unsigned int w, unsigned int h) :
m_Renderer(renderer),
m_Texture(texture),
m_X(x),
m_Y(y),
m_Width(w),
m_Height(h)
{
	AddData("t", new TypedPData<dVector>);

	PDataDirty();

	m_Points.push_back(dVector(0, 1, 0));
	m_Points.push_back(dVector(1, 1, 0));
	m_Points.push_back(dVector(1, 0, 0));
	m_Points.push_back(dVector(0, 0, 0));

	m_TexData->push_back(dVector(1, 0, 0));
	m_TexData->push_back(dVector(0, 0, 0));
	m_TexData->push_back(dVector(0, 1, 0));
	m_TexData->push_back(dVector(1, 1, 0));
}

ImagePrimitive::ImagePrimitive(const ImagePrimitive &other) :
Primitive(other),
m_Points(other.m_Points),
m_TexData(other.m_TexData),
m_Renderer(other.m_Renderer),
m_Texture(other.m_Texture),
m_Width(other.m_Width),
m_Height(other.m_Height)
{
	PDataDirty();
}

ImagePrimitive::~ImagePrimitive()
{
}

ImagePrimitive* ImagePrimitive::Clone() const
{
	return new ImagePrimitive(*this);
}

void ImagePrimitive::PDataDirty()
{
	m_TexData = GetDataVec<dVector>("t");
}

void ImagePrimitive::Render()
{
	int width, height;

	m_Renderer->GetResolution(width, height);

	// set orthographic projection
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    // invert the y axis, down is positive move the origin from the
	// bottom left corner to the upper left corner
    glOrtho(0, width, height, 0, -1, 1);
    glMatrixMode(GL_MODELVIEW);

	// override the state texture and disable depth test
	glEnable(GL_TEXTURE_2D);
	// FIXME: set texture states
	glBindTexture(GL_TEXTURE_2D, m_Texture);

	glDisable(GL_LIGHTING);
	glDisable(GL_DEPTH_TEST);
	glDisable(GL_CULL_FACE);

	glPushMatrix();
	glLoadIdentity();
	glTranslatef(m_X, m_Y, 0.0);
	glScalef(m_Width, m_Height, 1.0);

	glBegin(GL_QUADS);
	glTexCoord2fv((*m_TexData)[0].arr());
	glVertex3fv(m_Points[0].arr());
	glTexCoord2fv((*m_TexData)[1].arr());
	glVertex3fv(m_Points[1].arr());
	glTexCoord2fv((*m_TexData)[2].arr());
	glVertex3fv(m_Points[2].arr());
	glTexCoord2fv((*m_TexData)[3].arr());
	glVertex3fv(m_Points[3].arr());
	glEnd();

	glEnable(GL_LIGHTING);
	glDisable(GL_TEXTURE_2D);
    if (!(m_State.Hints & HINT_IGNORE_DEPTH))
		glEnable(GL_DEPTH_TEST);
	if (m_State.Cull)
		glEnable(GL_CULL_FACE);

	glPopMatrix();
	// set perspective back
    glMatrixMode(GL_PROJECTION);
    glPopMatrix();
    glMatrixMode(GL_MODELVIEW);
}

dBoundingBox ImagePrimitive::GetBoundingBox(const dMatrix &space)
{
	dBoundingBox box;
	for (vector<dVector>::iterator i=m_Points.begin(); i!=m_Points.end(); ++i)
	{
		box.expand(space.transform(*i));
	}
	return box;
}

void ImagePrimitive::ApplyTransform(bool ScaleRotOnly)
{
	if (!ScaleRotOnly)
	{
		for (vector<dVector>::iterator i=m_Points.begin(); i!=m_Points.end(); ++i)
		{
			*i=GetState()->Transform.transform(*i);
		}
	}
	else
	{
		for (unsigned int i=0; i<m_Points.size(); i++)
		{
			m_Points[i]=GetState()->Transform.transform_no_trans(m_Points[i]);
		}
	}

	GetState()->Transform.init();
}

