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
#include "TexturePainter.h"
#include "State.h"

using namespace Fluxus;

State::State() :
Colour(1,1,1),
Shinyness(1.0f),
Opacity(1.0f),
Parent(1),
Hints(HINT_SOLID),
LineWidth(1),
PointWidth(1),
SourceBlend(GL_SRC_ALPHA),
DestinationBlend(GL_ONE_MINUS_SRC_ALPHA),
WireColour(1,1,1),
WireOpacity(1.0f),
Shader(NULL)
{
	for (int c=0; c<MAX_TEXTURES; c++)
	{
		Textures[c]=0;
	}
}

void State::Apply()
{
	glMultMatrixf(Transform.arr());
	Colour.a=Ambient.a=Emissive.a=Specular.a=Opacity;
	WireColour.a=WireOpacity;
	glColor4f(Colour.r,Colour.g,Colour.b,Colour.a);
	glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,Ambient.arr());
	glMaterialfv(GL_FRONT_AND_BACK,GL_EMISSION,Emissive.arr());
	glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,Colour.arr());
	glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,Specular.arr());
	glMaterialfv(GL_FRONT_AND_BACK,GL_SHININESS,&Shinyness);
	glLineWidth(LineWidth);
	glPointSize(PointWidth);
	glBlendFunc(SourceBlend,DestinationBlend);

	TexturePainter::Get()->SetCurrent(Textures);

	if (Shader)
	{
		Shader->Apply();
	}
	else
	{
		GLSLShader::Unapply();
	}
}

void State::Spew()
{
	Trace::Stream<<"Colour: "<<Colour<<endl
		<<"Specular: "<<Specular<<endl
		<<"Ambient: "<<Ambient<<endl
		<<"Emissive: "<<Emissive<<endl
		<<"Shinyness: "<<Shinyness<<endl
		<<"Opacity: "<<Opacity<<endl
		<<"WireOpacity: "<<WireOpacity<<endl
		<<"Texture: "<<Textures[0]<<endl
		<<"Parent: "<<Parent<<endl
		<<"Hints: "<<Hints<<endl
		<<"LineWidth: "<<LineWidth<<endl
		<<"Transform: "<<Transform<<endl;
}

