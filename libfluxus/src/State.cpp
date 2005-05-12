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

using namespace fluxus;

State::State() :
Colour(1,1,1),
Shinyness(1.0f),
Opacity(1.0f),
Texture(-1),
Parent(1),
Hints(HINT_SOLID),
LineWidth(1)
{
}

void State::Apply()
{
	glMultMatrixf(Transform.arr());
	Colour.a=Ambient.a=Emissive.a=Specular.a=Opacity;
	glColor3f(Colour.r,Colour.g,Colour.b);
	glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT,Ambient.arr());
	glMaterialfv(GL_FRONT_AND_BACK,GL_EMISSION,Emissive.arr());
	glMaterialfv(GL_FRONT_AND_BACK,GL_DIFFUSE,Colour.arr());
	glMaterialfv(GL_FRONT_AND_BACK,GL_SPECULAR,Specular.arr());
	glMaterialfv(GL_FRONT_AND_BACK,GL_SHININESS,&Shinyness);
	glLineWidth(LineWidth);
	
	if (Texture>=0)
	{ 
		glEnable(GL_TEXTURE_2D);
		TexturePainter::Get()->SetCurrent(Texture);
	}
	else
	{
		glDisable(GL_TEXTURE_2D);
	}
}

void State::Spew()
{
	cerr<<"Colour: "<<Colour<<endl
		<<"Specular: "<<Specular<<endl
		<<"Ambient: "<<Ambient<<endl
		<<"Emissive: "<<Emissive<<endl
		<<"Shinyness: "<<Shinyness<<endl
		<<"Opacity: "<<Opacity<<endl
		<<"Texture: "<<Texture<<endl
		<<"Parent: "<<Parent<<endl
		<<"Hints: "<<Hints<<endl
		<<"LineWidth: "<<LineWidth<<endl
		<<"Transform: "<<Transform<<endl;
}

istream &fluxus::operator>>(istream &s, State &o)
{
	s.ignore(3);
	s.read((char*)o.Colour.arr(),sizeof(float)*4);
	s.read((char*)o.Specular.arr(),sizeof(float)*4);
	s.read((char*)o.Emissive.arr(),sizeof(float)*4);
	s.read((char*)o.Ambient.arr(),sizeof(float)*4);
	s.read((char*)o.Transform.arr(),sizeof(float)*16);
	s.read((char*)&o.Shinyness,sizeof(float));
	s.read((char*)&o.Opacity,sizeof(float));
	s.read((char*)&o.Texture,sizeof(int));
	s.read((char*)&o.Parent,sizeof(int));
	s.read((char*)&o.Hints,sizeof(int));
	s.read((char*)&o.LineWidth,sizeof(float));	
	return s;
}

ostream &fluxus::operator<<(ostream &s, State &o)
{
	s.write("sta",3); 	
	s.write((char*)o.Colour.arr(),sizeof(float)*4);
	s.write((char*)o.Specular.arr(),sizeof(float)*4);
	s.write((char*)o.Emissive.arr(),sizeof(float)*4);
	s.write((char*)o.Ambient.arr(),sizeof(float)*4);
	s.write((char*)o.Transform.arr(),sizeof(float)*16);
	s.write((char*)&o.Shinyness,sizeof(float));
	s.write((char*)&o.Opacity,sizeof(float));
	s.write((char*)&o.Texture,sizeof(int));
	s.write((char*)&o.Parent,sizeof(int));
	s.write((char*)&o.Hints,sizeof(int));
	s.write((char*)&o.LineWidth,sizeof(float));
	return s;
}
