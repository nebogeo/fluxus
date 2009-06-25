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

#ifndef N_IMAGEPRIM
#define N_IMAGEPRIM

#include "Primitive.h"
#include "Renderer.h"

namespace Fluxus
{

class ImagePrimitive : public Primitive
{
public:
	ImagePrimitive(Renderer *renderer, unsigned texture,
				   float x, float y, unsigned int w, unsigned int h);
	ImagePrimitive(const ImagePrimitive &other);
	virtual ~ImagePrimitive();

	virtual ImagePrimitive* Clone() const;

	virtual void Render();
	virtual dBoundingBox GetBoundingBox(const dMatrix &space);
	virtual void ApplyTransform(bool ScaleRotOnly=false);
	virtual string GetTypeName() { return "ImagePrimitive"; }
	virtual Evaluator *MakeEvaluator() { return NULL; }

protected:

	virtual void PDataDirty();

	vector<dVector> m_Points;
	vector<dVector> *m_TexData;

	Renderer *m_Renderer;

	unsigned int m_Texture;

	float m_X;
	float m_Y;
	unsigned int m_Width;
	unsigned int m_Height;
};

};

#endif

