// Copyright (C) 2008 Dave Griffiths
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

#include "assert.h"
#include "PixelPrimitive.h"
#include "PixelPrimitiveIO.h"
#include "SceneGraph.h"
#include "Trace.h"

using namespace Fluxus;

PixelPrimitiveIO::PixelPrimitiveIO()
{
}

PixelPrimitiveIO::~PixelPrimitiveIO()
{
}

Primitive *PixelPrimitiveIO::FormatRead(const string &filename)
{
	PixelPrimitive *pp = new PixelPrimitive(1, 1);
	pp->Load(filename);
	pp->Upload();
	return pp;
}

bool PixelPrimitiveIO::FormatWrite(const std::string &filename, const Primitive *ob, unsigned id,
		const SceneGraph &world)
{
	const PixelPrimitive *pp = dynamic_cast<const PixelPrimitive *>(ob);

    if (!pp)
    {
        Trace::Stream<<"Can only save images from pixel primitives"<<endl;
        return false;
    }

	pp->Save(filename);
	return true;
}

