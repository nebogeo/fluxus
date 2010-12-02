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

#ifndef FLUX_GEOMETRY_IO
#define FLUX_GEOMETRY_IO

#include <map>
#include "Primitive.h"
#include "SceneGraph.h"

namespace Fluxus
{

class PrimitiveIO
{
public:
	PrimitiveIO();
	virtual ~PrimitiveIO();

	virtual Primitive *FormatRead(const std::string &filename)=0;
	virtual bool FormatWrite(const std::string &filename, const Primitive *ob, unsigned id,
			const SceneGraph &world)=0;

	static Primitive *Read(const std::string &filename, bool cache=true);
	static bool Write(const std::string &filename, const Primitive *ob, unsigned id,
			const SceneGraph &world);
	static void ClearGeometryCache();
	static void Dump();

private:
	static PrimitiveIO *GetFromExtension(const std::string &extension);
	static std::map<std::string, Primitive*> m_GeometryCache;
};

}

#endif
