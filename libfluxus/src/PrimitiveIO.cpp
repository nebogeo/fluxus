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
 
#include "PrimitiveIO.h"
#include "OBJPrimitiveIO.h"
#include "PixelPrimitiveIO.h"
#include "SceneGraph.h"

using namespace Fluxus;
	
map<string, Primitive*> PrimitiveIO::m_GeometryCache;

PrimitiveIO::PrimitiveIO()
{
}

PrimitiveIO::~PrimitiveIO()
{	
	ClearGeometryCache();
}
	
Primitive *PrimitiveIO::Read(const string &filename, bool cache)
{
	// look in the cache, and copy it if it is there
	map<string, Primitive*>::iterator i = m_GeometryCache.find(filename);
	if (i!=m_GeometryCache.end()) return i->second->Clone();
	
	// otherwise, we need to load it...
	string extension = filename.substr(filename.find_last_of('.')+1,filename.size());
	PrimitiveIO *pio = GetFromExtension(extension);
	Primitive *prim = NULL;
	if (pio!=NULL)
	{
		prim = pio->FormatRead(filename);
	}
	delete pio;
	
	if (prim==NULL) return NULL;
	if (!cache) return prim;
	m_GeometryCache[filename]=prim;
	return prim->Clone();
}

bool PrimitiveIO::Write(const std::string &filename, const Primitive *ob, unsigned id,
		const SceneGraph &world)
{
	string extension = filename.substr(filename.find_last_of('.') + 1, filename.size());
	PrimitiveIO *pio = GetFromExtension(extension);
	Primitive *prim = NULL;
	bool ret = false;
	if (pio != NULL)
	{
		ret = pio->FormatWrite(filename, ob, id, world);
	}
	delete pio;
	return ret;
}

PrimitiveIO *PrimitiveIO::GetFromExtension(const string &extension)
{
	if (extension=="obj") return new OBJPrimitiveIO;
	else if (extension=="png") return new PixelPrimitiveIO;
	return NULL;
}

void PrimitiveIO::ClearGeometryCache()
{
	for (map<string, Primitive*>::iterator i=m_GeometryCache.begin();
		i!=m_GeometryCache.end(); ++i)
	{
		delete i->second;
	}
	m_GeometryCache.clear();
}

void PrimitiveIO::Dump()
{
	for (map<string, Primitive*>::iterator i=m_GeometryCache.begin();
		i!=m_GeometryCache.end(); ++i)
	{
		Trace::Stream<<i->first<<endl;
	}
}
