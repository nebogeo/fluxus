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
 
#include "ShaderCache.h"

using namespace Fluxus;
	
std::map<std::string,GLSLShader *> ShaderCache::m_Cache;
	
ShaderCache::ShaderCache()
{
}

ShaderCache::~ShaderCache()
{
	Clear();
}
	
GLSLShader *ShaderCache::Get(const string &vert, const string &frag)
{
	// look in the cache, and copy it if it is there
	string key = vert+" "+frag;
	map<string, GLSLShader *>::iterator i = m_Cache.find(key);
	if (i!=m_Cache.end()) return i->second;
	
	GLSLShader *ret = new GLSLShader(vert,frag);
	m_Cache[key] = ret;
	return ret;
}

void ShaderCache::Clear()
{
	for (map<string, GLSLShader *>::iterator i=m_Cache.begin();
		i!=m_Cache.end(); ++i)
	{
		delete i->second;
	}
	m_Cache.clear();
}

void ShaderCache::Dump()
{
	for (map<string, GLSLShader *>::iterator i=m_Cache.begin();
		i!=m_Cache.end(); ++i)
	{
		Trace::Stream<<i->first<<endl;
	}
	m_Cache.clear();
}
