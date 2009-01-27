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

#ifndef FLUXUS_SHADER_CACHE
#define FLUXUS_SHADER_CACHE

#include <string>
#include <map>
#include "dada.h"
#include "GLSLShader.h"

namespace Fluxus
{

//////////////////////////////////////////////////////
/// A hardware shader
class ShaderCache
{
public:
	ShaderCache();
	~ShaderCache();
	
	static GLSLShader *Get(const std::string &vert, const std::string &frag);
	static GLSLShader *Make(const std::string &vertsource, const std::string &fragsource);
	static void Clear();
	static void Dump();
	
private:
	static std::map<std::string,GLSLShaderPair *> m_Cache;
};

}

#endif
