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

#ifndef N_TEXTURE
#define N_TEXTURE

#include <iostream>
#include <string>
#include <map>
#include "PNGLoader.h"

using namespace std;

namespace fluxus
{

class TextureDesc
{
public:
	TextureDesc() : Format(NONE) {}
	int Width;
	int Height;
	PixelFormat Format;
};

class TexturePainter
{
public:
	TexturePainter();
	~TexturePainter();
	
	int LoadTexture(const string &Filename);
	void SetCurrent(int id);
	
private:

	map<string,int> m_LoadedMap;
	map<unsigned int,TextureDesc*> m_TextureMap;
};

}

#endif
