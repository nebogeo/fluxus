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
#include "PData.h"

using namespace std;

namespace Fluxus
{

//////////////////////////////////////////////////////
/// A texture descriptor
class TextureDesc
{
public:
	TextureDesc() : Format(NONE) {}
	int Width;
	int Height;
	PixelFormat Format;
};

//////////////////////////////////////////////////////
/// The texture painter is resposible for making
/// textures - initialising, loading, generating them 
/// from pdata for the pixel primitive, and uploading 
/// them ready for use. The TexturePainter also contains
/// a cache, so it knows which filenames map to which
/// texture ID's - so it won't load and upload files 
/// which are already on the graphics card.
class TexturePainter
{
public:
	///\todo stop this being a singleton...
	static TexturePainter* Get()
	{
		if (m_Singleton==NULL) m_Singleton=new TexturePainter;
		return m_Singleton;
	}
	
	static void Shutdown()
	{
		if (m_Singleton!=NULL) delete m_Singleton;
	}
	
	/// Initialise all texture units
	void Initialise();
	
	/// Clear the texture cache
	void ClearCache();
		
	////////////////////////////////////
	///@name Texture Generation/Conversion
	///@{
	
	/// Loads a texture, use ignore cache to force it to load. 
	/// Returns the OpenGL ID number 
	unsigned int LoadTexture(const string &Filename, bool ignorecache=false);
	
	/// Loads texture information into a pdata array of colour type
	bool LoadPData(const string &Filename, unsigned int &w, unsigned int &h, TypedPData<dColour> &pixels);
	
	/// Uploads texture data from pdata - returns OpenGL ID number
	unsigned int MakeTexture(unsigned int w, unsigned int h, PData *data);
	///@}
	
	////////////////////////////////////
	///@name State control
	/// Controls the texture rendering state 
	///@{
	
	/// Sets the current texture state - allow settings for each unit if multitexturing is enabled.
	/// The size of ids is expexted to be the same as MAX_TEXTURES
	bool SetCurrent(unsigned int *ids);
	
	/// Disables all texturing
	void DisableAll();
	///@}
	
private:
	TexturePainter();
	~TexturePainter();
	static TexturePainter *m_Singleton;

	map<string,int> m_LoadedMap;
	map<unsigned int,TextureDesc*> m_TextureMap;
};

}

#endif
