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
#include "OpenGL.h"
#include "PData.h"

using namespace std;

namespace Fluxus
{

	class DDSLoader;

//////////////////////////////////////////////////////
/// The texture state
class TextureState
{
public:

	TextureState(): TexEnv(GL_MODULATE), Min(GL_LINEAR_MIPMAP_LINEAR),
	Mag(GL_LINEAR), WrapS(GL_REPEAT), WrapT(GL_REPEAT), WrapR(GL_REPEAT),
	Priority(1), MinLOD(-1000), MaxLOD(1000) {}

	int TexEnv;
	int Min;
	int Mag;
	int WrapS;
	int WrapT;
	int WrapR;
	dColour BorderColour;
	float Priority;
	dColour EnvColour;
	float MinLOD;
	float MaxLOD;
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
	friend class PNGLoader;
	friend class DDSLoader;

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

	///////////////////////////////////
	/// Options for texture creation
	class CreateParams
	{
		public:
		CreateParams(): ID(-1), Type(GL_TEXTURE_2D), GenerateMipmaps(true), MipLevel(0), Border(0), Compress(false) {}

		int ID;
		int Type;
		bool GenerateMipmaps;
		int MipLevel;
		int Border;
		bool Compress;
	};

	////////////////////////////////////
	///@name Texture Generation/Conversion
	///@{

	/// Loads a texture returns the OpenGL ID number
	unsigned int LoadTexture(const string &Filename, CreateParams &params);

	/// Loads texture information into a pdata array of colour type
	bool LoadPData(const string &Filename, unsigned int &w, unsigned int &h, TypedPData<dColour> &pixels);

	/// Saves texture information from a pdata array
	bool SavePData(const string &Filename, unsigned int w, unsigned int h, const TypedPData<dColour> &pixels);

	/// Uploads texture data from pdata - returns OpenGL ID number
	unsigned int MakeTexture(unsigned int w, unsigned int h, PData *data);

	/// Checks if a texture is in high-performance memory
	bool IsResident(unsigned int id);

	/// Hints to decide texture residency
	void SetTexturePriority(unsigned int id, float priority);

	/// Returns texture width
	unsigned GetTextureWidth(unsigned int id);

	/// Returns texture height
	unsigned GetTextureHeight(unsigned int id);

	///@}

	////////////////////////////////////
	///@name State control
	/// Controls the texture rendering state
	///@{

	/// Sets the current texture state - allow settings for each unit if multitexturing is enabled.
	/// The size of ids is expected to be the same as MAX_TEXTURES
	bool SetCurrent(unsigned int *ids, TextureState *states);

	/// Disables all texturing
	void DisableAll();

	/// Print out information
	void Dump();

	/// Is multitexturing possible?
	bool MultitexturingEnabled() { return m_MultitexturingEnabled; }
	///@}

private:
	//////////////////////////////////////////////////////
	/// A texture descriptor
	class TextureDesc
	{
	public:
		TextureDesc() : Format(0), ImageData(NULL) {}

		unsigned int Width;
		unsigned int Height;
		int InternalFormat; // number of colour components (GL_RGB, GL_RGBA, GL_COMPRESSED..., etc)
		int Format; // pixel data format
		int Size; // pixel data size
		unsigned char *ImageData;
	};

	//////////////////////////////////////////////////////
	/// We need to group together the cube map ids, so we
	/// know which id's to use when the primary one gets set
	class CubeMapDesc
	{
	public:
		CubeMapDesc() { Positive[0]=0; Positive[1]=0; Positive[2]=0;
		                Negative[0]=0; Negative[1]=0; Negative[2]=0; }
		unsigned int Positive[3];
		unsigned int Negative[3];
	};

	TexturePainter();
	~TexturePainter();
	void ApplyState(int type, TextureState &state, bool cubemap);
	unsigned int LoadCubeMap(const string &Fullpath, CreateParams &params);
	void UploadTexture(TextureDesc desc, CreateParams params);
	static TexturePainter *m_Singleton;

	map<string,int> m_LoadedMap;
	map<string,int> m_LoadedCubeMap;
	map<unsigned int,TextureDesc> m_TextureMap;
	map<unsigned int,CubeMapDesc> m_CubeMapMap;
	bool m_MultitexturingEnabled;
	bool m_TextureCompressionEnabled;
	bool m_SGISGenerateMipmap;
};

}

#endif
