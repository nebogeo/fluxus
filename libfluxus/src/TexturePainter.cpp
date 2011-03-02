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

#include "OpenGL.h"
#include "State.h"
#include "TexturePainter.h"
#include "PNGLoader.h"
#include "DDSLoader.h"
#include "SearchPaths.h"
#include <assert.h>

using namespace Fluxus;

TexturePainter *TexturePainter::m_Singleton=NULL;

TexturePainter::TexturePainter() :
m_MultitexturingEnabled(true),
m_TextureCompressionEnabled(true),
m_SGISGenerateMipmap(true)
{
	if (glewInit() != GLEW_OK)
	{
		cerr << "ERROR Unable to check OpenGL extensions" << endl;
	}

	#ifndef DISABLE_MULTITEXTURE
	if (!GLEW_ARB_multitexture || (glActiveTexture == NULL) || (glClientActiveTexture == NULL))
	{
		Trace::Stream<<"Warning: Can't do multitexturing (no glActiveTexture or GLEW_ARB_multitexture not set)"<<endl;
		m_MultitexturingEnabled=false;
	}
	#else
	m_MultitexturingEnabled=false;
	#endif

	if (!GLEW_EXT_texture_compression_s3tc || !GLEW_ARB_texture_compression ||
			(glCompressedTexImage2DARB == NULL))
	{
		Trace::Stream << "Warning: Texture compression disabled." << endl;
		m_TextureCompressionEnabled = false;
	}
	if (!GLEW_SGIS_generate_mipmap) // required for texture mipmap compression
	{
		Trace::Stream << "Warning: Automatic mipmap generation disabled." << endl;
		m_SGISGenerateMipmap = false;
	}
}

TexturePainter::~TexturePainter()
{
	///\todo Shouldn't we delete all textures here?
}

void TexturePainter::Initialise()
{
#ifndef DISABLE_MULTITEXTURE
	if (m_MultitexturingEnabled)
	{
		for (int c=0; c<MAX_TEXTURES; c++)
		{
			glActiveTexture(GL_TEXTURE0+c);
			glMatrixMode(GL_TEXTURE);
			glLoadIdentity();
		}
	}
	else
#endif
	{
		glEnableClientState(GL_TEXTURE_COORD_ARRAY);
		glMatrixMode(GL_TEXTURE);
		glLoadIdentity();
	}
}

void TexturePainter::ClearCache()
{
	m_TextureMap.clear();
	m_LoadedMap.clear();
	m_LoadedCubeMap.clear();
}

unsigned int TexturePainter::LoadTexture(const string &Filename, CreateParams &params)
{
	string Fullpath = SearchPaths::Get()->GetFullPath(Filename);
	if (params.Type==GL_TEXTURE_CUBE_MAP_POSITIVE_X || params.Type==GL_TEXTURE_CUBE_MAP_NEGATIVE_X ||
		params.Type==GL_TEXTURE_CUBE_MAP_POSITIVE_Y || params.Type==GL_TEXTURE_CUBE_MAP_NEGATIVE_Y ||
		params.Type==GL_TEXTURE_CUBE_MAP_POSITIVE_Z || params.Type==GL_TEXTURE_CUBE_MAP_NEGATIVE_Z)
	{
		return LoadCubeMap(Fullpath, params);
	}

	// see if we've loaded this one already
	map<string,int>::iterator i=m_LoadedMap.find(Fullpath);
	if (i!=m_LoadedMap.end())
	{
		return i->second;
	}

	TextureDesc desc;
	string extension = Filename.substr(Filename.find_last_of('.') + 1, Filename.size());

	vector<TextureDesc> mipmaps;

	if (extension == "dds")
		DDSLoader::Load(Fullpath, desc, mipmaps);
	else
		PNGLoader::Load(Fullpath, desc);

	if (desc.ImageData!=NULL)
	{
		//\todo support more depths than 8bit

		// upload to card...
		glEnable(params.Type);

		if (params.ID==-1) // is this a new texture?
		{
			GLuint id;
			glGenTextures(1,&id);
			params.ID=id; // ahem
			//\todo this means mipmap levels won't be cached
			m_TextureMap[params.ID]=desc;
			m_LoadedMap[Fullpath]=params.ID;
		}

		UploadTexture(desc,params);

		// upload mipmaps from compressed textures
		if (!mipmaps.empty() && params.GenerateMipmaps)
		{
			for (unsigned i = 0; i < mipmaps.size(); i++)
			{
				params.MipLevel = i + 1;
				UploadTexture(mipmaps[i], params);
			}
		}

		delete [] desc.ImageData;
		for (unsigned i = 0; i < mipmaps.size(); i++)
		{
			delete [] mipmaps[i].ImageData;
		}
		mipmaps.clear();

		return params.ID;
	}
	return 0;
}

unsigned int TexturePainter::LoadCubeMap(const string &Fullpath, CreateParams &params)
{
	// see if we've loaded this one already
	map<string,int>::iterator i=m_LoadedCubeMap.find(Fullpath);
	if (i!=m_LoadedCubeMap.end())
	{
		return i->second;
	}

	TextureDesc desc;
	PNGLoader::Load(Fullpath, desc);

	if (desc.ImageData!=NULL)
	{
		//\todo support more depths than 8bit

		// upload to card...
		glEnable(params.Type);

		if (params.ID==-1) // is this a new texture?
		{
			GLuint id;
			glGenTextures(1,&id);
			params.ID=id; // ahem
			//\todo this means cubemaps won't be cached
			m_TextureMap[params.ID]=desc;
			m_LoadedCubeMap[Fullpath]=params.ID;

			CubeMapDesc newcubemap;
			m_CubeMapMap[params.ID] = newcubemap;

			switch (params.Type) // record the cubemap face
			{
				case GL_TEXTURE_CUBE_MAP_POSITIVE_X: m_CubeMapMap[params.ID].Positive[0]=params.ID; break;
				case GL_TEXTURE_CUBE_MAP_NEGATIVE_X: m_CubeMapMap[params.ID].Negative[0]=params.ID; break;
				case GL_TEXTURE_CUBE_MAP_POSITIVE_Y: m_CubeMapMap[params.ID].Positive[1]=params.ID; break;
				case GL_TEXTURE_CUBE_MAP_NEGATIVE_Y: m_CubeMapMap[params.ID].Negative[1]=params.ID; break;
				case GL_TEXTURE_CUBE_MAP_POSITIVE_Z: m_CubeMapMap[params.ID].Positive[2]=params.ID; break;
				case GL_TEXTURE_CUBE_MAP_NEGATIVE_Z: m_CubeMapMap[params.ID].Negative[2]=params.ID; break;
				default: assert(0); break;
			}
		}
		else // we have an existing texture id specified
		{
			// make a new texture id for this face
			// record the primary cubemap id
			unsigned int primary = params.ID;
			GLuint id;
			glGenTextures(1,&id);
			params.ID=id; // ahem
			m_TextureMap[params.ID]=desc;
			m_LoadedCubeMap[Fullpath]=params.ID;

			switch (params.Type) // record the cubemap face
			{
				case GL_TEXTURE_CUBE_MAP_POSITIVE_X: m_CubeMapMap[primary].Positive[0]=params.ID; break;
				case GL_TEXTURE_CUBE_MAP_NEGATIVE_X: m_CubeMapMap[primary].Negative[0]=params.ID; break;
				case GL_TEXTURE_CUBE_MAP_POSITIVE_Y: m_CubeMapMap[primary].Positive[1]=params.ID; break;
				case GL_TEXTURE_CUBE_MAP_NEGATIVE_Y: m_CubeMapMap[primary].Negative[1]=params.ID; break;
				case GL_TEXTURE_CUBE_MAP_POSITIVE_Z: m_CubeMapMap[primary].Positive[2]=params.ID; break;
				case GL_TEXTURE_CUBE_MAP_NEGATIVE_Z: m_CubeMapMap[primary].Negative[2]=params.ID; break;
				default: assert(0); break;
			}
		}

		UploadTexture(desc,params);
		delete[] desc.ImageData;
		return params.ID;
	}
	m_LoadedMap[Fullpath]=0;
	return 0;
}

void TexturePainter::UploadTexture(TextureDesc desc, CreateParams params)
{
	glBindTexture(params.Type,params.ID);

	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

	if (params.Compress && m_TextureCompressionEnabled)
	{
		switch (desc.InternalFormat)
		{
			case GL_RGB:
				desc.InternalFormat = GL_COMPRESSED_RGB_S3TC_DXT1_EXT;
				break;
			case GL_RGBA:
				desc.InternalFormat = GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
				break;
		}

		if (params.GenerateMipmaps && m_SGISGenerateMipmap)
		{
			glTexParameteri(params.Type, GL_GENERATE_MIPMAP_SGIS, 1);
		}

		glTexImage2D(params.Type, params.MipLevel, desc.InternalFormat,
				desc.Width, desc.Height, params.Border,
				desc.Format, GL_UNSIGNED_BYTE, desc.ImageData);
	}
	else
	if ((desc.InternalFormat == GL_RGB) || (desc.InternalFormat == GL_RGBA))
	{
		if (params.GenerateMipmaps)
		{
			gluBuild2DMipmaps(params.Type, desc.InternalFormat, desc.Width, desc.Height, desc.Format, GL_UNSIGNED_BYTE, desc.ImageData);
		}
		else
		{
			//\todo check power of two
			//\todo and scale?
			glTexImage2D(params.Type, params.MipLevel, desc.InternalFormat, desc.Width, desc.Height, params.Border,
					desc.Format, GL_UNSIGNED_BYTE, desc.ImageData);
		}
	}
	else
	{
		if (m_TextureCompressionEnabled)
		{
			glCompressedTexImage2DARB(params.Type, params.MipLevel,
					desc.InternalFormat, desc.Width, desc.Height, params.Border,
					desc.Size, desc.ImageData);
		}
		else
		{
			Trace::Stream << "Couldn't upload image, Texture compression is not supported."<<endl;
		}
	}
}

///todo: all pdata texture load/save is to 8 bit RGB or RGBA - need to deal with arbitrary channels and bit depths
bool TexturePainter::LoadPData(const string &Filename, unsigned int &w, unsigned int &h, TypedPData<dColour> &pixels)
{
	string Fullpath = SearchPaths::Get()->GetFullPath(Filename);

	TextureDesc desc;
	PNGLoader::Load(Fullpath, desc);

	if (desc.ImageData!=NULL)
	{
		pixels.Resize(desc.Width*desc.Height);
		w=desc.Width;
		h=desc.Height;

		if (desc.Format==GL_RGB)
		{
			for (unsigned int n=0; n<desc.Width*desc.Height; n++)
			{
				pixels.m_Data[n]=dColour(desc.ImageData[n*3]/255.0f,
				                         desc.ImageData[n*3+1]/255.0f,
										 desc.ImageData[n*3+2]/255.0f,1.0f);

			}
		}
		else if (desc.Format==GL_RGBA)
		{
			for (unsigned int n=0; n<desc.Width*desc.Height; n++)
			{
				pixels.m_Data[n]=dColour(desc.ImageData[n*4]/255.0f,
				                         desc.ImageData[n*4+1]/255.0f,
										 desc.ImageData[n*4+2]/255.0f,
										 desc.ImageData[n*4+3]/255.0f);

			}
		}
		else
		{
			delete[] desc.ImageData;
			return false;
		}

		delete[] desc.ImageData;
		return true;
	}

	return false;
}

bool TexturePainter::SavePData(const string &Filename, unsigned int w, unsigned int h, const TypedPData<dColour> &pixels)
{
	// save as 8bit rgba
	unsigned char *ImageData=new unsigned char[w*h*4];
	for (unsigned int n=0; n<w*h; n++)
	{
		ImageData[n*4]=(unsigned char)(pixels.m_Data[n].r*255.0f);
		ImageData[n*4+1]=(unsigned char)(pixels.m_Data[n].g*255.0f);
		ImageData[n*4+2]=(unsigned char)(pixels.m_Data[n].b*255.0f);
		ImageData[n*4+3]=(unsigned char)(pixels.m_Data[n].a*255.0f);
	}

	PNGLoader::Save(Filename, w, h, GL_RGBA, ImageData);

	delete[] ImageData;
	return true;
}

unsigned int TexturePainter::MakeTexture(unsigned int w, unsigned int h, PData *data)
{
	GLuint ID=0;
	TypedPData<dColour> *pixels = dynamic_cast<TypedPData<dColour>*>(data);
	if (pixels)
	{
		// upload to card...
		glGenTextures(1,&ID);
		glBindTexture(GL_TEXTURE_2D,ID);
		gluBuild2DMipmaps(GL_TEXTURE_2D,4,w,h,GL_RGBA,GL_FLOAT,&pixels->m_Data[0]);
		return ID;
	}
	return 0;
}

bool TexturePainter::SetCurrent(unsigned int *ids, TextureState *states)
{
	bool ret=false;

	int tcount = (m_MultitexturingEnabled ? MAX_TEXTURES : 1);
	for (int c = 0; c < tcount; c++)
	{
	#ifndef DISABLE_MULTITEXTURE
		if (m_MultitexturingEnabled)
		{
			glActiveTexture(GL_TEXTURE0+c);
		}
	#endif

		if (ids[c]!=0)
		{
			map<unsigned int,CubeMapDesc>::iterator i=m_CubeMapMap.find(ids[c]);

			if (i!=m_CubeMapMap.end()) // cubemap texture path
			{
				glBindTexture(GL_TEXTURE_CUBE_MAP_POSITIVE_X,i->second.Positive[0]);
				glBindTexture(GL_TEXTURE_CUBE_MAP_NEGATIVE_X,i->second.Negative[0]);
				glBindTexture(GL_TEXTURE_CUBE_MAP_POSITIVE_Y,i->second.Positive[1]);
				glBindTexture(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y,i->second.Negative[1]);
				glBindTexture(GL_TEXTURE_CUBE_MAP_POSITIVE_Z,i->second.Positive[2]);
				glBindTexture(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z,i->second.Negative[2]);

				glEnable(GL_TEXTURE_CUBE_MAP);
				ApplyState(GL_TEXTURE_CUBE_MAP,states[c],true);
			}
			else // normal 2D texture path
			{
				glEnable(GL_TEXTURE_2D);
				glBindTexture(GL_TEXTURE_2D,ids[c]);
				ApplyState(GL_TEXTURE_2D,states[c],false);
			}

			ret=true;
		}
		else
		{
			glDisable(GL_TEXTURE_2D);
			glDisable(GL_TEXTURE_CUBE_MAP);
		}
	}

	#ifndef DISABLE_MULTITEXTURE
	if (m_MultitexturingEnabled)
	{
		glActiveTexture(GL_TEXTURE0);
	}
	#endif

	return ret;
}

void TexturePainter::ApplyState(int type, TextureState &state, bool cubemap)
{
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, state.TexEnv);
	glTexEnvfv(GL_TEXTURE_ENV, GL_TEXTURE_ENV_COLOR, state.EnvColour.arr());
	glTexParameteri(type, GL_TEXTURE_MIN_FILTER, state.Min);
	glTexParameteri(type, GL_TEXTURE_MAG_FILTER, state.Mag);
	glTexParameteri(type, GL_TEXTURE_WRAP_S, state.WrapS);
	glTexParameteri(type, GL_TEXTURE_WRAP_T, state.WrapT);
	if (cubemap) glTexParameteri(type, GL_TEXTURE_WRAP_R, state.WrapR);
	glTexParameterfv(type, GL_TEXTURE_BORDER_COLOR, state.BorderColour.arr());
	glTexParameterf(type, GL_TEXTURE_PRIORITY, state.Priority);
	glTexParameterf(type, GL_TEXTURE_MIN_LOD, state.MinLOD);
	glTexParameterf(type, GL_TEXTURE_MAX_LOD, state.MaxLOD);
}

void TexturePainter::DisableAll()
{
	#ifndef DISABLE_MULTITEXTURE
	if (m_MultitexturingEnabled)
	{
		for (int c=0; c<MAX_TEXTURES; c++)
		{
			glActiveTexture(GL_TEXTURE0+c);
			glDisable(GL_TEXTURE_2D);
			glDisable(GL_TEXTURE_CUBE_MAP);
		}
		glClientActiveTexture(GL_TEXTURE0);
	}
	else
	#endif
	{
		glDisable(GL_TEXTURE_2D);
		glDisable(GL_TEXTURE_CUBE_MAP);
	}
}

void TexturePainter::Dump()
{
	for (map<string,int>::iterator i=m_LoadedMap.begin(); i!=m_LoadedMap.end(); ++i)
	{
		TextureDesc info = m_TextureMap[i->second];
		Trace::Stream<<i->first<<" "<<info.Width<<"X"<<info.Height<<" ";
		if (info.Format==GL_RGB) Trace::Stream<<"RGB"<<endl;
		else if (info.Format==GL_RGBA) Trace::Stream<<"RGBA"<<endl;
	}
}

bool TexturePainter::IsResident(unsigned int id)
{
	GLboolean resident;
	return glAreTexturesResident(1, &id, &resident);
}

void TexturePainter::SetTexturePriority(unsigned int id, float priority)
{
	glPrioritizeTextures(1, &id, &priority);
}

unsigned TexturePainter::GetTextureWidth(unsigned int id)
{
	map<unsigned int, TextureDesc>::iterator i = m_TextureMap.find(id);
	if (i != m_TextureMap.end())
	{
		return i->second.Width;
	}
	else
	{
		return 0;
	}
}

unsigned TexturePainter::GetTextureHeight(unsigned int id)
{
	map<unsigned int, TextureDesc>::iterator i = m_TextureMap.find(id);
	if (i != m_TextureMap.end())
	{
		return i->second.Height;
	}
	else
	{
		return 0;
	}
}

