// Copyright (C) 2010 Gabor Papp, Dave Griffiths
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

#include <cstdlib>
#include <iostream>
#include "Trace.h"
#include "DDSLoader.h"

using namespace Fluxus;
using namespace std;

unsigned char *DDSLoader::Load(const string &Filename, TexturePainter::TextureDesc &desc)
{
	unsigned char *ImageData = NULL;
	FILE *fp = fopen(Filename.c_str(), "rb");
	if (!fp || Filename == "")
	{
		Trace::Stream << "Couldn't open image [" << Filename << "]" << endl;
	}
	else
	{
		// read in file marker, make sure its a DDS file
		char magic[4];
		fread(magic, 1, 4, fp);
		if (strncmp(magic, "DDS ", 4) != 0)
		{
			Trace::Stream << "Couldn't find DDS filecode in image [" << Filename << "]" << endl;
			goto failure;
		}

		// read in DDS header
		DDS_HEADER ddsh;
		fread(&ddsh, sizeof(ddsh), 1, fp);

		// store primary surface width/height/depth
		int width, height, depth;
		int format;
		int components;
		bool compressed;

		width = ddsh.dwWidth;
		height = ddsh.dwHeight;
		depth = ddsh.dwDepth;
		if (depth <= 0)
			depth = 1;

		// figure out what the image format is
		if (ddsh.ddspf.dwFlags & DDS_FOURCC)
		{
			switch(ddsh.ddspf.dwFourCC)
			{
				case FOURCC_DXT1:
					format = GL_COMPRESSED_RGBA_S3TC_DXT1_EXT;
					components = 3;
					compressed = true;
					break;
				case FOURCC_DXT3:
					format = GL_COMPRESSED_RGBA_S3TC_DXT3_EXT;
					components = 4;
					compressed = true;
					break;
				case FOURCC_DXT5:
					format = GL_COMPRESSED_RGBA_S3TC_DXT5_EXT;
					components = 4;
					compressed = true;
					break;
				default:
					fclose(fp);
					return false;
			}
		}
		else if (ddsh.ddspf.dwFlags == DDS_RGBA && ddsh.ddspf.dwRGBBitCount == 32)
		{
			format = GL_BGRA_EXT;
			compressed = false;
			components = 4;
		}
		else if (ddsh.ddspf.dwFlags == DDS_RGB  && ddsh.ddspf.dwRGBBitCount == 32)
		{
			format = GL_BGRA_EXT;
			compressed = false;
			components = 4;
		}
		else if (ddsh.ddspf.dwFlags == DDS_RGB  && ddsh.ddspf.dwRGBBitCount == 24)
		{
			format = GL_BGR_EXT;
			compressed = false;
			components = 3;
		}
		else
		{
			Trace::Stream << "Couldn't determine image format [" << Filename << "]" << endl;
			goto failure;
		}

		// load surface
		// FIXME: cubemaps and mipmaps are ignored at the moment
		int size = surface_size(compressed, format, width, height, components);
		ImageData = new unsigned char [size];
		fread(ImageData, 1, size, fp);

		desc.Width = width;
		desc.Height = height;
		desc.InternalFormat = format;
		desc.Format = (components == 3) ? GL_RGB : GL_RGBA;
		desc.Size = size;

failure:
		fclose(fp);
	}

	return ImageData;
}

int DDSLoader::surface_size(bool compressed, int format, int width, int height, int components)
{
	if (compressed)
	{
		return ((width + 3) / 4) * ((height + 3) / 4) *
			(format == GL_COMPRESSED_RGBA_S3TC_DXT1_EXT ? 8 : 16);
	}
	else
	{
		return width * height * components;
	}
}

