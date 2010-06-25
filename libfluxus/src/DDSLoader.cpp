// Copyright (C) 2010 Gabor Papp, Dave Griffiths
//
// based on NVIDIA's nv_dss.cpp
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

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <assert.h>

#include "Trace.h"
#include "DDSLoader.h"

using namespace Fluxus;
using namespace std;

void DDSLoader::Load(const string &Filename, TexturePainter::TextureDesc &desc,
		vector<TexturePainter::TextureDesc> &mipmaps)
{
	desc.ImageData = NULL;

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
		int size;

		width = ddsh.dwWidth;
		height = ddsh.dwHeight;
		depth = clamp_size(ddsh.dwDepth);

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
					goto failure;
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
		// FIXME: cubemaps are ignored at the moment
		size = surface_size(compressed, format, width, height, components) * depth;
		desc.ImageData = new unsigned char [size];
		fread(desc.ImageData, 1, size, fp);
		flip(desc.ImageData, compressed, format, width, height, depth, size);

		desc.Width = width;
		desc.Height = height;
		desc.InternalFormat = format;
		desc.Format = (components == 3) ? GL_RGB : GL_RGBA;
		desc.Size = size;

		// load mipmaps
		int num_mipmaps = ddsh.dwMipMapCount;

		// number of mipmaps in file includes main surface
		if (num_mipmaps != 0)
			num_mipmaps--;

		// load all mipmaps for current surface
		for (int i = 0; i < num_mipmaps; i++)
		{
		width = clamp_size(width >> 1);
		height = clamp_size(height >> 1);
		depth = clamp_size(depth >> 1);
		// calculate mipmap size
			size = surface_size(compressed, format, width, height, components) * depth;

			TexturePainter::TextureDesc mipmap_desc;
			mipmap_desc.Width = width;
			mipmap_desc.Height = height;
			mipmap_desc.InternalFormat = desc.InternalFormat;
			mipmap_desc.Format = desc.Format;
			mipmap_desc.Size = size;
			mipmap_desc.ImageData = new unsigned char [size];

			fread(mipmap_desc.ImageData, 1, size, fp);

			flip(mipmap_desc.ImageData, compressed, desc.InternalFormat,
					width, height, depth, size);

			mipmaps.push_back(mipmap_desc);
	}
failure:
		fclose(fp);
	}
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

void DDSLoader::flip(unsigned char *image, bool compressed, int format, int width, int height, int depth, int size)
{
	int linesize;
	int offset;

	if (!compressed)
	{
		assert(depth > 0);

		int imagesize = size/depth;
		linesize = imagesize / height;

		for (int n = 0; n < depth; n++)
		{
			offset = imagesize*n;
			unsigned char *top = image + offset;
			unsigned char *bottom = top + (imagesize-linesize);

			for (int i = 0; i < (height >> 1); i++)
			{
				swap(bottom, top, linesize);

				top += linesize;
				bottom -= linesize;
			}
		}
	}
	else
	{
		void (*flipblocks)(DXTColBlock*, int);
		int xblocks = width / 4;
		int yblocks = height / 4;
		int blocksize;

		switch (format)
		{
			case GL_COMPRESSED_RGBA_S3TC_DXT1_EXT:
				blocksize = 8;
				flipblocks = &flip_blocks_dxtc1;
				break;
			case GL_COMPRESSED_RGBA_S3TC_DXT3_EXT:
				blocksize = 16;
				flipblocks = &DDSLoader::flip_blocks_dxtc3;
				break;
			case GL_COMPRESSED_RGBA_S3TC_DXT5_EXT:
				blocksize = 16;
				flipblocks = &DDSLoader::flip_blocks_dxtc5;
				break;
			default:
				return;
		}

		linesize = xblocks * blocksize;

		DXTColBlock *top;
		DXTColBlock *bottom;

		for (int j = 0; j < (yblocks >> 1); j++)
		{
			top = (DXTColBlock*)(image + j * linesize);
			bottom = (DXTColBlock*)(image + (((yblocks-j)-1) * linesize));

			(*flipblocks)(top, xblocks);
			(*flipblocks)(bottom, xblocks);

			swap(bottom, top, linesize);
		}
	}
}

/* swap to sections of memory */
void DDSLoader::swap(void *byte1, void *byte2, int size)
{
	unsigned char *tmp = new unsigned char[size];

	memcpy(tmp, byte1, size);
	memcpy(byte1, byte2, size);
	memcpy(byte2, tmp, size);

	delete [] tmp;
}

/* flip a DXT1 color block */
void DDSLoader::flip_blocks_dxtc1(DXTColBlock *line, int numBlocks)
{
	DXTColBlock *curblock = line;

	for (int i = 0; i < numBlocks; i++)
	{
		swap(&curblock->row[0], &curblock->row[3], sizeof(unsigned char));
		swap(&curblock->row[1], &curblock->row[2], sizeof(unsigned char));

		curblock++;
	}
}

/* flip a DXT3 color block */
void DDSLoader::flip_blocks_dxtc3(DXTColBlock *line, int numBlocks)
{
	DXTColBlock *curblock = line;
	DXT3AlphaBlock *alphablock;

	for (int i = 0; i < numBlocks; i++)
	{
		alphablock = (DXT3AlphaBlock*)curblock;

		swap(&alphablock->row[0], &alphablock->row[3], sizeof(unsigned short));
		swap(&alphablock->row[1], &alphablock->row[2], sizeof(unsigned short));

		curblock++;

		swap(&curblock->row[0], &curblock->row[3], sizeof(unsigned char));
		swap(&curblock->row[1], &curblock->row[2], sizeof(unsigned char));

		curblock++;
	}
}

/* flip a DXT5 alpha block */
void DDSLoader::flip_dxt5_alpha(DXT5AlphaBlock *block)
{
	unsigned char gBits[4][4];

	const unsigned long mask = 0x00000007;		    // bits = 00 00 01 11
	unsigned long bits = 0;
	memcpy(&bits, &block->row[0], sizeof(unsigned char) * 3);

	gBits[0][0] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[0][1] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[0][2] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[0][3] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[1][0] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[1][1] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[1][2] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[1][3] = (unsigned char)(bits & mask);

	bits = 0;
	memcpy(&bits, &block->row[3], sizeof(unsigned char) * 3);

	gBits[2][0] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[2][1] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[2][2] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[2][3] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[3][0] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[3][1] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[3][2] = (unsigned char)(bits & mask);
	bits >>= 3;
	gBits[3][3] = (unsigned char)(bits & mask);

	unsigned long *pBits = ((unsigned long*) &(block->row[0]));

	*pBits &= 0xff000000;

	*pBits = *pBits | (gBits[3][0] << 0);
	*pBits = *pBits | (gBits[3][1] << 3);
	*pBits = *pBits | (gBits[3][2] << 6);
	*pBits = *pBits | (gBits[3][3] << 9);

	*pBits = *pBits | (gBits[2][0] << 12);
	*pBits = *pBits | (gBits[2][1] << 15);
	*pBits = *pBits | (gBits[2][2] << 18);
	*pBits = *pBits | (gBits[2][3] << 21);

	pBits = ((unsigned long*) &(block->row[3]));

	*pBits &= 0xff000000;

	*pBits = *pBits | (gBits[1][0] << 0);
	*pBits = *pBits | (gBits[1][1] << 3);
	*pBits = *pBits | (gBits[1][2] << 6);
	*pBits = *pBits | (gBits[1][3] << 9);

	*pBits = *pBits | (gBits[0][0] << 12);
	*pBits = *pBits | (gBits[0][1] << 15);
	*pBits = *pBits | (gBits[0][2] << 18);
	*pBits = *pBits | (gBits[0][3] << 21);
}

/* flip a DXT5 color block */
void DDSLoader::flip_blocks_dxtc5(DXTColBlock *line, int numBlocks)
{
	DXTColBlock *curblock = line;
	DXT5AlphaBlock *alphablock;

	for (int i = 0; i < numBlocks; i++)
	{
		alphablock = (DXT5AlphaBlock*)curblock;

		flip_dxt5_alpha(alphablock);

		curblock++;

		swap(&curblock->row[0], &curblock->row[3], sizeof(unsigned char));
		swap(&curblock->row[1], &curblock->row[2], sizeof(unsigned char));

		curblock++;
	}
}

