// Copyright (C) 2010 Gabor Papp, Dave Griffiths
//
// based on NVIDIA's nv_dss.h
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

#ifndef N_DDS
#define N_DDS

#include <string>
#include <vector>

#include "TexturePainter.h"
#include "OpenGL.h"

using namespace std;

namespace Fluxus
{

class DDSLoader
{
	public:
		static void Load(const string &Filename, TexturePainter::TextureDesc &desc,
							vector<TexturePainter::TextureDesc> &mipmaps);

	private:
		struct DDS_PIXELFORMAT
		{
			unsigned long dwSize;
			unsigned long dwFlags;
			unsigned long dwFourCC;
			unsigned long dwRGBBitCount;
			unsigned long dwRBitMask;
			unsigned long dwGBitMask;
			unsigned long dwBBitMask;
			unsigned long dwABitMask;
		};

		struct DDS_HEADER
		{
			unsigned long dwSize;
			unsigned long dwFlags;
			unsigned long dwHeight;
			unsigned long dwWidth;
			unsigned long dwPitchOrLinearSize;
			unsigned long dwDepth;
			unsigned long dwMipMapCount;
			unsigned long dwReserved1[11];
			DDS_PIXELFORMAT ddspf;
			unsigned long dwCaps1;
			unsigned long dwCaps2;
			unsigned long dwReserved2[3];
		};

		struct DXTColBlock
		{
			unsigned short col0;
			unsigned short col1;

			unsigned char row[4];
		};

		struct DXT3AlphaBlock
		{
			unsigned short row[4];
		};

		struct DXT5AlphaBlock
		{
			unsigned char alpha0;
			unsigned char alpha1;

			unsigned char row[6];
		};

		static int surface_size(bool compressed, int format, int width, int height, int components);

		static void flip(unsigned char *image, bool compressed, int format, int width, int height, int depth, int size);
		static void swap(void *byte1, void *byte2, int size);
		static void flip_blocks_dxtc1(DXTColBlock *line, int numBlocks);
		static void flip_blocks_dxtc3(DXTColBlock *line, int numBlocks);
		static void flip_blocks_dxtc5(DXTColBlock *line, int numBlocks);
		static void flip_dxt5_alpha(DXT5AlphaBlock *block);

		static inline int clamp_size(int s) { return (s <= 0) ? 1 : s; }
};

const unsigned long DDS_FOURCC = 0x00000004;
const unsigned long DDS_RGB    = 0x00000040;
const unsigned long DDS_RGBA   = 0x00000041;
const unsigned long DDS_DEPTH  = 0x00800000;

const unsigned long DDS_COMPLEX = 0x00000008;
const unsigned long DDS_CUBEMAP = 0x00000200;
const unsigned long DDS_VOLUME  = 0x00200000;

const unsigned long FOURCC_DXT1 = 0x31545844; //(MAKEFOURCC('D','X','T','1'))
const unsigned long FOURCC_DXT3 = 0x33545844; //(MAKEFOURCC('D','X','T','3'))
const unsigned long FOURCC_DXT5 = 0x35545844; //(MAKEFOURCC('D','X','T','5'))

}
#endif

