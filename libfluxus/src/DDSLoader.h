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

#ifndef N_DDS
#define N_DDS

#include <string>

#include "TexturePainter.h"
#include "OpenGL.h"

using namespace std;

namespace Fluxus
{

class DDSLoader
{
	public:
		static unsigned char *Load(const string &Filename,
				TexturePainter::TextureDesc &desc);
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

		static int surface_size(bool compressed, int format, int width, int height, int components);
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

