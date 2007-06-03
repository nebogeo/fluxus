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

#include <png.h>
#include "PNGLoader.h"

using namespace Fluxus;

unsigned char *PNGLoader::Load(const string &Filename, int &w, int &h, PixelFormat &pf)
{
	unsigned char *ImageData = NULL;
	FILE *fp=fopen(Filename.c_str(),"rb");
	if (!fp) cerr<<"Couldn't open image ["<<Filename<<"]"<<endl;
	else
	{
		png_structp png_ptr;
		png_infop info_ptr;
		
		png_ptr = png_create_read_struct(PNG_LIBPNG_VER_STRING, NULL, NULL, NULL);
   	 	info_ptr = png_create_info_struct(png_ptr);
   		png_init_io(png_ptr, fp);
		png_read_png(png_ptr, info_ptr, PNG_TRANSFORM_IDENTITY, NULL);		
		fclose(fp);
		
		ImageData = new unsigned char[png_ptr->rowbytes*png_ptr->height];
		int p=0;
		for (unsigned int row = 0; row < png_ptr->height; row++)
		{
			for (unsigned int i=0; i<png_ptr->rowbytes; i++)
			{
				ImageData[p]=(unsigned char)info_ptr->row_pointers[row][i];
				p++;
			}
		}
		
		w=png_ptr->width;
		h=png_ptr->height;
		
		switch (png_ptr->color_type)
		{
			case PNG_COLOR_TYPE_RGB : pf=RGB; break;
        	case PNG_COLOR_TYPE_RGB_ALPHA : pf=RGBA; break;
        	default : cerr<<"PNG pixel format not supported : "<<png_ptr->color_type<<endl;
        }

		png_destroy_read_struct(&png_ptr, &info_ptr, (png_infopp)NULL);	
	}
	
	return ImageData;
}
