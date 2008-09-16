;;// Copyright (C) 2005 Dave Griffiths
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

#include <cstring>
#include <tiffio.h> 
extern "C"
{
#include <jpeglib.h>	
}
#include "Utils.h"
#include <iostream>
#include <fstream>

using namespace std;

GLubyte *GetScreenBuffer(int x, int y, int &width, int &height, int super)
{
	// get the raw image
	GLubyte *image = (GLubyte *) malloc(width * height * sizeof(GLubyte) * 3);
	// OpenGL's default 4 byte pack alignment would leave extra bytes at the
	// end of each image row so that each full row contained a number of bytes
	// divisible by 4.  Ie, an RGB row with 3 pixels and 8-bit componets would
	// be laid out like "RGBRGBRGBxxx" where the last three "xxx" bytes exist
	// just to pad the row out to 12 bytes (12 is divisible by 4). To make sure
	// the rows are packed as tight as possible (no row padding), set the pack
	// alignment to 1. 
	glPixelStorei(GL_PACK_ALIGNMENT, 1);
	glReadPixels(x, y, width, height, GL_RGB, GL_UNSIGNED_BYTE, image);
	
	if (super==1) return image;
		
	// supersample the image
	int newwidth=width/super;
	int newheight=height/super;

	GLubyte *image2 = (GLubyte *) malloc(newwidth * newheight * sizeof(GLubyte) * 3);

	for (int yy=0; yy<newheight; yy++)
	{
		for (int xx=0; xx<newwidth; xx++)
		{
			int sx=xx*super;
			int sy=yy*super;
			int i=(yy*newwidth+xx)*3;

			int a=(sy*width+sx)*3;
			int b=(sy*width+(sx+1))*3;
			int c=((sy+1)*width+(sx+1))*3;
			int d=((sy+1)*width+sx)*3;

			image2[i]=(image[a]+image[b]+image[c]+image[d])/4;
			image2[i+1]=(image[a+1]+image[b+1]+image[c+1]+image[d+1])/4;
			image2[i+2]=(image[a+2]+image[b+2]+image[c+2]+image[d+2])/4;
		}
	}
	
	width=newwidth;
	height=newheight;

	free(image);	
	return image2;
}

int WriteTiff(const char *filename, const char *description, int x, int y, int width, int height, int compression, int super)
{
	TIFF *file;
	GLubyte *image, *p;
	int i;

	file = TIFFOpen(filename, "w");
	if (file == NULL) 
	{
		return 1;
	}
	
	image = GetScreenBuffer(x, y, width, height, super);
	
	TIFFSetField(file, TIFFTAG_IMAGEWIDTH, (uint32) width);
	TIFFSetField(file, TIFFTAG_IMAGELENGTH, (uint32) height);
	TIFFSetField(file, TIFFTAG_BITSPERSAMPLE, 8);
	TIFFSetField(file, TIFFTAG_COMPRESSION, compression);
	TIFFSetField(file, TIFFTAG_PHOTOMETRIC, PHOTOMETRIC_RGB);
	TIFFSetField(file, TIFFTAG_SAMPLESPERPIXEL, 3);
	TIFFSetField(file, TIFFTAG_PLANARCONFIG, PLANARCONFIG_CONTIG);
	TIFFSetField(file, TIFFTAG_ROWSPERSTRIP, 1);
	TIFFSetField(file, TIFFTAG_IMAGEDESCRIPTION, description);
	p = image;
	for (i = height - 1; i >= 0; i--) 
	{
		if (TIFFWriteScanline(file, p, i, 0) < 0) 
		{
			free(image);
			TIFFClose(file);
			return 1;
		}
		p += width * sizeof(GLubyte) * 3;
	}
	TIFFClose(file);
	free(image);
	return 0;
}	

int WriteJPG(const char *filename, const char *description, int x, int y, int width, int height, int quality, int super)
{
	GLubyte *image = GetScreenBuffer(x, y, width, height, super);

	struct jpeg_compress_struct cinfo;
	struct jpeg_error_mgr jerr;
 
 	FILE * outfile;		
	JSAMPROW row_pointer[1];
	int row_stride;		

	cinfo.err = jpeg_std_error(&jerr);
	jpeg_create_compress(&cinfo);
 
	if ((outfile = fopen(filename, "wb")) == NULL) 
	{
    	return 1;
  	}
  	
	jpeg_stdio_dest(&cinfo, outfile);
	
 	cinfo.image_width = width; 
  	cinfo.image_height = height;
  	cinfo.input_components = 3;	
  	cinfo.in_color_space = JCS_RGB;
 
 	jpeg_set_defaults(&cinfo);
	jpeg_set_quality(&cinfo, quality, TRUE);
	jpeg_start_compress(&cinfo, TRUE);

	row_stride = width * 3;	

	while (cinfo.next_scanline < cinfo.image_height) 
	{
    	row_pointer[0] = & image[(cinfo.image_height-1-cinfo.next_scanline) * row_stride];
    	(void) jpeg_write_scanlines(&cinfo, row_pointer, 1);
  	}

	jpeg_finish_compress(&cinfo);
 	fclose(outfile);

	jpeg_destroy_compress(&cinfo);
	free(image);
	
	return 0;
}	

int WritePPM(const char *filename, const char *description, int x, int y, int width, int height, int compression, int super)
{
	FILE* file = fopen(filename,"w");
	if (file == NULL) 
	{
		return 1;
	}
	
	GLubyte *image = GetScreenBuffer(x, y, width, height, super);
	char buf[256];
	sprintf(buf,"P6\n%d\n%d\n255\n",width,height);
	fwrite(buf,strlen(buf)*sizeof(char),1,file);
	for (int y=height-1; y>-1; y--)
	{
		fwrite(image+y*width*3,width*3,1,file);
	}
	fclose(file);
	free(image);
	
	return 0;
}	
