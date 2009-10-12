// Copyright (C) 2009 Gabor Papp
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

#include <stdio.h>
#include <math.h>
#include <string.h>

#include <iostream>

#include "OpenGL.h"

#include "Video.h"

using namespace std;

//#define DEBUG_GL

#ifdef DEBUG_GL
static int check_gl_errors(const char *call)
{
	GLenum status = glGetError();
	if (status == GL_NO_ERROR)
		return 0;

	const char *status_msg = (const char *)gluErrorString(status);
	if (status_msg == NULL)
		status_msg = "unknown gl error";

	cerr << call << " - " << status_msg << " (" << status << ")" << endl;
	return 1;
}

#define CHECK_GL_ERRORS(call) check_gl_errors(call)

#else

#define CHECK_GL_ERRORS(call)

#endif

Video::Video(string name) :
	texture_id(0)
{
	player.loadMovie(name);

	video_width = player.getWidth();
	video_height = player.getHeight();

	tex_width = 1 << (unsigned)ceil(log2(video_width));
	tex_height = 1 << (unsigned)ceil(log2(video_height));

	glEnable(GL_TEXTURE_2D);
	glGenTextures(1, &texture_id);

	glBindTexture(GL_TEXTURE_2D, texture_id);

	glTexImage2D(GL_TEXTURE_2D, 0, 4, tex_width, tex_height, 0,
			GL_RGB, GL_UNSIGNED_BYTE, NULL);
	CHECK_GL_ERRORS("glTexImage2d");

	glGenerateMipmapEXT(GL_TEXTURE_2D);
	CHECK_GL_ERRORS("glGenerateMipmapEXT");

	glBindTexture(GL_TEXTURE_2D, 0);
	glDisable(GL_TEXTURE_2D);
}

Video::~Video()
{
	player.closeMovie();

	if (texture_id != 0)
	{
		glDeleteTextures(1, &texture_id);
	}

}

void Video::update()
{
	player.update();

	if (!player.isFrameNew())
	{
		return;
	}

	unsigned char *pixels = player.getPixels();

	glEnable(GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, texture_id);

	glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, video_width, video_height,
			GL_RGB, GL_UNSIGNED_BYTE, pixels);
	CHECK_GL_ERRORS("update: glTexImage2d");
	glGenerateMipmapEXT(GL_TEXTURE_2D);
	CHECK_GL_ERRORS("update: glGenerateMipmapEXT");

	glBindTexture(GL_TEXTURE_2D, 0);
	glDisable(GL_TEXTURE_2D);
}

/**
 * Returns video texture coordinates
 * \return 2x3 floats (top-left, bottom-right)
 **/
float *Video::video_tcoords()
{
	static float t[6];

	t[0] = 0.0;
	t[1] = 0.0;
	t[2] = 0.0;
	t[3] = (float)(video_width - 1)/(float)tex_width;
	t[4] = (float)(video_height - 1)/(float)tex_height;
	t[5] = 0.0;

	return t;
}

