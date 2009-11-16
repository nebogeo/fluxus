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

#ifndef VIDEO_H
#define VIDEO_H

#include <string>

#include "ofVideoPlayer.h"
#include "ofVideoGrabber.h"

class VideoTexture
{
	public:
			VideoTexture();
			virtual ~VideoTexture();

			float* get_tcoords();

			unsigned get_texture_id()
			{
				return texture_id;
			}

	protected:
			void gen_texture();
			void upload(unsigned char *pixels);

			int width, height; // pixel buffer resolution of video or camera image
			int tex_width, tex_height; // texture resolution (power of 2)

			unsigned texture_id;
};

class Video: public VideoTexture
{
	public:
			Video(std::string name);
			~Video();

			void update();

			void play()
			{
				player.play();
			}

			void stop()
			{
				player.stop();
			}

			void seek(float pos)
			{
				player.setPosition(pos);
			}

	private:
			ofVideoPlayer player;
};

class Camera: public VideoTexture
{
	public:
			Camera(unsigned device_id, int w, int h);
			~Camera();

			void update();

	private:
			ofVideoGrabber camera;
};

#endif

