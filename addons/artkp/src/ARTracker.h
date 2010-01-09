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

#ifndef ARTRACKER_H
#define ARTRACKER_H

#include <iostream>
#include <string>

#include "ARToolKitPlus/TrackerSingleMarker.h"

class Logger:public ARToolKitPlus::Logger
{
	void artLog(const char *str)
	{
		std::cerr << str << std::endl;
	}
};

class ARTracker
{
	public:
		ARTracker();
		~ARTracker();

		bool init(std::string &cam_param_file, int width, int height,
				ARToolKitPlus::MARKER_MODE mode);

		void set_threshold(int t)
		{
			tracker->setThreshold(t);
		}

		int get_threshold()
		{
			return tracker->getThreshold();
		}

		void activate_auto_threshold(bool e)
		{
			tracker->activateAutoThreshold(e);
		}

		void set_pattern_width(float w)
		{
			patt_width = w;
			tracker->setPatternWidth(patt_width);
		}

		void activate_vignetting_compensation(bool e)
		{
			tracker->activateVignettingCompensation(e);
		}

		int load_pattern(const char *filename)
		{
			return tracker->addPattern(filename);
		}

		int detect(const unsigned char *img);
		const ARFloat *get_projection_matrix();
		const ARFloat *get_modelview_matrix(int i);
		ARFloat get_confidence(int i);
		int get_id(int i);

	private:
		ARToolKitPlus::TrackerSingleMarker *tracker;
		Logger logger;

		int marker_count;
		ARToolKitPlus::ARMarkerInfo *marker_info;

		ARFloat patt_width;
		ARFloat patt_center[2];
		ARFloat patt_trans[3][4];

		ARFloat gl_modelview[16];
};

#endif
