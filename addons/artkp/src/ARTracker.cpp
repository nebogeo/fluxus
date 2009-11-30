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

#include <iostream>
#include "ARTracker.h"

#include "ARToolKitPlus/TrackerSingleMarkerImpl.h"

using namespace std;

ARTracker::ARTracker()
{
	tracker = NULL;
	patt_width = 80;
	patt_center[0] = patt_center[1] = 0;
}

ARTracker::~ARTracker()
{
	if (tracker != NULL)
	{
		tracker->cleanup();
		delete tracker;
	}
}

bool ARTracker::init(std::string &cam_param_file, int width, int height)
{
	float clip_near = 1.0;
	float clip_far = 1000.0;

	if (tracker != NULL)
	{
		tracker->cleanup();
		delete tracker;
	}

	/* create a tracker that does:
	 *  - 6x6 sized marker images
	 *  - samples at a maximum of 6x6
	 *  - works with rgb images
	 *  - can load a maximum of 16 patterns
	 *  - can detect a maximum of 16 patterns in one image */
	tracker = new ARToolKitPlus::TrackerSingleMarkerImpl<6, 6, 6, 16, 16>(width, height);
	tracker->setLogger(&logger);
	tracker->setPixelFormat(ARToolKitPlus::PIXEL_FORMAT_RGB);
	if (!tracker->init(cam_param_file.c_str(), clip_near, clip_far))
	{
		return false;
	}

	tracker->setPatternWidth(patt_width);
	tracker->setBorderWidth(0.250);
	tracker->setUndistortionMode(ARToolKitPlus::UNDIST_STD);

	/* RPP is more robust than ARToolKit's standard pose estimator */
	tracker->setPoseEstimator(ARToolKitPlus::POSE_ESTIMATOR_RPP);

	/* switch to simple ID based markers
	 * use the tool in tools/IdPatGen to generate markers */
	tracker->setMarkerMode(ARToolKitPlus::MARKER_ID_SIMPLE);

	return true;
}

int ARTracker::detect(const unsigned char *img)
{
	tracker->calc(img, -1, false, &marker_info, &marker_count);
	return marker_count;
}

const ARFloat *ARTracker::get_projection_matrix()
{
	return tracker->getProjectionMatrix();
}

const ARFloat *ARTracker::get_modelview_matrix(int i)
{
	if ((i >= 0) && (i < marker_count))
	{
		tracker->executeSingleMarkerPoseEstimator(&marker_info[i], patt_center, patt_width, patt_trans);

		for(int j = 0; j < 3; j++)
		{
			for(int i = 0; i < 4; i++)
			{
				gl_modelview[i * 4 + j] = patt_trans[j][i];
			}
		}
		gl_modelview[0 * 4 + 3] = gl_modelview[1 * 4 + 3] = gl_modelview[2 * 4 + 3] = 0.0;
		gl_modelview[3 * 4 + 3] = 1.0;

		return gl_modelview;
	}
	else
	{
		return NULL;
	}
}

int ARTracker::get_id(int i)
{
	if ((i >= 0) && (i < marker_count))
		return marker_info[i].id;
	else
		return -1;
}

ARFloat ARTracker::get_confidence(int i)
{
	if ((i >= 0) && (i < marker_count))
		return marker_info[i].cf;
	else
		return 0;
}

