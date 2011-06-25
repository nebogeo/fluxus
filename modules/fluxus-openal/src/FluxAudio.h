// Copyleft (C) 2003 David Griffiths <dave@pawfal.org>
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

#ifndef FLUXAUDIO
#define FLUXAUDIO

#include <string>
#include <map>
#include <vector>
#include <math.h>

#ifdef __APPLE__

#include <OpenAL/al.h>
#include <OpenAL/alc.h>
//#include <OpenAL/alext.h>
#else

#include <AL/al.h>
#include <AL/alc.h>
//#include <AL/alext.h>
//#include <AL/alut.h>

#endif

using namespace std;

namespace fluxus
{

class dVector
{
public:
	dVector() {}
	dVector(float a, float b, float c) : x(a),y(b),z(c) {}
	float x,y,z;

	float *arr() { return &x; };

	inline dVector operator-(const dVector &other)
	{
		return dVector(x-other.x,y-other.y,z-other.z);
	}

	inline float dist(dVector const &rhs) const
	{
		return sqrt((rhs.x-x)*(rhs.x-x)+
				(rhs.y-y)*(rhs.y-y)+
				(rhs.z-z)*(rhs.z-z));
	}

	inline float mag()
	{
		return dist(dVector(0,0,0));
	}

};

class FluxAudio
{
public:
	FluxAudio();
	virtual ~FluxAudio();
	int Load(const string &Filename);
	void Play(unsigned int id, dVector pos = dVector(0, 0, 0), float pitch = 1.0f, float gain = 1.0f, bool looping = false);
	void Stop();
	void SetHeadPos(dVector pos, dVector front);
	void SetPoly(int s) { m_Poly=s; }
	void SetCullDist(float s) { m_CullDist=s; }
	void Update();

	struct AcousticDesc
	{
		AcousticDesc() :
			AttenScale(0.3),
			MaxDistance(0),
			RefDistance(0),
			Rolloff(0) {}

		float AttenScale;
		float MaxDistance;
		float RefDistance;
		float Rolloff;
	};

	void SetAcoustics(const AcousticDesc &d);

private:
	short *LoadSample(const string &Filename, unsigned int &frames);

	struct Event
	{
		unsigned int Id;
		dVector Pos;
		float Pitch;
		float Gain;
		bool Looping;
	};

	AcousticDesc m_Acoustics;
	dVector					 m_HeadPos;
	float					 m_CullDist;
	map<string,unsigned int> m_Loaded;
	vector<Event>			 m_EventVec;
	unsigned int			 m_Poly;
	ALCcontext				*m_ContextID;
	ALCdevice				*m_Device;
	vector<unsigned int>     m_Sources;
	int						 m_NextSource;
};


}

#endif

