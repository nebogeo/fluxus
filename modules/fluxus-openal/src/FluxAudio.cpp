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
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.FluxAudioFluxAudio();

#include <iostream>
#include <cstring>
#include <cstdio>

#include "FluxAudio.h"

using namespace fluxus;

static const float MIN_FREQ = 0.1;
static const float MIN_GAIN = 0.1;
static const int MAX_POLY = 10;

/////////////////////////////////////////////////////////////////////////

FluxAudio::FluxAudio():
m_CullDist(1000),
m_Poly(MAX_POLY),
m_NextSource(0)
{
	//alExt::Init();

	//int attrlist[] = { ALC_FREQUENCY, 22050, ALC_INVALID };

	m_Device = alcOpenDevice( NULL );
	if( m_Device == NULL )
	{
		cerr<<"Could not open al audio device"<<endl;
		return;
	}

	// Initialize ALUT.
	m_ContextID = alcCreateContext( m_Device, NULL );
	if(m_ContextID == NULL)
	{
		cerr<<"Could not open context: "<<alGetString(alcGetError(m_Device))<<endl;
		return;
	}

	alcMakeContextCurrent(m_ContextID);

	ALfloat zeroes[] = { 0.0f, 0.0f,  0.0f };
	ALfloat front[]  = { 0.0f, 0.0f, -1.0f, 0.0f, 1.0f, 0.0f };
	ALfloat position[] = { 20.0f, 0.0f, 5.0f };

	alListenerfv(AL_POSITION, zeroes );
	alListenerfv(AL_VELOCITY, zeroes );
	alListenerfv(AL_ORIENTATION, front );

	// generate the sources
    for (int n=0; n<MAX_POLY; n++)
	{
		unsigned int t=0;
		alGenSources( 1, &t);
		m_Sources.push_back(t);
	}
}

FluxAudio::~FluxAudio()
{
	alcDestroyContext(m_ContextID);
	alcCloseDevice(m_Device);
}

void FluxAudio::SetAcoustics(const AcousticDesc &d)
{
	m_Acoustics=d;
}

short *LoadWav(FILE *file, unsigned int &size, unsigned short &channels)
{
	char id[5];
	id[4]='\0';
	fread(id,1,4,file);
	if (strcmp(id,"RIFF")!=0)
	{
		cerr<<"WAV format error (RIFF): "<<id<<endl;
		return NULL;
	}
	fread(&size,1,4,file);
	fread(id,1,4,file);
	if (strcmp(id,"WAVE")!=0)
	{
		cerr<<"WAV format error (WAVE): "<<id<<endl;
		return NULL;
	}
	fread(id,1,4,file);
	if (strcmp(id,"fmt ")!=0)
	{
		cerr<<"WAV format error (fmt ): "<<id<<endl;
		return NULL;
	}
	fread(&size,1,4,file);
	unsigned int datastart=size+ftell(file);
	short compression;
	fread(&compression,1,2,file);
	if (compression!=1)
	{
		cerr<<"WAV data is compressed"<<endl;
		return NULL;
	}
	fread(&channels,1,2,file);
	if (!(channels==1 || channels==2))
	{
		cerr<<"WAV data is not mono or stereo"<<endl;
		return NULL;
	}
	fseek(file,datastart,SEEK_SET);
	fread(id,1,4,file);
	if (strcmp(id,"data")!=0)
	{
		cerr<<"WAV format error (data): "<<id<<endl;
		return NULL;
	}
	fread(&size,1,4,file);
	char *data=new char[size];
	fread(data,1,size,file);
	return (short*)data;
};

int FluxAudio::Load(const string &Filename)
{
	map<string,unsigned int>::iterator i=m_Loaded.find(Filename);
	if (i!=m_Loaded.end())
	{
		return i->second;
	}

	ALsizei size,bits,freq,format;
	ALboolean err;
	short *data = NULL;
	unsigned int ID = 0;
	alGenBuffers(1, &ID);
	m_Loaded[Filename]=ID;

	FILE* file = fopen (Filename.c_str(), "rb") ;
	if (!file)
	{
		cerr<<"Error opening ["<<Filename<<"]"<<endl;
	}
	else
	{
		unsigned short channels=0;
		unsigned int usize=0;
		data = LoadWav(file, usize, channels);

		if (channels==1) format=AL_FORMAT_MONO16;
		else if (channels==2) format=AL_FORMAT_STEREO16;
		else
		{
			cerr<<"Can only used mono or stero wavs : "<<Filename<<endl;
			return 0;
		}
		bits=16;
		freq=44100;
		size=usize;

		fclose(file);

		if(data == NULL)
		{
			cerr<<"Couldn't open "<<Filename<<endl;
			return 0;
		}

		alBufferData(ID, format, data, size, freq);
		delete data;

		if( alGetError() != AL_NO_ERROR )
		{
			cerr<<"Could not BufferData "<<Filename<<endl;
			return 0;
		}
	}
	return ID;
}

int FluxAudio::Play(unsigned int id, dVector pos /*= dVector(0, 0, 0) */,
		float pitch /* = 1.0f */, float gain /* = 1.0f */, bool looping /* = false */)
{
	int SourceID = -1;

	if (pitch<MIN_FREQ || gain<MIN_GAIN)
		return SourceID;

	if ((m_HeadPos-pos).mag()<m_CullDist)
	{
		// todo: add priority???
		if (m_EventVec.size()<m_Poly)
		{
			Event newevent;
			newevent.Id=id;
			newevent.Pos=pos;
			newevent.Pitch=pitch;
			newevent.Gain=gain;
			newevent.Looping=looping;
			newevent.SourceID = m_NextSource;
			SourceID = m_NextSource;
			m_EventVec.push_back(newevent);

			m_NextSource++;
			if (m_NextSource >= (int)m_Sources.size())
				m_NextSource = 0;
		}
	}

	return SourceID;
}

void FluxAudio::Update()
{
	// play sounds in the queue
	for (vector<Event>::iterator i=m_EventVec.begin(); i!=m_EventVec.end(); i++)
	{
		unsigned source = m_Sources[i->SourceID];

		alSourceStop(source);
		alSourcefv(source, AL_POSITION, i->Pos.arr());
		alSourcei(source, AL_BUFFER, i->Id);
		alSourcef(source, AL_PITCH, i->Pitch);
		alSourcef(source, AL_GAIN, i->Gain);
		alSourcei(source, AL_LOOPING, i->Looping ? AL_TRUE : AL_FALSE);

		//alSourcef(source, AL_DISTANCE_SCALE, m_Acoustics.AttenScale);
		alSourcef(source, AL_MAX_DISTANCE, m_Acoustics.MaxDistance);
		alSourcef(source, AL_ROLLOFF_FACTOR, m_Acoustics.Rolloff);
		alSourcef(source, AL_REFERENCE_DISTANCE, m_Acoustics.RefDistance);

		//if (m_Acoustics.Reverb)
		//{
		//	alExt::ReverbScale(m_Sources[m_NextSource], m_Acoustics.ReverbScale);
		//	alExt::ReverbDelay(m_Sources[m_NextSource], m_Acoustics.ReverbDelay);
		//}

		alSourcePlay(source);
	}

	m_EventVec.clear();
}

void FluxAudio::SetHeadPos(dVector pos, dVector front)
{
	alListenerfv(AL_POSITION, pos.arr() );
	alListenerfv(AL_ORIENTATION, front.arr() );
	m_HeadPos=pos;
}

void FluxAudio::SetPitch(int sourceID, float pitch)
{
	if ((pitch <= 0) || (sourceID < 0) || (sourceID >= (int)m_Sources.size()))
		return;

	alSourcef(m_Sources[sourceID], AL_PITCH, pitch);
}

void FluxAudio::Stop()
{
	// stop all sources
	alSourceStopv(m_Sources.size(), &(m_Sources[0]));
}

