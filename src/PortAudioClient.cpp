// Copyright (C) 2005 David Griffiths <dave@pawfal.org>
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
#include <limits.h>

#include "PortAudioClient.h"

PortAudioClient*  PortAudioClient::m_Singleton  = NULL;
bool              PortAudioClient::m_Attached   = false;
long unsigned int PortAudioClient::m_BufferSize = 0;
long unsigned int PortAudioClient::m_SampleRate = 44100;
void            (*PortAudioClient::RunCallback)(void*, unsigned int BufSize)=NULL;
void             *PortAudioClient::RunContext   = NULL;	
PortAudioStream  *PortAudioClient::m_Client     = NULL;
float *PortAudioClient::m_RightData=NULL;
float *PortAudioClient::m_LeftData=NULL;
float *PortAudioClient::m_RightInData=NULL;
float *PortAudioClient::m_LeftInData=NULL;

///////////////////////////////////////////////////////

PortAudioClient::PortAudioClient() 
{
}

/////////////////////////////////////////////////////////////////////////////////////////////

PortAudioClient::~PortAudioClient()	
{	
	Detach();
}

/////////////////////////////////////////////////////////////////////////////////////////////

bool PortAudioClient::Attach(const string &ClientName)
{
	if (m_Attached) return true;

    PaError err;
    int i;
    int totalSamps;

	err = Pa_Initialize();
    if( err != paNoError ) 
 	{
		cerr<<"Could not init PortAudioClient: "<<Pa_GetErrorText( err )<<endl;
		Pa_Terminate();
		return false;
	}
		
    err = Pa_OpenStream(&m_Client,
              Pa_GetDefaultInputDeviceID(),
              2,            
              paFloat32,  /* 32 bit floating point input */
              NULL,
              paNoDevice,
              0,              /* stereo output */
              paFloat32,      /* 32 bit floating point output */
              NULL,
              m_SampleRate,
              256,
              2,    /* number of buffers, if zero then use default minimum */
              paClipOff|paDitherOff, /* we won't output out of range samples so don't bother clipping them */
              Process,
              NULL );
	
    if( err != paNoError ) 
	{
		cerr<<"Could not attach PortAudioClient: "<<Pa_GetErrorText( err )<<endl;
		Pa_Terminate();
		return false;
	}

	err = Pa_StartStream( m_Client );
	
	if( err != paNoError ) 
	{
		cerr<<"Could not start stream: "<<Pa_GetErrorText( err )<<endl;
		Pa_Terminate();
		return false;
	}
	
	m_Attached=true;
	cerr<<"connected to portaudio..."<<endl;
	return true;
}

/////////////////////////////////////////////////////////////////////////////////////////////

void PortAudioClient::Detach()
{
	if (m_Client)
	{
		cerr<<"Detaching from portaudio"<<endl;
		Pa_Terminate();
		m_Client=NULL;
		m_Attached=false;
	}
}

/////////////////////////////////////////////////////////////////////////////////////////////

int PortAudioClient::Process(void *inputBuffer, void *outputBuffer,
                           unsigned long framesPerBuffer,
                           PaTimestamp outTime, void *userData)
{	
	m_BufferSize=framesPerBuffer;

	if(RunCallback&&RunContext)
	{
		// do the work
		RunCallback(RunContext, framesPerBuffer);
	}
	
	if (m_RightData && m_LeftData)
	{
		float *out = (float*)outputBuffer;
		for (unsigned int n=0; n<m_BufferSize; n++)
		{
			*out=m_LeftData[n];
			out++;
			*out=m_RightData[n];
			out++;
		}
	}
		
	if (m_RightInData && m_LeftInData)
	{
		float *in = (float*)inputBuffer;
		for (unsigned int n=0; n<m_BufferSize; n++)
		{
			m_LeftInData[n]=*in;
			in++;
			m_RightInData[n]=*in;
			in++;
		}
	}
	return 0;
}

