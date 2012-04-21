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

#include <cstring>
#include <limits.h>
#include <iostream>
#include <sndfile.h>
#include "AudioCollector.h"
#include "JackClient.h"

static const int MAX_FFT_LENGTH = 4096;

FFT::FFT(int length) :
m_FFTLength(length),
#ifndef __FFTWFLOAT__
m_In(new double[length]),
#else
m_In(new float[length]),
#endif

#ifndef __FFTWFLOAT__
m_Spectrum(new fftw_complex[length])
{
	m_Plan = fftw_plan_dft_r2c_1d(m_FFTLength, m_In, m_Spectrum, FFTW_ESTIMATE);
}
#else
m_Spectrum(new fftwf_complex[length])
{
	m_Plan = fftwf_plan_dft_r2c_1d(m_FFTLength, m_In, m_Spectrum, FFTW_ESTIMATE);
}
#endif

FFT::~FFT()
{
	delete[] m_In;
#ifndef __FFTWFLOAT__
	fftw_destroy_plan(m_Plan);
#else
	fftwf_destroy_plan(m_Plan);
#endif
}

void FFT::Impulse2Freq(float *imp, float *out)
{
  unsigned int i;

  for (i=0; i<m_FFTLength; i++)
  {
    m_In[i] = imp[i];
  }

#ifndef __FFTWFLOAT__
  fftw_execute(m_Plan);
#else
  fftwf_execute(m_Plan);
#endif

  for (i=0; i<m_FFTLength; i++)
  {
    out[i] = m_Spectrum[i][0];
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////

AudioCollector::AudioCollector(const string &port, int BufferLength, unsigned int Samplerate, const string &portname, int FFTBuffers) :
m_Gain(1),
m_SmoothingBias(0.8),
m_FFT(BufferLength),
m_FFTBuffers(FFTBuffers),
m_JackBuffer(NULL),
m_OSSBuffer(NULL),
m_OneOverSHRT_MAX(1/(float)SHRT_MAX),
m_Processing(false),
m_ProcessPos(0),
m_NumBars(16)
{
	m_BufferLength = BufferLength;
	m_Samplerate = Samplerate;
	m_BufferTime = m_BufferLength/(float)m_Samplerate;
	
	m_Buffer = new float[BufferLength];
	memset(m_Buffer,0,BufferLength*sizeof(float));
	
	m_FFTBuffer = new float[BufferLength*m_FFTBuffers];
	memset(m_FFTBuffer,0,BufferLength*sizeof(float));
	
	m_JackBuffer = new float[BufferLength];
	memset(m_JackBuffer,0,BufferLength*sizeof(float));
	
	m_AudioBuffer = new float[BufferLength];
	memset(m_AudioBuffer,0,BufferLength*sizeof(float));
	
	m_FFTOutput = new float[m_NumBars];
	for (unsigned int n=0; n<m_NumBars; n++) m_FFTOutput[n]=0;
	
	m_Mutex = new pthread_mutex_t;
	pthread_mutex_init(m_Mutex,NULL);
	
	JackClient *Jack = JackClient::Get();
	Jack->SetCallback(AudioCallback,(void*)this);
	Jack->Attach(portname);
	if (Jack->IsAttached())
	{	
		int id=Jack->AddInputPort();
		Jack->SetInputBuf(id,m_JackBuffer);
		Jack->ConnectInput(id, port);
	}
	else
	{
		cerr<<"Could not attach to jack"<<endl;
	}
}

AudioCollector::~AudioCollector()
{
	JackClient::Get()->Detach();
}

bool AudioCollector::IsConnected()
{
	return JackClient::Get()->IsAttached();
}

float AudioCollector::GetHarmonic(int h)
{
	return  m_FFTOutput[h%m_NumBars];
}

float *AudioCollector::GetFFT()
{
	if (m_Processing)
	{
		if (m_ProcessPos+m_BufferLength<m_ProcessLength)
		{
			m_FFT.Impulse2Freq(m_ProcessBuffer+m_ProcessPos,m_FFTBuffer);
			memcpy((void*)m_AudioBuffer,(void*)(m_ProcessBuffer+m_ProcessPos),m_BufferLength*sizeof(float));
			m_ProcessPos+=m_BufferLength;
		}
		else
		{
			cerr<<"Finished processing audio file..."<<endl;
			// finished, so clean up...
			delete[] m_ProcessBuffer;
			m_ProcessPos=0;
			m_Processing=false;
		}
	}
	else
	{
		pthread_mutex_lock(m_Mutex);
		memcpy((void*)m_AudioBuffer,(void*)m_Buffer,m_BufferLength*sizeof(float));
		pthread_mutex_unlock(m_Mutex);

		m_FFT.Impulse2Freq(m_AudioBuffer,m_FFTBuffer);
	}

	// seem to only have stuff in the lower half - something to do with nyquist?
	float UsefulArea = m_BufferLength/2;

	for (unsigned int n=0; n<m_NumBars; n++)
	{
		float Value = 0;

		float f = n/(float)m_NumBars;
		float t = (n+1)/(float)m_NumBars;
		f*=f;
		t*=t;
		unsigned from = f*UsefulArea;
		unsigned to = t*UsefulArea;

		for (unsigned int i=from; i<=to; i++)
		{
			if (i<m_BufferLength)
			{
				Value += m_FFTBuffer[i];
			}
		}

		if (Value<0) Value=-Value;
		Value*=m_Gain;
		m_FFTOutput[n]=((m_FFTOutput[n]*m_SmoothingBias)+Value*(1-m_SmoothingBias));
	}

	return m_FFTOutput;
}


void AudioCollector::Process(const string &filename)
{
	if (m_Processing) return;

	SF_INFO info;
	info.format=0;

	SNDFILE* file = sf_open (filename.c_str(), SFM_READ, &info) ;
	if (!file)
	{
		cerr<<"Error opening ["<<filename<<"] : "<<sf_strerror(file)<<endl;
		return;
	}

	m_ProcessBuffer = new float[info.frames];
	memset((void*)m_ProcessBuffer,0,info.frames*sizeof(float));
	m_ProcessLength=info.frames;

	// mix down to mono if need be
	if (info.channels>1)
	{
		float *Buffer = new float[info.frames*info.channels];
		sf_readf_float(file,Buffer,info.frames*info.channels);
		int from=0;
		for (int n=0; n<info.frames; n++)
		{
			for (int c=0; c<info.channels; c++)
			{
				m_ProcessBuffer[n]=(m_ProcessBuffer[n]+Buffer[from++])/2.0f;
			}
		}
	}
	else
	{
		sf_readf_float(file, m_ProcessBuffer, info.frames);
	}
	sf_close(file);

	m_Processing=true;
	m_ProcessPos=0;
}

void AudioCollector::AudioCallback_i(unsigned int Size)
{
	if (Size<=m_BufferLength && !pthread_mutex_trylock(m_Mutex))
	{
		memcpy((void*)m_Buffer,(void*)m_JackBuffer,m_BufferLength*sizeof(float));
		pthread_mutex_unlock(m_Mutex);
	}
}

void AudioCollector::AudioCallback(void *Context, unsigned int Size)
{
	((AudioCollector*)Context)->AudioCallback_i(Size);
}
