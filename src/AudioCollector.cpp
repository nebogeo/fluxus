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

#include <limits.h>
#include <iostream>
#include <sndfile.h>
#include "AudioCollector.h"
#include "PortAudioClient.h"

static const int MAX_FFT_LENGTH = 4096;
static int XRanges[NUM_BARS+1] = {0, 1, 2, 3, 5, 7, 10, 14, 20, 28, 40, 54, 74, 101, 137, 187, 255};

FFT::FFT(int length) :
m_FFTLength(length),
m_In(new double[length]),
m_Spectrum(new fftw_complex[length])
{
	m_Plan = fftw_plan_dft_r2c_1d(m_FFTLength, m_In, m_Spectrum, FFTW_ESTIMATE);
}

FFT::~FFT()
{
	delete[] m_In;
	fftw_destroy_plan(m_Plan);
}

void FFT::Impulse2Freq(float *imp, float *out)
{
  unsigned int i;

  for (i=0; i<m_FFTLength; i++)
  {
    m_In[i] = imp[i];
  }

  fftw_execute(m_Plan);

  for (i=0; i<m_FFTLength; i++)
  {
    out[i] = m_Spectrum[i][0];
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////////////

AudioCollector::AudioCollector(int BufferLength, int FFTBuffers) :
m_Gain(0.025),
m_FFT(BufferLength),
m_FFTBuffers(FFTBuffers),
m_JackBuffer(NULL),
m_OSSBuffer(NULL),
m_OneOverSHRT_MAX(1/(float)SHRT_MAX),
m_Processing(false),
m_ProcessPos(0)
{
	m_BufferLength = BufferLength;
	m_Buffer = new float[BufferLength];
	m_FFTBuffer = new float[BufferLength*m_FFTBuffers];
	m_JackBuffer = new float[BufferLength];
	m_AudioBuffer = new float[BufferLength];
	m_FFTOutput = new float[NUM_BARS];
	for (int n=0; n<NUM_BARS; n++) m_FFTOutput[n]=0;
	m_BufferLength=BufferLength;

	m_JackBuffer = new float[BufferLength];
	m_Mutex = new pthread_mutex_t;
	pthread_mutex_init(m_Mutex,NULL);
	
	m_BufferLength=BufferLength;
	PortAudioClient *Jack = PortAudioClient::Get();
	Jack->SetCallback(AudioCallback,(void*)this);
	Jack->Attach("fluxus");
	if (Jack->IsAttached())
	{	
		Jack->SetInputs(m_JackBuffer,m_JackBuffer);
	}
}

AudioCollector::~AudioCollector()
{
	PortAudioClient::Get()->Detach();
}

bool AudioCollector::IsConnected()
{
	return PortAudioClient::Get()->IsAttached();
}

float AudioCollector::GetHarmonic(int h)
{
	return  m_FFTOutput[h%NUM_BARS];
}

float *AudioCollector::GetFFT()
{
	if (m_Processing)
	{
		if (m_ProcessPos+m_BufferLength<m_ProcessLength)
		{
			m_FFT.Impulse2Freq(m_ProcessBuffer+m_ProcessPos,m_FFTBuffer);
			m_ProcessPos+=m_BufferLength;
		}
		else
		{
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
	
	
	for (int n=0; n<NUM_BARS; n++)
	{
		float Value = 0;
	 	for (int i=XRanges[n]; i<XRanges[n+1]; i++)
	 	{			
			Value += m_FFTBuffer[i];
		}
		Value*=Value;
		Value*=m_Gain;
		if (Value>1.0) Value=1.0;
		if (Value<0.0) Value=0.0;
		
		float Bias=1.5;
		m_FFTOutput[n]=((m_FFTOutput[n]*Bias)+Value*(1/Bias))/2.0f;
	}
	
	/*for (int n=1; n<NUM_BARS-1; n++)
	{
		m_FFTOutput[n]=(m_FFTOutput[n-1]+m_FFTOutput[n]+m_FFTOutput[n+1])/3.0f;
	} */
	
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
	if (!pthread_mutex_trylock(m_Mutex))
	{
		memcpy((void*)m_Buffer,(void*)m_JackBuffer,m_BufferLength*sizeof(float));
		pthread_mutex_unlock(m_Mutex);
	}
}

void AudioCollector::AudioCallback(void *Context, unsigned int Size)
{
	((AudioCollector*)Context)->AudioCallback_i(Size);
}
