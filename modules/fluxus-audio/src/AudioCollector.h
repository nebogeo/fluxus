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

//#define __FFTWFLOAT__
#include <fftw3.h>
#include <pthread.h>
#include <string>

#ifndef AUDIO_COLLECTOR
#define AUDIO_COLLECTOR

using namespace std;

class FFT
{
public:
    FFT(int length);
    ~FFT();
	void Impulse2Freq(float *imp, float *out);
private:
#ifndef __FFTWFLOAT__
	fftw_plan m_Plan;
	unsigned int m_FFTLength;
	double *m_In;
	fftw_complex *m_Spectrum;
#else
	fftwf_plan m_Plan;
	unsigned int m_FFTLength;
	float *m_In;
	fftwf_complex *m_Spectrum;
#endif
};

class AudioCollector
{
public:
	AudioCollector(const string &port, int BufferLength, unsigned int Samplerate, int FFTBuffers = 1);
	~AudioCollector();

	float *GetFFT();
	float *GetAudioBuffer() { return m_AudioBuffer; }
	int GetAudioBufferLength() { return m_BufferLength; }
	float GetHarmonic(int h);
	bool  IsConnected();
	void  SetGain(float s) { m_Gain=s; }
	float  GetGain() { return m_Gain; }
	void  SetSmoothingBias(float s) { if ((s < 1) && (s >= 0)) m_SmoothingBias = s; }
	void  Process(const string &filename);
	bool  IsProcessing() { return m_Processing; }
	float BufferTime() { return m_BufferTime; }

	void SetNumBars(unsigned int s)
	{
		if (s < 1) s = 1;
		m_NumBars = s;
		delete[] m_FFTOutput;
		m_FFTOutput = new float[s];
		memset(m_FFTOutput, 0, sizeof(float) * s);
	}

	unsigned GetNumBars(void) { return m_NumBars; }

private:

    void AudioCallback_i(unsigned int);
	static void AudioCallback(void *, unsigned int);

	float m_Gain;
	float m_SmoothingBias;
	unsigned int m_Samplerate;
	float m_BufferTime;
	unsigned int m_BufferLength;
	FFT m_FFT;
	pthread_mutex_t* m_Mutex;
	float *m_Buffer;
	float *m_AudioBuffer;
	float *m_FFTBuffer;
	float *m_FFTOutput;
	int    m_FFTBuffers;
	int    m_InputPort;

	float *m_JackBuffer;

	int    m_Dspfd;
	short *m_OSSBuffer;
	float  m_OneOverSHRT_MAX;
	bool   m_Processing;
	float *m_ProcessBuffer;
	unsigned int m_ProcessPos;
	unsigned int m_ProcessLength;
    unsigned int m_NumBars;
};

#endif

