// Copyright (C) 2004 David Griffiths <dave@pawfal.org>
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

#include "Types.h"
#include "Sample.h"

#ifndef MODULES
#define MODULES

using namespace spiralcore;
using namespace std;

static const int NUM_TABLES = 9;
static const int DEFAULT_TABLE_LEN = 1024;
static const int FILTER_GRANULARITY = 10;
static const float PI=3.141592654;
static const float RAD=(PI/180.0)*360.0;

float RandRange(float L, float H);
void Crush(Sample &buf, float freq, float bits);
void Distort(Sample &buf, float amount);
void HardClip(Sample &buf, float level);

class Module
{
public:
	Module(int SampleRate) : m_SampleRate(SampleRate) {}
	virtual ~Module() {}
	
	virtual void Process(unsigned int BufSize, Sample &In) {}
	virtual void Trigger(float time, float pitch, float vol) {}
	virtual void Reset() {}
protected:
	int m_SampleRate;
};


class WaveTable : public Module
{
public:
	WaveTable(int SampleRate);
	~WaveTable() {}
	
	typedef char Type;
	enum {SINE,SQUARE,SAW,REVSAW,TRIANGLE,PULSE1,PULSE2,NOISE,PINKNOISE};

	virtual void Process(unsigned int BufSize, Sample &In);
	virtual void ProcessFM(unsigned int BufSize, Sample &In, const Sample &Pitch);
	void SimpleProcess(unsigned int BufSize, Sample &In);
	virtual void Trigger(float time, float pitch, float slidepitch, float vol);
	virtual void Reset();

	static void WriteWaves();
	
	void SetVolume(float s)   { m_Volume=s; }
	void SetType(Type s)      { m_Type=s; }
	void SetOctave(int s)     { m_Octave=s; }
	void SetFineFreq(float s) { m_FineFreq=s; }
	void SetSlideLength(float s) { m_SlideLength=s; }
	
private:
	
	float m_Pitch;
	float m_TargetPitch;
	float m_Volume;
	int   m_Note;
	float m_CyclePos;
	Type  m_Type;
	int   m_Octave;
	float m_FineFreq;
	float m_SlideTime;
	float m_SlideLength;
	float m_TimePerSample;
	float m_TablePerSample;
		
	static Sample m_Table[NUM_TABLES];
	static int    m_TableLength;
};

class SimpleWave : public Module
{
public:
	SimpleWave(int SampleRate);
	~SimpleWave() {}
	
	virtual void Process(unsigned int BufSize, Sample &In);
	virtual void Trigger(float time, float pitch, float slidepitch, float vol);
	virtual void Reset();
	
	void WriteWaves();
	
	void SetVolume(float s)   { m_Volume=s; }
	void SetFineFreq(float s) { m_FineFreq=s; }
	
private:
	
	float m_Pitch;
	float m_SlidePitch;
	float m_Volume;
	int   m_Note;
	float m_CyclePos;
	float m_FineFreq;
		
	Sample m_Table;
	int    m_TableLength;
};

class Envelope : public Module
{
public:
	Envelope(int SampleRate);
	virtual ~Envelope() {}
	
	virtual void Process(unsigned int BufSize, Sample &In, Sample &CV, bool Smooth=true);
	virtual void Trigger(float time, float pitch, float vol);
	virtual void Reset();

	void SetAttack(float s)  { m_Attack=s; }
	void SetDecay(float s)   { m_Decay=s; }
	void SetSustain(float s) { m_Sustain=s; }
	void SetRelease(float s) { m_Release=s; }
	void SetVolume(float s)  { m_Volume=s; }

protected:
	bool   m_Trigger;
	float  m_t;
	float m_Attack;
	float m_Decay;
	float m_Sustain;
	float m_Release;
	float m_Volume;	
	float m_SampleTime;
	float m_Current;

};

class SimpleEnvelope : public Module
{
public:
	SimpleEnvelope(int SampleRate);
	virtual ~SimpleEnvelope() {}
	
	virtual void Process(unsigned int BufSize, Sample &In, Sample &CV, bool Smooth=true);
	virtual void Trigger(float time, float pitch, float vol);
	virtual void Reset();

	void SetDecay(float s)   { m_Decay=s; }
	void SetVolume(float s)  { m_Volume=s; }

protected:
	bool  m_Trigger;
	float m_t;
	float m_Decay;
	float m_Volume;	
	float m_SampleTime;
	float m_Current;
};

class MoogFilter : public Module
{
public:
	MoogFilter(int SampleRate);
	virtual ~MoogFilter() {}
	
	virtual void Process(unsigned int BufSize, Sample &In, Sample &CutoffCV, Sample *LPFOut, Sample *BPFOut, Sample *HPFOut);
	virtual void Reset();

	void SetCutoff(float s) { Cutoff=s; }
	void SetResonance(float s) { if (s<0.5 && s>=0.0) Resonance=s; }
	
protected:
	float Cutoff, Resonance;
	
	float fs, fc;
	float f,p,q;
	float b0,b1,b2,b3,b4;
	float t1,t2;
	
	float in1,in2,in3,in4,out1,out2,out3,out4;
};

class FormantFilter : public Module
{
public:
 	FormantFilter(int SampleRate);
	virtual ~FormantFilter() {}
	virtual void Process(unsigned int BufSize, Sample &in, Sample &CutoffCV, Sample &out);
	virtual void Reset();

	void SetCutoff(float s) { m_Vowel=s; }
	void SetResonance(float s) {}

private:
	float m_Vowel;
		
	float memory[5][10];
};

// a wrapper for the other filters
class FilterWrapper : public Module
{
public:
	FilterWrapper(int SampleRate);
	virtual ~FilterWrapper() {}
	
	enum Type {MOOG_LO,MOOG_BAND,MOOG_HI,FORMANT};
	
	void SetType(Type s) { m_Type=s; }
	void SetCutoff(float s) { m_MoogFilter.SetCutoff(s); m_FormantFilter.SetCutoff(s); }
	void SetResonance(float s) { m_MoogFilter.SetResonance(s); m_FormantFilter.SetResonance(s); }

	virtual void Process(unsigned int BufSize, Sample &in, Sample &CutoffCV, Sample &out);
	virtual void Reset();

private:
	MoogFilter m_MoogFilter;
	FormantFilter m_FormantFilter;
	Type m_Type;
};

class Delay : public Module
{
public:
	Delay(int SampleRate);
	virtual ~Delay() {}
	
	virtual void Process(unsigned int BufSize, Sample &In, Sample &DelayCV, Sample &FeedbackCV, Sample &Out);
	virtual void Process(unsigned int BufSize, Sample &In, Sample &Out);
	virtual void Reset();

	void SetDelay(float s) { m_Delay=s; }
	void SetFeedback(float s) { m_Feedback=s; }
	
protected:
	float m_Delay, m_Feedback;
	unsigned int m_Position;
	Sample m_Buffer;
};

class Eq : public Module
{
public:
	Eq(int SampleRate);
	virtual ~Eq() {}
	
	virtual void Process(unsigned int BufSize, Sample &In);

	void SetLow(float s) { m_Low=s; }
	void SetMid(float s) { m_Mid=s; }
	void SetHigh(float s) { m_High=s; }
	
protected:
	// Filter #1 (Low band)
	float  lf;       // Frequency
	float  f1p0;     // Poles ...
	float  f1p1;    
	float  f1p2;
	float  f1p3;

	// Filter #2 (High band)
	float  hf;       // Frequency
	float  f2p0;     // Poles ...
	float  f2p1;
	float  f2p2;
	float  f2p3;

	// Sample history buffer
	float  sdm1;     // Sample data minus 1
	float  sdm2;     //                   2
	float  sdm3;     //                   3

	float m_Low;
	float m_Mid;
	float m_High;
	
};


class Compressor : public Module
{
public:
	Compressor(int SampleRate);
	virtual ~Compressor() {}
	
	virtual void Process(unsigned int BufSize, Sample &In);

	void SetAttack(float s) { tatt=s*1e-3; }
	void SetRelease(float s) { trel=s*1e-3; }
	void SetThreshold(float s) { threshold=s; }
	void SetSlope(float s) { slope=s; }
	
protected:

	float threshold;  // threshold (percents)    
	float slope;      // slope angle (percents)
    int   sr;         // sample rate (smp/sec)
    float tla;        // lookahead  (ms)
    float twnd;       // window time (ms)
    float tatt;       // attack time  (ms)
    float trel;       // release time (ms)
};
#endif
