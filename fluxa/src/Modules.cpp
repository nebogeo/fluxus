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

#include "Modules.h"
#include <stdlib.h>
#include <math.h>

using namespace std;

static float SmallNumber = (1.0 / 4294967295.0);   // Very small amount (Denormal Fix)

float RandRange(float L, float H)
{
	return ((rand()%10000/10000.0f)*(H-L))+L;
}

void Crush(Sample &buf, float freq, float bits)
{
	float step = pow((float)0.5,(float)bits);
    float phasor = 1;
    float last = 0;

    for(unsigned int i=0; i<buf.GetLength(); i++)
	{
       phasor = phasor + freq;
       if (phasor >= 1.0)
	   {
          phasor = phasor - 1.0;
          last = step * floor( buf[i]/step + 0.5 ); 
       }
       buf[i] = last; 
    }
}

void Distort(Sample &buf, float amount)
{
	if (amount>=0.99) amount = 0.99;
	
	float k=2*amount/(1-amount);
	
	for(unsigned int i=0; i<buf.GetLength(); i++)
	{
		buf[i]=((1+k)*buf[i]/(1+k*fabs(buf[i])))*(1-amount);
	}
}

void HardClip(Sample &buf, float level)
{
	if (!level) level==0.0001;
	
	for(unsigned int i=0; i<buf.GetLength(); i++)
	{
		if (buf[i]>level) buf[i]=level;
		if (buf[i]<-level) buf[i]=-level;
		buf[i]*=1/level;
	}
}

///////////////////////////////////////////////////////////////////////////

unsigned int WaveTable::m_TableLength=DEFAULT_TABLE_LEN;
Sample WaveTable::m_Table[NUM_TABLES];

WaveTable::WaveTable(int SampleRate) :
Module(SampleRate)
{
	m_TimePerSample=1/(float)m_SampleRate;
	m_CyclePos=0;	
	m_Pitch=m_SampleRate/DEFAULT_TABLE_LEN;
	m_TargetPitch=m_Pitch;
	m_Volume=1.0f;
	m_SlideTime=0;
	Reset();
	m_TablePerSample=m_TableLength/(float)SampleRate;
}

void WaveTable::Reset()
{
	m_Type=SINE;
	m_Octave=0;
	m_FineFreq=1.0f;
	m_SlideTime=0;
	m_SlideLength=0;
}

void WaveTable::WriteWaves()
{
	for (int n=0; n<NUM_TABLES; n++)
	{
		m_Table[n].Allocate(m_TableLength);
	}
	
	float RadCycle = (M_PI/180)*360;
	float Pos=0; 

	for (unsigned int n=0; n<m_TableLength; n++)
	{
		if (n==0) Pos=0;
		else Pos=(n/(float)m_TableLength)*RadCycle;
		m_Table[NOISE].Set(n,RandRange(-1,1));		
	}
	
	// todo - might be better to run this a few cycles before storing
	float White=0;
	float b0=0,b1=0,b2=0,b3=0,b4=0,b5=0,b6=0;
	for (unsigned int n=0; n<m_TableLength; n++)
	{
		White=(1.0f-((rand()%INT_MAX)/(float)INT_MAX)*2.0)*0.2f;
		b0 = 0.99886f * b0 + White * 0.0555179f;
 		b1 = 0.99332f * b1 + White * 0.0750759f;
  		b2 = 0.96900f * b2 + White * 0.1538520f;
  		b3 = 0.86650f * b3 + White * 0.3104856f;
  		b4 = 0.55000f * b4 + White * 0.5329522f;
  		b5 = -0.7616f * b5 - White * 0.0168980f;
  		m_Table[PINKNOISE].Set(n,b0 + b1 + b2 + b3 + b4 + b5 + b6 + White * 0.5362f);
  		b6 = White * 0.115926f;
	}
	
	for (unsigned int n=0; n<m_TableLength; n++)
	{
		if (n==0) Pos=0;
		else Pos=(n/(float)m_TableLength)*RadCycle;
		m_Table[SINE].Set(n,sin(Pos));		
	}

	for (unsigned int n=0; n<m_TableLength; n++)
	{
		if (n<m_TableLength/2) m_Table[SQUARE].Set(n,1.0f);
		else m_Table[SQUARE].Set(n,-1);				
	}
	
	for (unsigned int n=0; n<m_TableLength; n++)
	{
		m_Table[REVSAW].Set(n,((n/(float)m_TableLength)*2.0f)-1.0f);
	}
	
	for (unsigned int n=0; n<m_TableLength; n++)
	{
		m_Table[SAW].Set(n,1-(n/(float)m_TableLength)*2.0f);
	}

	float HalfTab=m_TableLength/2;
	float v=0;
	for (unsigned int n=0; n<m_TableLength; n++)
	{
		if (n<HalfTab) v=1-(n/HalfTab)*2.0f;
		else v=(((n-HalfTab)/HalfTab)*2.0f)-1.0f;
		v*=0.99;
		m_Table[TRIANGLE].Set(n,v);		
	}

	for (unsigned int n=0; n<m_TableLength; n++)
	{
		if (n<m_TableLength/1.2) m_Table[PULSE1].Set(n,1);
		else m_Table[PULSE1].Set(n,-1);				
	}
	
	for (unsigned int n=0; n<m_TableLength; n++)
	{
		if (n<m_TableLength/1.5) m_Table[PULSE2].Set(n,1);
		else m_Table[PULSE2].Set(n,-1);				
	}
}

void WaveTable::Trigger(float time, float pitch, float slidepitch, float vol)
{
	m_TargetPitch=pitch;
	if (m_SlideLength==0) m_Pitch=pitch; 
	m_Volume=vol*1.0f;
	m_SlideTime=0;
}

void WaveTable::Process(unsigned int BufSize, Sample &In)
{
	if (m_SlideLength>0)
	{
		float Incr;
		float Freq;
		float StartFreq=m_Pitch;
		StartFreq*=m_FineFreq;
		if (m_Octave>0) StartFreq*=1<<(m_Octave);
		if (m_Octave<0) StartFreq/=1<<(-m_Octave);

		float SlideFreq=m_TargetPitch;
		SlideFreq*=m_FineFreq;
		if (m_Octave>0) SlideFreq*=1<<(m_Octave);
		if (m_Octave<0) SlideFreq/=1<<(-m_Octave);

		for (unsigned int n=0; n<BufSize; n++)
		{	
			float t=m_SlideTime/m_SlideLength;
			if (t>1) Freq=SlideFreq;
			else Freq=(1-t)*StartFreq+t*SlideFreq;
			Incr = Freq*m_TablePerSample;
			m_CyclePos+=Incr;
			if (m_CyclePos<0) m_CyclePos=m_TableLength-m_CyclePos;
			m_CyclePos=fmod(m_CyclePos,m_TableLength-1);	
			In[n]=m_Table[(int)m_Type][m_CyclePos]*m_Volume;	
			m_SlideTime+=m_TimePerSample;
		}
	}
	else
	{
		float Incr;
		float Freq=m_Pitch;
		Freq*=m_FineFreq;
		if (m_Octave>0) Freq*=1<<(m_Octave);
		if (m_Octave<0) Freq/=1<<(-m_Octave);
		Incr = Freq*m_TablePerSample;

		for (unsigned int n=0; n<BufSize; n++)
		{	
			m_CyclePos+=Incr;
			if (m_CyclePos<0) m_CyclePos=m_TableLength-m_CyclePos;
			m_CyclePos=fmod(m_CyclePos,m_TableLength-1);
			In[n]=m_Table[(int)m_Type][m_CyclePos]*m_Volume;	
		}
	}
}

void WaveTable::ProcessFM(unsigned int BufSize, Sample &In, const Sample &Pitch)
{
	for (unsigned int n=0; n<BufSize; n++)
	{	
		if (isfinite(Pitch[n]))
		{
			m_CyclePos+=Pitch[n]*m_TablePerSample;
			if (m_CyclePos<0) m_CyclePos=m_TableLength-m_CyclePos;
			m_CyclePos=fmod(m_CyclePos,m_TableLength-1);
			In[n]=m_Table[(int)m_Type][m_CyclePos]*m_Volume;	
		}
	}
}

void WaveTable::SimpleProcess(unsigned int BufSize, Sample &In)
{
	float Incr = m_Pitch*m_FineFreq*(m_TableLength/(float)m_SampleRate);
	for (unsigned int n=0; n<BufSize; n++)
	{	
		m_CyclePos+=Incr;
		if (m_CyclePos<0) m_CyclePos=m_TableLength-m_CyclePos;
		m_CyclePos=fmod(m_CyclePos,m_TableLength-1);	
		In[n]+=m_Table[(int)m_Type][m_CyclePos]*m_Volume;	
	}
}

///////////////////////////////////////////////////////////////////////////

SimpleWave::SimpleWave(int SampleRate) :
Module(SampleRate),
m_TableLength(DEFAULT_TABLE_LEN)
{
	m_CyclePos=0;
	Reset();
	
	for (int n=0; n<NUM_TABLES; n++)
	{
		m_Table.Allocate(m_TableLength);
	}
	
	WriteWaves();
}

void SimpleWave::Reset()
{
	m_Pitch=0;
	m_FineFreq=1.0f;
}

void SimpleWave::WriteWaves()
{
	float RadCycle = (M_PI/180)*360;
	float Pos=0;

	for (unsigned int n=0; n<m_TableLength; n++)
	{
		if (n==0) Pos=0;
		else Pos=(n/(float)m_TableLength)*RadCycle;
		m_Table.Set(n,sin(Pos));		
	}
}

void SimpleWave::Trigger(float time, float pitch, float slidepitch, float vol)
{
	m_Pitch=pitch; 
	m_SlidePitch=slidepitch; 
	m_Volume=vol*1.0;
}

void SimpleWave::Process(unsigned int BufSize, Sample &In)
{
	float Incr = m_Pitch*m_FineFreq*(m_TableLength/(float)m_SampleRate);
	for (unsigned int n=0; n<BufSize; n++)
	{	
		m_CyclePos+=Incr;
		if (m_CyclePos<0) m_CyclePos=m_TableLength-m_CyclePos;
		m_CyclePos=fmod(m_CyclePos,m_TableLength-1);	
		In[n]+=m_Table[m_CyclePos]*m_Volume;	
	}
}

///////////////////////////////////////////////////////////////////////////

Envelope::Envelope(int SampleRate) : 
Module(SampleRate)
{
	m_t=-1000.0f;
	m_Trigger=false;
	m_SampleTime=1.0/(float)m_SampleRate;
	m_Current=0;
	Reset();
}
	
void Envelope::Reset()
{
	m_Attack=0.0f;
	m_Decay=0.2f;
	m_Sustain=0.0f;
	m_Release=5.0f;
	m_Volume=1.0f;
	m_t=-1000.0f;
	m_Trigger=false;
	m_Current=0;
}
	
void Envelope::Process(unsigned int BufSize, Sample &CV, bool Smooth) 
{	
	if (m_Attack==0 && m_Decay==0 && m_Release==0)
	{
		return;
	}

	Smooth=true;

	// a bit of a crap filter to smooth clicks
	static float SMOOTH = 0.98;
	static float ONEMINUS_SMOOTH = 1-SMOOTH;
	
	float temp=0;
	bool Freeze=false;
	float nt;
	
	if (m_t==-1000)
	{
		CV.Zero();
		m_Current=0;
		return;
	}
	
	
	for (unsigned int n=0; n<BufSize; n++)
	{
	
		// if we are in the delay (before really being triggered)		
		if (m_t<0) 
		{
			float temp=0;
			if (!feq(temp,m_Current,0.01) && Smooth) 
			{
				// only filter if necc
				temp=(temp*ONEMINUS_SMOOTH+m_Current*SMOOTH);
			}
			CV[n]=temp;
			m_Current=temp;
			m_t+=m_SampleTime;
		}
		else // in the envelope
		{
			// if we are in the envelope...
			if (m_t>=0 && m_t<m_Attack+m_Decay+m_Release) 
			{				
				// find out what part of the envelope we are in	
				// in the attack
				if (m_t<m_Attack)
				{	
					// get normalised position to
					// get the volume between 0 and 1
					temp=m_t/m_Attack;
				}
				else
				// in the decay
				if (m_t<m_Attack+m_Decay)
				{
					// normalised position in m_Attack->m_Decay range
					nt=(m_t-m_Attack)/m_Decay;

					// volume between 1 and m_Sustain
					temp=(1-nt)+(m_Sustain*nt);
				}
				else // in the release
				{
					// normalised position in m_Decay->m_Release range
					nt=(m_t-(m_Attack+m_Decay))/m_Release;

					// volume between m_Sustain and 0			
					temp=m_Sustain*(1-nt);

					if (m_Release<0.2f)
					{
						temp=m_Sustain;
					}	

					//if (m_Trigger) Freeze=true;
				}

				temp*=m_Volume;

				if (!feq(temp,m_Current,0.01) && Smooth) 
				{
					// only filter if necc
					temp=(temp*ONEMINUS_SMOOTH+m_Current*SMOOTH);
				}
				CV[n]=temp;
				m_Current=temp;

				if (!Freeze) m_t+=m_SampleTime;
			}
			else
			{
				if (!feq(temp,m_Current,0.01) && Smooth) 
				{
					temp=m_Current*SMOOTH;
				}

				CV[n]=temp;
				m_Current=temp;

				// if we've run off the end
				if (m_t>m_Attack+m_Decay+m_Release)
				{
					m_t=-1000;
				}
			}
		}
	} 	
}

void Envelope::Trigger(float time, float pitch, float vol) 
{
	if (vol<0.0001)
	{
		if (m_t!=-1000 && m_t<m_Attack+m_Decay+m_Release) m_t=m_Attack+m_Decay+m_Release;
	}
	else
	{
		m_t=time; 
	}
}

///////////////////////////////////////////////////////////////////////////

SimpleEnvelope::SimpleEnvelope(int SampleRate) : 
Module(SampleRate)
{
	m_Trigger=false;
	m_t=-1.0f;
	m_Current=0;
	m_SampleTime=1.0/(float)m_SampleRate;
	Reset();
	m_Decay=1.0f;
	m_Volume=1.0f;
}
	
void SimpleEnvelope::Reset()
{
	//m_Decay=1.0f;
	//m_Volume=0.1f;
}

void SimpleEnvelope::Process(unsigned int BufSize, Sample &In, Sample &CV, bool Smooth) 
{	
	// a bit of a crap filter to smooth clicks
	static float SMOOTH = 0.999;
	static float ONEMINUS_SMOOTH = 1-SMOOTH;
	float OneOverDecay=1/m_Decay;
	float temp=0;
	
	if (m_t==-1000)
	{
		In.Zero();
		CV.Zero();
		m_Current=0;
		return;
	}

	for (unsigned int n=0; n<BufSize; n++)
	{
		// if we are in the delay (before really being triggered)		
		if (m_t<0) 
		{
			In[n]*=m_Current; 
			CV[n]=m_Current;
		}
		else // in the envelope
		{
			// if we are in the envelope...
			if (m_t<m_Decay) 
			{				
				// in the decay
				temp=(1-m_t*OneOverDecay)*m_Volume;
				if (!feq(temp,m_Current,0.01) && Smooth) 
				{
					// only filter if necc
					temp=(temp*ONEMINUS_SMOOTH+m_Current*SMOOTH);
				}
				In[n]*=temp; 
				CV[n]=temp;
				m_Current=temp;
			}
			else
			{
				In[n]*=0; 
				CV[n]=0;
				m_Current=0;
				
				// we've run off the end
				m_t=-1000;
			}
		}
		
		m_t+=m_SampleTime;
	} 	
}

void SimpleEnvelope::Trigger(float time, float pitch, float vol) 
{
	m_t=time; 
}

///////////////////////////////////////////////////////////////////////////
// CSound source code, Stilson/Smith CCRMA paper., Paul Kellett version
// Moog VCF, variation 1 from musicdsp archive 

MoogFilter::MoogFilter(int SampleRate) :
Module(SampleRate),
fs(SampleRate),
fc(1000.0f),
f(0.0f),
p(0.0f),
q(0.0f),
b0(0.1f),
b1(0.1f),
b2(0.0f),
b3(0.0f),
b4(0.0f),
t1(0.0f),
t2(0.0f)
{
	Reset();
}

void MoogFilter::Reset()
{
	Cutoff=0.5f;
	Resonance=0.0f;
}

void MoogFilter::Process(unsigned int BufSize, Sample &In, Sample *CutoffCV, Sample *LPFOut, Sample *BPFOut, Sample *HPFOut)
{
	float in=0,Q=0;
	for (unsigned int n=0; n<BufSize; n++)
	{
		if (n%FILTER_GRANULARITY==0)
		{
			fc = Cutoff;
			if (CutoffCV!=NULL) fc+=(*CutoffCV)[n]; 
			fc*=0.25;
			if (fc<0) fc=0;
			else if (fc>1) fc=1;
			
			q = 1.0f - fc;
			p = fc + 0.8f * fc * q;
			f = p + p - 1.0f;
			Q = Resonance*6-3;
			q = Q + (1.0f + 0.5f * q * (1.0f - q + 5.6f * q * q));
		}
		
		in = In[n];
		
		// say no to denormalisation!
		in+=(rand()%1000)*0.000000001;	
		
		in -= q * b4;
		
		if (in>1) in=1;
		if (in<-1) in=-1;
								
		t1 = b1; b1 = (in + b0) * p - b1 * f;
		t2 = b2; b2 = (b1 + t1) * p - b2 * f;
		t1 = b3; b3 = (b2 + t2) * p - b3 * f;		
     		     b4 = (b3 + t1) * p - b4 * f;	
		b4 = b4 - b4 * b4 * b4 * 0.166667f;
		
		b0 = in;
		
		if (LPFOut) (*LPFOut)[n]=b4;	 
		if (BPFOut) (*BPFOut)[n]=(in-b4);
		if (HPFOut) (*HPFOut)[n]=3.0f * (b3 - b4);			
	}			
}

///////////////////////////////////////////////////////////////////////////

//-------------------------------------------------------------VOWEL COEFFICIENTS
const float coeff[5][11]= {
{ 8.11044e-06,
8.943665402, -36.83889529, 92.01697887, -154.337906, 181.6233289,
-151.8651235,   89.09614114, -35.10298511, 8.388101016, -0.923313471  ///A
},
{4.36215e-06,
8.90438318, -36.55179099, 91.05750846, -152.422234, 179.1170248,  ///E
-149.6496211,87.78352223, -34.60687431, 8.282228154, -0.914150747
},
{ 3.33819e-06,
8.893102966, -36.49532826, 90.96543286, -152.4545478, 179.4835618,
-150.315433, 88.43409371, -34.98612086, 8.407803364, -0.932568035  ///I
},
{1.13572e-06,
8.994734087, -37.2084849, 93.22900521, -156.6929844, 184.596544,   ///O
-154.3755513, 90.49663749, -35.58964535, 8.478996281, -0.929252233
},
{4.09431e-07,
8.997322763, -37.20218544, 93.11385476, -156.2530937, 183.7080141,  ///U
-153.2631681, 89.59539726, -35.12454591, 8.338655623, -0.910251753
}
};

FormantFilter::FormantFilter(int SampleRate) :
Module(SampleRate)
{
	Reset();
}

void FormantFilter::Reset()
{
	for (int x=0; x<5; x++)
		for (int y=0; y<10; y++) 
			memory[x][y]=0;
	m_Vowel=0;
}

void FormantFilter::Process(unsigned int BufSize, Sample &In, Sample *CutoffCV, Sample &Out)
{		
	float res,o[5],out=0, in=0;
		
	for (unsigned int n=0; n<BufSize; n++)
	{		
		in = In[n];
		
		// work around denormal calculation CPU spikes where in --> 0
		if ((in >= 0) && (in < 0.000000001))
			in += 0.000000001;
		else
			if ((in <= 0) && (in > -0.000000001))
				in -= 0.000000001;

		for (int v=0; v<5; v++)
		{
			res= (float) (coeff[v][0]*(in*0.1f) +
					  coeff[v][1]*memory[v][0] +  
					  coeff[v][2]*memory[v][1] +
					  coeff[v][3]*memory[v][2] +
					  coeff[v][4]*memory[v][3] +
					  coeff[v][5]*memory[v][4] +
					  coeff[v][6]*memory[v][5] +
					  coeff[v][7]*memory[v][6] +
					  coeff[v][8]*memory[v][7] +
					  coeff[v][9]*memory[v][8] +
					  coeff[v][10]*memory[v][9] );


			memory[v][9]=memory[v][8];
			memory[v][8]=memory[v][7];
			memory[v][7]=memory[v][6];
			memory[v][6]=memory[v][5];
			memory[v][5]=memory[v][4];
			memory[v][4]=memory[v][3];
			memory[v][3]=memory[v][2];
			memory[v][2]=memory[v][1];
			memory[v][1]=memory[v][0];
			memory[v][0]=(float) res;

			o[v]=res;
		}

		float vowel=m_Vowel;
		if (CutoffCV!=NULL) vowel+=(*CutoffCV)[n];

		// mix between vowel sounds
		if (vowel<1) 
		{
			out=Linear(0,1,vowel,o[1],o[0]); 
		}	
		else 
		if (vowel>1 && vowel<2) 
		{
			out=Linear(0,1,vowel-1.0f,o[2],o[1]);
		}	
		else 
		if (vowel>2 && vowel<3) 
		{
			out=Linear(0,1,m_Vowel-2.0f,o[3],o[2]);
		}	
		else 
		if (vowel>3 && vowel<4) 
		{
			out=Linear(0,1,vowel-3.0f,o[4],o[3]);
		}	
		else 
		if (vowel==4) 
		{
			out=o[4];
		}	
	
		Out[n]=out;
	}	
}

///////////////////////////////////////////////////////////////////////////

FilterWrapper::FilterWrapper(int SampleRate):
Module(SampleRate),
m_MoogFilter(SampleRate),
m_FormantFilter(SampleRate)
{
}

void FilterWrapper::Reset()
{
	m_MoogFilter.Reset();
	m_FormantFilter.Reset();
}

void FilterWrapper::Process(unsigned int BufSize, Sample &in, Sample &CutoffCV, Sample &out)
{
	switch (m_Type)
	{
		case MOOG_LO : m_MoogFilter.Process(BufSize, in, &CutoffCV, &out, NULL, NULL); break;
		case MOOG_BAND : m_MoogFilter.Process(BufSize, in, &CutoffCV, NULL, &out, NULL); break;
		case MOOG_HI : m_MoogFilter.Process(BufSize, in, &CutoffCV, NULL, NULL, &out); break;
		case FORMANT : m_FormantFilter.Process(BufSize, in, &CutoffCV, out); break;
	}		
}

void FilterWrapper::Process(unsigned int BufSize, Sample &in, Sample &out)
{
	switch (m_Type)
	{
		case MOOG_LO : m_MoogFilter.Process(BufSize, in, NULL, &out, NULL, NULL); break;
		case MOOG_BAND : m_MoogFilter.Process(BufSize, in, NULL, NULL, &out, NULL); break;
		case MOOG_HI : m_MoogFilter.Process(BufSize, in, NULL, NULL, NULL, &out); break;
		case FORMANT : m_FormantFilter.Process(BufSize, in, NULL, out); break;
	}		
}

///////////////////////////////////////////////////////////////////////////

static const float MAX_DELAYTIME=2.0;

Delay::Delay(int SampleRate) :
Module(SampleRate),
m_Position(0)
{
	Reset();
	m_Buffer.Allocate((int)(MAX_DELAYTIME*m_SampleRate));
}

void Delay::Reset()
{
	m_Delay=0;
	m_Feedback=0;
}

void Delay::Process(unsigned int BufSize, Sample &In, Sample &DelayCV, Sample &FeedbackCV, Sample &Out)
{
	
}

void Delay::Process(unsigned int BufSize, Sample &In, Sample &Out)
{
	unsigned int delay=(unsigned int)(m_SampleRate*m_Delay);
	
	if (delay==0) 
	{
		return;
	}

	if (delay>=(unsigned int)m_Buffer.GetLength()) delay=m_Buffer.GetLength()-1;
	
	for (unsigned int n=0; n<BufSize; n++)
	{
		m_Buffer[m_Position]=In[n]+m_Buffer[m_Position]*m_Feedback;
		Out[n]=m_Buffer[m_Position];
		m_Position=(m_Position+1)%delay;
	}
}

///////////////////////////////////////////////////////////////////////////
// (c) Neil C / Etanza Systems / 2K6
//
// Shouts / Loves / Moans = etanza at lycos dot co dot uk
//
// This work is hereby placed in the public domain for all purposes, including
// use in commercial applications.
//
// The author assumes NO RESPONSIBILITY for any problems caused by the use of
// this software.
//
//----------------------------------------------------------------------------

// NOTES :
//
// - Original filter code by Paul Kellet (musicdsp.pdf)
//
// - Uses 4 first order filters in series, should give 24dB per octave
//
// - Now with P4 Denormal fix :)

Eq::Eq(int SampleRate) :
Module(SampleRate),
lf(0),
f1p0(0),
f1p1(0),
f1p2(0),
f1p3(0),
hf(0), 
f2p0(0),
f2p1(0),
f2p2(0),
f2p3(0),
sdm1(0),
sdm2(0),
sdm3(0),
m_Low(1),
m_Mid(1),
m_High(1)
{
	lf = 2 * sin(M_PI * (880.0f / (float)SampleRate));
	hf = 2 * sin(M_PI * (5000.0f / (float)SampleRate));	
}

void Eq::Process(unsigned int BufSize, Sample &In)
{
	for (unsigned int n=0; n<BufSize; n++)
	{
		float  l,m,h; // Low / Mid / High - Sample Values

		// Filter #1 (lowpass)
		f1p0  += (lf * (In[n] - f1p0));// + SmallNumber;
		f1p1  += (lf * (f1p0 - f1p1));
		f1p2  += (lf * (f1p1 - f1p2));
		f1p3  += (lf * (f1p2 - f1p3));
		l = f1p3;

		// Filter #2 (highpass)
		f2p0  += (hf * (In[n] - f2p0));// + SmallNumber;
		f2p1  += (hf * (f2p0 - f2p1));
		f2p2  += (hf * (f2p1 - f2p2));
		f2p3  += (hf * (f2p2 - f2p3));
		h = sdm3 - f2p3;

		// Calculate midrange (signal - (low + high))
		m = sdm3 - (h + l);

		// Scale, Combine and store
		l *= m_Low;
		m *= m_Mid;
		h *= m_High;

		// Shuffle history buffer
		sdm3 = sdm2;
		sdm2 = sdm1;
		sdm1 = In[n];  			  

		// Return result
		In[n]=(l + m + h);
	}
}

// Type : Hardknee compressor with RMS look-ahead envelope calculation and adjustable attack/decay
// References : Posted by flashinc[AT]mail[DOT]ru
//
// Notes :
// RMS is a true way to estimate _musical_ signal energy,
// our ears behaves in a same way.

Compressor::Compressor(int SampleRate) :
Module(SampleRate),
threshold(0.5),  
slope(0.5),      
sr(SampleRate),    
tla(1.0f*1e-3),         
twnd(3.0f*1e-3),        
tatt(0.1f*1e-3),        
trel(300.0f*1e-3)        
{
}

void Compressor::Process(unsigned int BufSize, Sample &In)
{
    // attack and release "per sample decay"
    float att=(tatt == 0.0) ? (0.0) : exp (-1.0 / (sr * tatt));
    float rel=(trel == 0.0) ? (0.0) : exp (-1.0 / (sr * trel));
    // envelope
    float env = 0.0;
    // sample offset to lookahead wnd start
    int lhsmp = (int)(sr*tla);
    // samples count in lookahead window
    int nrms = (int)(sr*twnd);

    // for each sample...
    for (unsigned int i=0; i<BufSize; ++i)
    {
        // now compute RMS
        float summ = 0;

        // for each sample in window
        for (int j=0; j<nrms; ++j)
        {
            unsigned int lki = i + j + lhsmp;
            float  smp;
            if (lki < BufSize) smp = In[lki];
            else smp = 0.0;      
            summ += smp * smp;  // square em..
        }

        float rms = sqrt (summ / nrms);   // root-mean-square
        // dynamic selection: attack or release?
        float theta = rms > env ? att : rel;
        // smoothing with capacitor, envelope extraction...
        // here be aware of pIV denormal numbers glitch
        env = (1.0 - theta) * rms + theta * env;
        // the very easy hard knee 1:N compressor
        float  gain = 1.0;
        if (env > threshold) gain = gain - (env - threshold) * slope;
        // result - two hard kneed compressed channels...
        In[i] *= gain;
		if (i==0) cerr<<threshold<<" "<<env<<" "<<gain<<endl;
    }
	
	
	
}
