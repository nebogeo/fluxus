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

#include "ModuleNodes.h"
	
TerminalNode::TerminalNode(float Value):
GraphNode(0),
m_Value(Value)
{
}

OscNode::OscNode(unsigned int Shape, unsigned int SampleRate):
GraphNode(1),
m_WaveTable(SampleRate),
m_Shape(Shape)
{
	m_WaveTable.SetType(m_Shape);
}

void OscNode::Trigger(float time)
{
	TriggerChildren(time);

	float freq=440;
	if (ChildExists(0) && GetChild(0)->IsTerminal()) 
	{
		freq=GetChild(0)->GetValue();
	}
	m_WaveTable.Trigger(time, freq, freq, 1);
}

void OscNode::Process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_Output.GetLength())
	{
		m_Output.Allocate(bufsize);
	}
	ProcessChildren(bufsize);
	
	if (ChildExists(0) && !GetChild(0)->IsTerminal())
	{
		m_WaveTable.ProcessFM(bufsize, m_Output, GetInput(0));
	}
	else
	{
		m_WaveTable.Process(bufsize, m_Output);
	}
}

ADSRNode::ADSRNode(unsigned int SampleRate):
GraphNode(4),
m_Envelope(SampleRate)
{
}

void ADSRNode::Trigger(float time)
{
	TriggerChildren(time);
	
	if (ChildExists(0)) m_Envelope.SetAttack(GetChild(0)->GetCVValue());
	if (ChildExists(1)) m_Envelope.SetDecay(GetChild(1)->GetCVValue());
	if (ChildExists(2)) m_Envelope.SetSustain(GetChild(2)->GetCVValue());
	if (ChildExists(3)) m_Envelope.SetRelease(GetChild(3)->GetCVValue());
	
	m_Envelope.Trigger(time, 0, 1);
}

void ADSRNode::Process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_Output.GetLength())
	{
		m_Output.Allocate(bufsize);
		m_Temp.Allocate(bufsize);
	}
	
	ProcessChildren(bufsize);
	m_Envelope.Process(bufsize, m_Temp, m_Output);
}

MathNode::MathNode(Type t):
GraphNode(2),
m_Type(t)
{
}

void MathNode::Trigger(float time)
{
	TriggerChildren(time);
}

void MathNode::Process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_Output.GetLength())
	{
		m_Output.Allocate(bufsize);
	}
	
	ProcessChildren(bufsize);
	
	if (ChildExists(0) && ChildExists(1))
	{
		if (GetChild(0)->IsTerminal() && GetChild(1)->IsTerminal())
		{
			float value=0;
			float v0 = GetChild(0)->GetValue();
			float v1 = GetChild(1)->GetValue();
			
			switch(m_Type)
			{
				case ADD: value=v0+v1; break;
				case SUB: value=v0-v1; break;
				case MUL: value=v0*v1; break;
				case DIV: if (v1!=0) value=v0/v1; break;
			};
			
			for (unsigned int n=0; n<bufsize; n++) m_Output[n]=value;
		}
		else if (GetChild(0)->IsTerminal() && !GetChild(1)->IsTerminal())
		{
			float v0 = GetChild(0)->GetValue();
			
			switch(m_Type)
			{
				case ADD: for (unsigned int n=0; n<bufsize; n++) m_Output[n]=v0+GetChild(1)->GetOutput()[n]; break;
				case SUB: for (unsigned int n=0; n<bufsize; n++) m_Output[n]=v0-GetChild(1)->GetOutput()[n]; break;
				case MUL: for (unsigned int n=0; n<bufsize; n++) m_Output[n]=v0*GetChild(1)->GetOutput()[n]; break;
				case DIV: 
				{
					for (unsigned int n=0; n<bufsize; n++) 
					{	
						if (GetChild(1)->GetOutput()[n]!=0)
						{
							m_Output[n]=v0/GetChild(1)->GetOutput()[n];
						}
					}
				}
				break;
			};
		}
		else if (!GetChild(0)->IsTerminal() && GetChild(1)->IsTerminal())
		{
			float v1 = GetChild(1)->GetValue();
			
			switch(m_Type)
			{
				case ADD: for (unsigned int n=0; n<bufsize; n++) m_Output[n]=GetChild(0)->GetOutput()[n]+v1; break;
				case SUB: for (unsigned int n=0; n<bufsize; n++) m_Output[n]=GetChild(0)->GetOutput()[n]-v1; break;
				case MUL: for (unsigned int n=0; n<bufsize; n++) m_Output[n]=GetChild(0)->GetOutput()[n]*v1; break;
				case DIV: 
				{
					if (v1!=0)
					{
						for (unsigned int n=0; n<bufsize; n++) 
						{
							m_Output[n]=GetChild(0)->GetOutput()[n]/v1;
						}
					}
				}
				break;
			};
		}
		else 
		{			
			switch(m_Type)
			{
				case ADD:  
				{
					for (unsigned int n=0; n<bufsize; n++) 
					{
						m_Output[n]=GetChild(0)->GetOutput()[n]+GetChild(1)->GetOutput()[n]; 				
					}
					
				} break;
				case SUB: 
				{
					for (unsigned int n=0; n<bufsize; n++) 
					{
						m_Output[n]=GetChild(0)->GetOutput()[n]-GetChild(1)->GetOutput()[n]; 
					}
				} break;
				case MUL: 
				{
					for (unsigned int n=0; n<bufsize; n++) 
					{
						m_Output[n]=GetChild(0)->GetOutput()[n]*GetChild(1)->GetOutput()[n]; 
					}
				} break;
				case DIV: 
				{
					for (unsigned int n=0; n<bufsize; n++) 
					{
						if (GetChild(1)->GetOutput()[n]!=0)
						{
							m_Output[n]=GetChild(0)->GetOutput()[n]/GetChild(1)->GetOutput()[n];
						}
					}
				}
				break;
			};
		}
	}	
}

FilterNode::FilterNode(Type t, unsigned int samplerate):
GraphNode(3),
m_Type(t),
m_Filter(samplerate)
{
	switch(m_Type)
	{
		case MOOGLP: m_Filter.SetType(FilterWrapper::MOOG_LO); break; 
		case MOOGBP: m_Filter.SetType(FilterWrapper::MOOG_BAND); break; 
		case MOOGHP: m_Filter.SetType(FilterWrapper::MOOG_HI); break; 
		case FORMANT: m_Filter.SetType(FilterWrapper::FORMANT); break; 
	};
}

void FilterNode::Trigger(float time)
{
	TriggerChildren(time);
}

void FilterNode::Process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_Output.GetLength())
	{
		m_Output.Allocate(bufsize);
		m_Temp.Allocate(bufsize);
	}
	
	ProcessChildren(bufsize);
	
	if (ChildExists(1) && ChildExists(2))
	{		
		float r=GetChild(2)->GetValue();
		if (r>=0 && r<0.5) m_Filter.SetResonance(r);
		
		if (GetChild(1)->IsTerminal())
		{
			float c=GetChild(1)->GetValue();			
			if (c>=0 && c<1) m_Filter.SetCutoff(c);
			
			m_Filter.Process(bufsize, GetInput(0), m_Temp, m_Output);
		}
		else
		{
			m_Filter.Process(bufsize, GetInput(0), GetInput(1), m_Output);
		}
	}
}

SampleNode::SampleNode(unsigned int samplerate):
GraphNode(2),
m_PlayMode(TRIGGER),
m_Sampler(samplerate)
{
}

void SampleNode::Trigger(float time)
{
	TriggerChildren(time);
	
	if (ChildExists(0))
	{
		Event e;
		e.ID = (int)GetChild(0)->GetCVValue();
		e.Frequency = GetChild(1)->GetCVValue();
		m_Sampler.Play(time, e);
	}
}

void SampleNode::Process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_Output.GetLength())
	{
		m_Output.Allocate(bufsize);
		m_Temp.Allocate(bufsize);
	}
	
	ProcessChildren(bufsize);
	m_Output.Zero();
	m_Sampler.Process(bufsize, m_Output, m_Temp);
}

EffectNode::EffectNode(Type type, unsigned int samplerate):
GraphNode(3),
m_Type(type),
m_Delay(samplerate)
{
}

void EffectNode::Trigger(float time)
{
	TriggerChildren(time);
}

void EffectNode::Process(unsigned int bufsize)
{
	if (bufsize>(unsigned int)m_Output.GetLength())
	{
		m_Output.Allocate(bufsize);
	}
	
	ProcessChildren(bufsize);

	if (ChildExists(0) && ChildExists(1) && ChildExists(2))
	{		
		switch (m_Type)
		{
			case CRUSH : m_Output=GetInput(0); Crush(m_Output, GetChild(1)->GetCVValue(), GetChild(2)->GetCVValue()); break;
			case DISTORT : m_Output=GetInput(0); Distort(m_Output, GetChild(1)->GetCVValue()); break;
			case CLIP : m_Output=GetInput(0); HardClip(m_Output, GetChild(1)->GetCVValue()); break;
			case DELAY : 
			{
				m_Delay.SetDelay(GetChild(1)->GetCVValue());
				m_Delay.SetFeedback(GetChild(2)->GetCVValue());
				m_Delay.Process(bufsize, GetInput(0), m_Output); break;
			}
		};
	}
}
