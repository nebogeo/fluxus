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
#include "Modules.h"
#include "GraphNode.h"
#include "Sampler.h"

#ifndef MODULENODES
#define MODULENODES

class TerminalNode : public GraphNode
{
public:
	TerminalNode(float Value);
	virtual void Process(unsigned int bufsize) {}
	virtual float GetValue() { return m_Value; }
	virtual void SetValue(float s) { m_Value=s; }
	virtual bool IsTerminal() { return true; }
	//virtual void Clear() { GraphNode::Clear(); m_Value=0; }
	
private:
	float m_Value;
};

class OscNode : public GraphNode
{
public:
	OscNode(unsigned int Shape, unsigned int SampleRate);
	virtual void Trigger(float time);
	virtual void Process(unsigned int bufsize);
	
private:
	WaveTable m_WaveTable;
	unsigned int m_Shape;
};

class KSNode : public GraphNode
{
public:
    KSNode(unsigned int SampleRate);
	virtual void Trigger(float time);
	virtual void Process(unsigned int bufsize);
	
private:
	KS m_KS;
	unsigned int m_Shape;
};

class XFadeNode : public GraphNode
{
public:
    XFadeNode();
	virtual void Trigger(float time);
	virtual void Process(unsigned int bufsize);
};

class HoldNode : public GraphNode
{
public:
	enum Type{SAMP, TRACK};
	
	HoldNode(Type t);
	virtual void Trigger(float time);
	virtual void Process(unsigned int bufsize);

private:
	Type m_Type;
	float m_heldValue, m_lastCtrlVal;

};


class ADSRNode : public GraphNode
{
public:
	ADSRNode(unsigned int SampleRate);
	virtual void Trigger(float time);
	virtual void Process(unsigned int bufsize);
	
private:
	Envelope m_Envelope;
	Sample m_Temp;
};

class MathNode : public GraphNode
{
public:
	enum Type{ADD,SUB,MUL,DIV,POW};

	MathNode(Type t);
	virtual void Trigger(float time);
	virtual void Process(unsigned int bufsize);
	
private:
	Type m_Type;
};

class FilterNode : public GraphNode
{
public:
	enum Type{MOOGLP,MOOGBP,MOOGHP,FORMANT};

	FilterNode(Type t, unsigned int samplerate);
	virtual void Trigger(float time);
	virtual void Process(unsigned int bufsize);
	
private:
	Type m_Type;
	FilterWrapper m_Filter;
	Sample m_Temp;
};

class SampleNode : public GraphNode
{
public:
	enum PlayMode{TRIGGER,LOOP,NOTRIGGER,REV_TRIGGER,REV_LOOP,REV_NOTRIGGER};

	SampleNode(unsigned int samplerate);
	virtual void Trigger(float time);
	virtual void Process(unsigned int bufsize);
	
private:
	PlayMode m_PlayMode;
	Sampler m_Sampler;
	Sample m_Temp;
};

class EffectNode : public GraphNode
{
public:
	enum Type{CRUSH,DISTORT,CLIP,DELAY};

	EffectNode(Type type, unsigned int samplerate);
	virtual void Trigger(float time);
	virtual void Process(unsigned int bufsize);
	
private:
	Type m_Type;
	Delay m_Delay;
};

#endif

