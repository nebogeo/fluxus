// Copyright (C) 2006 David Griffiths <dave@pawfal.org>
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

#include <vector>
#include <map>
#include <set>
#include <list>
#include <math.h>
#include "GraphNode.h"
#include "ModuleNodes.h"

#ifndef GRAPH
#define GRAPH

class Graph
{
public:
	Graph(unsigned int NumNodes, unsigned int SampleRate);
	~Graph();
	
	enum Type{TERMINAL,SINOSC,SAWOSC,TRIOSC,SQUOSC,WHITEOSC,PINKOSC,ADSR,ADD,SUB,MUL,DIV,POW,
		      MOOGLP,MOOGBP,MOOGHP,FORMANT,SAMPLER,CRUSH,DISTORT,CLIP,DELAY,KS,XFADE,SAMPNHOLD,
			  TRACKNHOLD,NUMTYPES};
	
	void Init();
	void Clear();
	void Create(unsigned int id, Type t, float v);
	void Connect(unsigned int id, unsigned int arg, unsigned int to);
	void Play(float time, unsigned int id, float pan);
	void Process(unsigned int bufsize, Sample &left, Sample &right);
	void SetMaxPlaying(int s) { m_MaxPlaying=s; }
	
private:
	class NodeDesc
	{
	public:
		NodeDesc(): m_Node(NULL), m_ID(0) {}
		GraphNode *m_Node;
		unsigned int m_ID;
	};
	
	class NodeDescVec
	{
	public:
		unsigned int NewIndex()
		{
			m_Current++;
			if (m_Current>=m_Vec.size()) 
			{
				//cerr<<"going round..."<<endl;
				m_Current=0;
			}
			return m_Current;
		}
		
		NodeDescVec(): m_Current(0) {}
		unsigned int m_Current;
		vector<NodeDesc*> m_Vec;
	};
	
	unsigned int m_MaxPlaying;
	list<pair<unsigned int, float> > m_RootNodes;
	map<unsigned int,GraphNode*> m_NodeMap;
	map<Type,NodeDescVec*> m_NodeDescMap;
	unsigned int m_NumNodes;
	unsigned int m_SampleRate;
};

#endif
