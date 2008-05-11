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
#include <math.h>
#include "Sample.h"

#ifndef GRAPHNODE
#define GRAPHNODE

using namespace std;
using namespace spiralcore;

/////////////////////////////////////////////////
/////////////////////////////////////////////////

class GraphNode
{
public:
	GraphNode(unsigned int numinputs);
	virtual ~GraphNode();
	
	virtual void Trigger(float time) {}
	virtual void Process(unsigned int bufsize)=0;
	virtual float GetValue() { return 0; }
	virtual bool IsTerminal() { return false; }
	virtual Sample &GetOutput() { return m_Output; }
	virtual void Clear();
	
	void TriggerChildren(float time);
	void ProcessChildren(unsigned int bufsize);
	void SetChild(unsigned int num, GraphNode *s);
	bool ChildExists(unsigned int num);
	GraphNode* GetChild(unsigned int num);
	Sample &GetInput(unsigned int num);
	float GetCVValue();
	
protected:
	Sample m_Output;
	
private:
	vector<GraphNode*> m_ChildNodes;
};

#endif
