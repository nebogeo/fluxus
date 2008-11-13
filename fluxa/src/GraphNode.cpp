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

#include "GraphNode.h"
#include <iostream>
#include <string>

using namespace std;

///////////////////////////////////////////
	
GraphNode::GraphNode(unsigned int numinputs) 
{ 
	for(unsigned int n=0; n<numinputs; n++)
	{
		m_ChildNodes.push_back(NULL);
	}
	
	m_Output.Allocate(1);
}

GraphNode::~GraphNode() 
{
	Clear();
}

void GraphNode::TriggerChildren(float time)
{
	for(vector<GraphNode*>::iterator i=m_ChildNodes.begin(); 
		i!=m_ChildNodes.end(); ++i)
	{
		if (*i!=NULL)
		{
			(*i)->Trigger(time);
		}
	}
}

void GraphNode::ProcessChildren(unsigned int bufsize)
{
	for(vector<GraphNode*>::iterator i=m_ChildNodes.begin(); 
		i!=m_ChildNodes.end(); ++i)
	{
		if (*i!=NULL)
		{
			(*i)->Process(bufsize);
		}
	}
}

void GraphNode::Clear()
{
	for(unsigned int n=0; n<m_ChildNodes.size(); n++)
	{
		m_ChildNodes[n]=NULL;
	}
}

void GraphNode::SetChild(unsigned int num, GraphNode *s)
{
	if(num<m_ChildNodes.size())
	{ 
		m_ChildNodes[num]=s;
	}
}

bool GraphNode::ChildExists(unsigned int num)
{
	return GetChild(num)!=NULL;
}

GraphNode* GraphNode::GetChild(unsigned int num) 
{
	if(num<m_ChildNodes.size())
	{ 
		return m_ChildNodes[num]; 
	}
	return NULL;
}

Sample &GraphNode::GetInput(unsigned int num) 
{ 
	assert(GetChild(num)!=NULL);
	return GetChild(num)->GetOutput(); 
}

float GraphNode::GetCVValue()
{
	if (IsTerminal()) return GetValue();
	else return GetOutput()[(unsigned int)0];
}
