#include "TurtleBuilder.h"

using namespace fluxus;

TurtleBuilder::TurtleBuilder() : 
m_Built(false),
m_CurrentPrim(NULL) 
{
	Reset();
}

void TurtleBuilder::Reset() 
{ 
	m_State.clear();
	Push();
	m_State.begin()->m_Pos=dVector(0,0,0); 
	m_State.begin()->m_Rot=dVector(0,0,0); 
	m_State.begin()->m_Colour=dColour(1,1,1); 
}
	
void TurtleBuilder::Prim(int Type)
{
	if(m_CurrentPrim && !m_Built) delete m_CurrentPrim;
	m_CurrentPrim=new PolyPrimitive((PolyPrimitive::Type)Type);
	m_Built=false;
}

void TurtleBuilder::Vert()
{
	if (m_CurrentPrim && !m_Built) 
	{
		m_CurrentPrim->AddVertex(dVertex(m_State.begin()->m_Pos,dVector(0,1,0)));
	}
}

int TurtleBuilder::Build(Renderer *renderer)
{
	if (m_CurrentPrim && !m_Built) 
	{
		m_Built=true;
		return renderer->AddPrimitive(m_CurrentPrim);
	}
	return -1;
}

void TurtleBuilder::Move(float d)
{
	dVector offset(d,0,0);
	dMatrix mat;
	mat.rotxyz(m_State.begin()->m_Rot.x,m_State.begin()->m_Rot.y,m_State.begin()->m_Rot.z);
	offset=mat.transform(offset);
	m_State.begin()->m_Pos+=offset;
}

void TurtleBuilder::Turn(dVector a)
{
	m_State.begin()->m_Rot+=a;
}

void TurtleBuilder::Colour(dColour c)
{
	m_State.begin()->m_Colour=c;
}

void TurtleBuilder::Push()
{
	if (m_State.size()<1)
	{
		State state;
		m_State.push_front(state);
	}
	else
	{
		m_State.push_front(*m_State.begin());
	}
}

void TurtleBuilder::Pop()
{
	if (m_State.size()>1);
	{
		m_State.pop_front();
	}
}

