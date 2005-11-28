#include "TurtleBuilder.h"

using namespace fluxus;
	
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
		m_CurrentPrim->AddVertex(dVertex(m_Pos,dVector(0,1,0)));
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
	mat.rotxyz(m_Rot.x,m_Rot.y,m_Rot.z);
	offset=mat.transform(offset);
	m_Pos+=offset;
}

void TurtleBuilder::Turn(dVector a)
{
	m_Rot+=a;
}

void TurtleBuilder::Colour(dColour c)
{
	m_Colour=c;
}
