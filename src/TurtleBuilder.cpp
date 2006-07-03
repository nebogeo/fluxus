#include "TurtleBuilder.h"

using namespace fluxus;

TurtleBuilder::TurtleBuilder() : 
m_BuildingPrim(NULL),
m_AttachedPoints(NULL),
m_Position(0)
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

void TurtleBuilder::Initialise()
{
	if(m_BuildingPrim) delete m_BuildingPrim;
	m_AttachedPoints=NULL;
	m_BuildingPrim=NULL;
	m_Position=0;
}

void TurtleBuilder::Prim(int Type)
{
	Initialise();
	m_BuildingPrim=new PolyPrimitive((PolyPrimitive::Type)Type);
}

void TurtleBuilder::Attach(PolyPrimitive *p)
{
	Initialise();
	TypedPData<dVector> *points = dynamic_cast<TypedPData<dVector>* >(p->GetDataRaw("p"));
	m_AttachedPoints = &points->m_Data;
}


void TurtleBuilder::Vert()
{
	if (m_BuildingPrim) 
	{
		m_BuildingPrim->AddVertex(dVertex(m_State.begin()->m_Pos,dVector(0,1,0)));
	}
	else if (m_AttachedPoints)
	{
		(*m_AttachedPoints)[m_Position%m_AttachedPoints->size()]=m_State.begin()->m_Pos;
	}
	
	m_Position++;
}

void TurtleBuilder::Skip(int n)
{
	m_Position+=n;
}

void TurtleBuilder::Shift(int n)
{
	// todo
}

int TurtleBuilder::Build(Renderer *renderer)
{
	if (m_BuildingPrim) 
	{
		int id = renderer->AddPrimitive(m_BuildingPrim);
		m_BuildingPrim=NULL;
		return id;
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

