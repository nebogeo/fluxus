#include <deque>
#include "PolyPrimitive.h"

#ifndef N_TURTLE_BUILDER
#define N_TURTLE_BUILDER

namespace fluxus
{

class TurtleBuilder
{
public:
	TurtleBuilder();
	~TurtleBuilder() {}
	
	void Initialise();
	
	void Prim(int Type=1);
	int Build(Renderer *renderer);
	
	void Attach(PolyPrimitive *p);
	
	void Vert();
	void Move(float d);
	void Turn(dVector a);
	void Reset();
	void Push();
	void Pop();
	int Position() { return m_Position; }
	void SetPosition(unsigned int s) { m_Position=s; }
	
	// for attached mode only
	void Skip(int n);
	void Shift(int n);
	
	
private:
	
	PolyPrimitive* m_BuildingPrim;	
	vector<dVector> *m_AttachedPoints;
	unsigned int m_Position;
	
	struct State
	{
		dVector m_Pos;
		dVector m_Rot;
		dColour m_Colour;
	};
	
	deque<State> m_State;
};

}

#endif
