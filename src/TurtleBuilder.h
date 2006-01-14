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
	
	void Prim(int Type=1);
	void Vert();
	int Build(Renderer *renderer);
	
	void Move(float d);
	void Turn(dVector a);
	void Colour(dColour c);
	void Reset();
	void Push();
	void Pop();
	
private:
	bool m_Built;
	PolyPrimitive* m_CurrentPrim;	
	
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
