#include "fluxus/PolyPrimitive.h"

#ifndef N_TURTLE_BUILDER
#define N_TURTLE_BUILDER

namespace fluxus
{

class TurtleBuilder
{
public:
	TurtleBuilder() : m_Built(false),m_CurrentPrim(NULL) {}
	~TurtleBuilder() {}
	
	void Prim(int Type=1);
	void Vert();
	int Build(Renderer *renderer);
	
	void Move(float d);
	void Turn(dVector a);
	void Colour(dColour c);
	void Reset() { m_Pos=dVector(0,0,0); m_Rot=dVector(0,0,0); }
	
private:
	bool m_Built;
	PolyPrimitive* m_CurrentPrim;
	dVector m_Pos;
	dVector m_Rot;
	dColour m_Colour;
};

}

#endif
