// Copyright (C) 2005 Dave Griffiths
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

#include <ode/ode.h>
#include <Renderer.h>
#include <set>

namespace fluxus
{

class Physics
{
public:
   	Physics(Renderer *r);
	virtual ~Physics();
	
	enum BoundingType {BOX,CYLINDER,SPHERE};
	enum ObjectType {ACTIVE,PASSIVE};
    void Tick();
	void MakeActive(int ID,float MassBoundingType, BoundingType Bound=BOX);
	void MakePassive(int ID,float MassBoundingType, BoundingType Bound=BOX);
    void Free(int ID);
    void Clear();
    void RegisterRenderer(Renderer* s) { m_Renderer=s; }
    void GroundPlane(dVector ori, float off);
    void Kick(int ID, dVector v);
    void Twist(int ID, dVector v);
    void SetMass(int ID, float mass);
	void SetCollisions(bool s) { m_Collisions=s; }
	void SetGravity(const dVector &g);
	void SetGlobalSurfaceParams(float slip1, float slip2, float softerp, float softcfm) 
		{ m_Slip1=slip1; m_Slip2=slip2; m_SoftErp=softerp; m_SoftCfm=softcfm; }
		
    int CreateJointBall(int Ob1, int Ob2, dVector Anchor);
	int CreateJointHinge(int Ob1, int Ob2, dVector Anchor, dVector Hinge);
    int CreateJointSlider(int Ob1, int Ob2, dVector Hinge);
    int CreateJointHinge2(int Ob1, int Ob2, dVector Anchor, dVector Hinge[2]);
	int CreateJointAMotor(int Ob1, int Ob2, dVector Axis);

	void SetJointAngle(int ID, float force, float angle); 
    void SetJointParam(int ID, const string &Param, float Value);

    int GetMaxObjectCount() { return m_MaxObjectCount; }
    void SetMaxObjectCount(int s) { m_MaxObjectCount=s; }

	bool HasCollided(int Ob);
	
private:

	enum JointType {BallJoint,HingeJoint,SliderJoint,ContactJoint,UniversalJoint,Hinge2Joint,FixedJoint,AMotorJoint};

	class Object
	{
	public:
		Object();
		~Object();
		ObjectType Type;
		dBodyID Body;
		dGeomID Bound;
		Primitive *Prim;
	};
	
	class JointObject
	{
	public:
		JointObject();
		~JointObject();
		dJointID Joint;
		JointType Type;
	};
	
	void UpdatePrimitives();
	
	static void NearCallback(void *data, dGeomID o1, dGeomID o2);
	void NearCallback_i(dGeomID o1, dGeomID o2);
	
    dWorldID m_World;
	dSpaceID m_Space;
	dGeomID m_Ground;
	
	map<int,Object*>       m_ObjectMap;
	map<int,dGeomID>       m_GroupMap;
	map<int,JointObject*>  m_JointMap;
	deque<int>             m_History;
	set<dBodyID>		   m_CollisionRecord;
	
	Renderer *m_Renderer;
	int m_MaxObjectCount;
	bool m_GroundCreated;
	dJointGroupID m_ContactGroup;
	int m_NextJointID;
	bool m_Collisions;

	float m_Slip1;
	float m_Slip2;
	float m_SoftErp;
	float m_SoftCfm;
};

};
