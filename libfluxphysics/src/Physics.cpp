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
#include "Physics.h"
#include <fluxus/State.h>
#include <fluxus/Primitive.h>

using namespace fluxus;

Physics::Object::Object()
{
	Prim=NULL;
}

Physics::Object::~Object()
{
	if (Type==ACTIVE) dBodyDestroy(Body);
	dGeomDestroy(Bound);
	Prim->SetPhysicalHint(false);
}

Physics::JointObject::JointObject()
{
}

Physics::JointObject::~JointObject()
{
	dJointDestroy(Joint);
}
		
//////////////////////////////////////////////////////////////////////

Physics::Physics(Renderer *r) :
m_Renderer(r),
m_MaxObjectCount(1000),
m_GroundCreated(false),
m_NextJointID(1),
m_Collisions(false)
{
	m_World = dWorldCreate();
  	m_Space = dHashSpaceCreate(0);
  	m_ContactGroup = dJointGroupCreate(0);
  	dWorldSetGravity(m_World,0,-1,0);
}

Physics::~Physics()
{
}
	
void Physics::Tick()
{
	dSpaceCollide(m_Space,this,&NearCallback);
    dWorldStep(m_World,0.05);
	
    // remove all contact joints
    dJointGroupEmpty(m_ContactGroup);

    UpdatePrimitives();
}

void Physics::GroundPlane(dVector ori, float off)
{
	m_Ground = dCreatePlane(m_Space,ori.x,ori.y,ori.z,off);
	m_GroundCreated=true;
}

void Physics::MakeActive(int ID, float Mass, BoundingType Bound)
{	
	if (m_ObjectMap.find(ID)!=m_ObjectMap.end())
	{
		cerr<<"Physics::AddToGroup : Object ["<<ID<<"] already registered"<<endl;
		return;
	}
	
    Object *Ob = new Object;
    Ob->Type = ACTIVE;
	Ob->Prim = m_Renderer->GetPrimitive(ID);	
	
	if (!Ob->Prim) return;
	Ob->Prim->SetPhysicalHint(true);
	
	dMass m;
	Ob->Body = dBodyCreate(m_World);
	
	State *ObState=Ob->Prim->GetState();
	
	dVector Pos=ObState->Transform.gettranslate();
  	dBodySetPosition(Ob->Body,Pos.x,Pos.y,Pos.z);
	
	// need to apply transform to object here, so we are left with an identity in the
	// state transform for the object, and the bounding volume will be correct etc.
	// can't undo this.	
	Ob->Prim->ApplyTransform(true);  	
  	
  	switch (Bound)
  	{
  		case BOX:
  		{
			dBoundingBox Box=Ob->Prim->GetBoundingBox();
			dVector BoxSize=Box.max-Box.min;
			dMassSetBox(&m,1,BoxSize.x,BoxSize.y,BoxSize.z);
			dMassAdjust(&m,Mass);
 			dBodySetMass(Ob->Body,&m);
 			Ob->Bound = dCreateBox (m_Space,BoxSize.x,BoxSize.y,BoxSize.z);
 	    } break;
 	    case SPHERE:
 	    {
			dBoundingBox Box=Ob->Prim->GetBoundingBox();
			// Take the distance across the box in x divided by 2 to be the
			// radius. This works with a sphere well enough...
			float Radius=(Box.max.x-Box.min.x)/2;
			dMassSetSphere(&m,1,Radius);
			dMassAdjust(&m,Mass);
 			dBodySetMass(Ob->Body,&m);
 			Ob->Bound = dCreateSphere (m_Space,Radius);	
 	    } break;
 	    case CYLINDER:
 	    {
            dBoundingBox Box=Ob->Prim->GetBoundingBox();
			float Radius=(Box.max.x-Box.min.x)/2;
			float Height=Box.max.y-Box.min.y;
			dMassSetCappedCylinder(&m,1,2,Radius,Height);
			dMassAdjust(&m,Mass);
 			dBodySetMass(Ob->Body,&m);
 			Ob->Bound = dCreateCCylinder(m_Space,Radius,Height);	
        }
 	}
  	dGeomSetBody (Ob->Bound,Ob->Body);
	  	
  	m_ObjectMap[ID]=Ob;
  	m_History.push_back(ID);
  	
  	// remove oldest object if neccesary
  	if ((int)m_ObjectMap.size()>m_MaxObjectCount)
  	{
  		Free(*m_History.begin());
  		m_Renderer->RemovePrimitive(*m_History.begin());
  		m_History.pop_front();
  	}
}

void Physics::MakePassive(int ID, float Mass, BoundingType Bound)
{	
	if (m_ObjectMap.find(ID)!=m_ObjectMap.end())
	{
		cerr<<"Physics::AddToGroup : Object ["<<ID<<"] already registered"<<endl;
		return;
	}
	
    Object *Ob = new Object;
    Ob->Type = PASSIVE;
	Ob->Prim = m_Renderer->GetPrimitive(ID);	
	
	if (!Ob->Prim) return;
	Ob->Prim->SetPhysicalHint(true);
	
	State *ObState=Ob->Prim->GetState();
	
	dVector Pos=ObState->Transform.gettranslate();
  	
  	// need to apply transform to object here, so we are left with an identity in the
	// state transform for the object, and the bounding volume will be correct etc.
	// can't undo this.	
	Ob->Prim->ApplyTransform(false);  	
  	
  	switch (Bound)
  	{
  		case BOX:
  		{
			dBoundingBox Box=Ob->Prim->GetBoundingBox();
			dVector BoxSize=Box.max-Box.min;
 			Ob->Bound = dCreateBox(m_Space,BoxSize.x,BoxSize.y,BoxSize.z);
 	    } break;
 	    case SPHERE:
 	    {
			dBoundingBox Box=Ob->Prim->GetBoundingBox();
			// Take the distance across the box in x divided by 2 to be the
			// radius. This works with a sphere well enough...
			float Radius=(Box.max.x-Box.min.x)/2;
 			Ob->Bound = dCreateSphere(m_Space,Radius);	
 	    } break;
 	    case CYLINDER:
 	    {
            dBoundingBox Box=Ob->Prim->GetBoundingBox();
			float Radius=(Box.max.x-Box.min.x)/2;
			float Height=Box.max.y-Box.min.y;
 			Ob->Bound = dCreateCCylinder(m_Space,Radius,Height);	
        }
 	}
  	
  	dGeomSetPosition(Ob->Bound,Pos.x,Pos.y,Pos.z);
  	
  	m_ObjectMap[ID]=Ob;
}

void Physics::Free(int ID)
{
	map<int,Object*>::iterator i = m_ObjectMap.find(ID);
	if (i==m_ObjectMap.end())
	{
		cerr<<"Physics::Free : Object ["<<ID<<"] doesn't exist"<<endl;
		return;
	}
	delete i->second;	
	m_ObjectMap.erase(i);
}

void Physics::Clear()
{
	for(map<int,Object*>::iterator i=m_ObjectMap.begin(); i!=m_ObjectMap.end(); ++i)
	{
		delete i->second;	
	}
	
	m_ObjectMap.clear();
	if (m_GroundCreated) 
	{
		dGeomDestroy(m_Ground);
		m_GroundCreated=false;
	}
}

void Physics::UpdatePrimitives()
{
	// for every object
	for(map<int,Object*>::iterator i=m_ObjectMap.begin(); i!=m_ObjectMap.end(); ++i)
	{
		if (i->second->Type==ACTIVE)
		{
			float Pos[3]={dBodyGetPosition(i->second->Body)[0],
			              dBodyGetPosition(i->second->Body)[1],
						  dBodyGetPosition(i->second->Body)[2]};
					
			dMatrix Rot(dBodyGetRotation(i->second->Body)[0], dBodyGetRotation(i->second->Body)[1], dBodyGetRotation(i->second->Body)[2],
			  			   dBodyGetRotation(i->second->Body)[3], dBodyGetRotation(i->second->Body)[4], dBodyGetRotation(i->second->Body)[5],
			               dBodyGetRotation(i->second->Body)[6], dBodyGetRotation(i->second->Body)[7], dBodyGetRotation(i->second->Body)[8],
			       		   dBodyGetRotation(i->second->Body)[9], dBodyGetRotation(i->second->Body)[10], dBodyGetRotation(i->second->Body)[11], 0,0,0,1);
       		   		
			dVector PosVec(Pos[0],Pos[1],Pos[2]);	
				
			i->second->Prim->GetState()->Transform=Rot;
			i->second->Prim->GetState()->Transform.settranslate(PosVec);
		}
	}
}

void Physics::Kick(int ID, dVector v)
{
    map<int,Object*>::iterator i = m_ObjectMap.find(ID);
	if (i==m_ObjectMap.end())
	{
		cerr<<"Physics::Kick : Object ["<<ID<<"] doesn't exist"<<endl;
		return;
	}
	const dReal *cv = dBodyGetLinearVel(i->second->Body);
	dBodySetLinearVel(i->second->Body,cv[0]+v.x,cv[1]+v.y,cv[2]+v.z);
}

void Physics::Twist(int ID, dVector v)
{
    map<int,Object*>::iterator i = m_ObjectMap.find(ID);
	if (i==m_ObjectMap.end())
	{
		cerr<<"Physics::Twist : Object ["<<ID<<"] doesn't exist"<<endl;
		return;
	}
	const dReal *cv = dBodyGetAngularVel(i->second->Body);
	dBodySetAngularVel(i->second->Body,cv[0]+v.x,cv[1]+v.y,cv[2]+v.z);
}

void Physics::NearCallback(void *data, dGeomID o1, dGeomID o2)
{
	((Physics*)data)->NearCallback_i(o1,o2);
}

void Physics::NearCallback_i(dGeomID o1, dGeomID o2)
{
	if (m_Collisions)
	{
  		const int N = 10;
		dContact contact[N];

		int n = dCollide (o1,o2,N,&contact[0].geom,sizeof(dContact));
		if (n > 0)
		{
			for (int i=0; i<n; i++)
			{
			  contact[i].surface.mode = dContactSlip1 | dContactSlip2 | dContactSoftERP | dContactSoftCFM | dContactApprox1;
			  contact[i].surface.mu = dInfinity;
			  contact[i].surface.slip1 = 0.9;
			  contact[i].surface.slip2 = 0.9;
			  contact[i].surface.soft_erp = 0.25;
			  contact[i].surface.soft_cfm = 0.15;
			  dJointID c = dJointCreateContact(m_World,m_ContactGroup,&contact[i]);
			  dJointAttach(c,dGeomGetBody(contact[i].geom.g1),dGeomGetBody(contact[i].geom.g2));
			}
		}
	}
}

int Physics::CreateJointHinge2(int Ob1, int Ob2, dVector Anchor, dVector Hinge[2])
{
	map<int,Object*>::iterator i1 = m_ObjectMap.find(Ob1);
	map<int,Object*>::iterator i2 = m_ObjectMap.find(Ob2);
	
	if (i1==m_ObjectMap.end())
	{
		cerr<<"Physics::CreateJoint2 : Object ["<<Ob1<<"] doesn't exist"<<endl;
		return 0;
	}
	
	if (i2==m_ObjectMap.end())
	{
		cerr<<"Physics::CreateJoint2 : Object ["<<Ob2<<"] doesn't exist"<<endl;
		return 0;
	}
	
	dJointID j = dJointCreateHinge2 (m_World,0);
	dJointAttach (j,i1->second->Body,i2->second->Body);
	dJointSetHinge2Anchor (j,Anchor.x,Anchor.y,Anchor.z);
	dJointSetHinge2Axis1 (j,Hinge[0].x, Hinge[0].y, Hinge[0].z);
	dJointSetHinge2Axis2 (j,Hinge[1].x, Hinge[1].y, Hinge[1].z);
	
	JointObject NewJoint;
	NewJoint.Joint=j;
	NewJoint.Type=Hinge2;
	m_JointMap[m_NextJointID]=NewJoint;
	m_NextJointID++;
	return m_NextJointID-1;
}

int Physics::CreateJointBall(int Ob1, int Ob2, dVector Anchor)
{
	map<int,Object*>::iterator i1 = m_ObjectMap.find(Ob1);
	map<int,Object*>::iterator i2 = m_ObjectMap.find(Ob2);
	
	if (i1==m_ObjectMap.end())
	{
		cerr<<"Physics::CreateJoint2 : Object ["<<Ob1<<"] doesn't exist"<<endl;
		return 0;
	}
	
	if (i2==m_ObjectMap.end())
	{
		cerr<<"Physics::CreateJoint2 : Object ["<<Ob2<<"] doesn't exist"<<endl;
		return 0;
	}
	
	dJointID j = dJointCreateHinge2 (m_World,0);
	dJointAttach (j,i1->second->Body,i2->second->Body);
	dJointSetBallAnchor (j,Anchor.x,Anchor.y,Anchor.z);
	
	JointObject NewJoint;
	NewJoint.Joint=j;
	NewJoint.Type=Ball;
	m_JointMap[m_NextJointID]=NewJoint;
	m_NextJointID++;
	return m_NextJointID-1;
}

void Physics::SetJointParam(int ID, JointParamType Param, float Value)
{
	map<int,JointObject>::iterator i = m_JointMap.find(ID);
	if (i==m_JointMap.end())
	{
		cerr<<"Physics::SetJointParam : Joint ["<<ID<<"] doesn't exist"<<endl;
		return;
	}
	
	int p=-1;

	switch (Param)
	{
	   	case LoStop        : p=dParamLoStop; break;
	   	case HiStop        : p=dParamHiStop; break;
	   	case Vel           : p=dParamVel; break;
	   	case FMax          : p=dParamFMax; break;
	   	case FudgeFactor   : p=dParamFudgeFactor; break;
	   	case Bounce        : p=dParamBounce; break;
	   	case CFM           : p=dParamCFM; break;
 	   	case StopERP       : p=dParamStopERP; break;
	   	case StopCFM       : p=dParamStopCFM; break;
 	   	case SuspensionERP : p=dParamSuspensionERP; break;
	   	case SuspensionCFM : p=dParamSuspensionCFM; break;
	   	case Vel2          : p=dParamVel2; break;
	   	case FMax2         : p=dParamFMax2; break;
	   	default : cerr<<"unknown parameter "<<Param<<endl; return; break;
	}

	switch (i->second.Type)
	{
		case Ball      : break; 	
		case Hinge     : dJointSetHingeParam(i->second.Joint,p,Value); break;	
		case Contact   : break;	
		case Universal : break;	
		case Hinge2    : dJointSetHinge2Param(i->second.Joint,p,Value); break;	
		case Fixed     : break;	
		case AMotor    : dJointSetAMotorParam(i->second.Joint,p,Value); break;
		default : cerr<<"unknown joint type "<<i->second.Type<<endl; return; break;	
	} 	
}

