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
#include "State.h"
#include "Primitive.h"

using namespace Fluxus;

Physics::Object::Object()
{
	Prim=NULL;
}

Physics::Object::~Object()
{
	if (Type==ACTIVE) dBodyDestroy(Body);
	dGeomDestroy(Bound);
}

Physics::JointObject::JointObject()
{
}

Physics::JointObject::~JointObject()
{
	dJointDestroy(Joint);
}

//////////////////////////////////////////////////////////////////////

bool Physics::m_ODEInited = false;

Physics::Physics(Renderer *r) :
m_Renderer(r),
m_MaxObjectCount(1000),
m_GroundCreated(false),
m_NextJointID(1),
m_Collisions(false),
m_Slip1(0.9),
m_Slip2(0.9),
m_SoftErp(0.25),
m_SoftCfm(0.15)
{
	if (!m_ODEInited)	// init ODE only once
	{
#ifdef GOODE_OLDE_ODE
		dInitODE();
#else
		dInitODE2(0);
#endif
		m_ODEInited = true;
	}

	m_World = dWorldCreate();
	m_Space = dHashSpaceCreate(0);
	m_ContactGroup = dJointGroupCreate(0);
	dWorldSetGravity(m_World,0,-5,0);
}

Physics::~Physics()
{
	dCloseODE();
}

void Physics::Tick()
{
	m_CollisionRecord.clear();

	dSpaceCollide(m_Space,this,&NearCallback);
    dWorldQuickStep(m_World,0.05);

    // remove all contact joints
    dJointGroupEmpty(m_ContactGroup);

    UpdatePrimitives();
}

void Physics::DrawLocator(dVector3 pos)
{
	float scale=0.5;
	glBegin(GL_LINES);
	glVertex3f(pos[0]-scale,pos[1],pos[2]);
	glVertex3f(pos[0]+scale,pos[1],pos[2]);
	glVertex3f(pos[0],pos[1]-scale,pos[2]);
	glVertex3f(pos[0],pos[1]+scale,pos[2]);
	glVertex3f(pos[0],pos[1],pos[2]-scale);
	glVertex3f(pos[0],pos[1],pos[2]+scale);
	glEnd();
}

void Physics::DrawAxis(dVector3 pos, dVector3 dir)
{
	glBegin(GL_LINES);
	glVertex3f(pos[0],pos[1],pos[2]);
	glVertex3f(pos[0]+dir[0],pos[1]+dir[1],pos[2]+dir[2]);
	glEnd();
}

void Physics::Render()
{
	glDisable(GL_LIGHTING);
	glDisable(GL_DEPTH_TEST);

	for (map<int,JointObject*>::iterator i=m_JointMap.begin(); i!=m_JointMap.end(); i++)
	{
		switch (i->second->Type)
		{
			case BallJoint      : 
			{ 
				dVector3 pos;
				dJointGetBallAnchor(i->second->Joint, pos);
				glColor3f(1,0,0);
				DrawLocator(pos);
			}
			break;
			case HingeJoint     : 
			{
				dVector3 pos;
				dJointGetHingeAnchor(i->second->Joint, pos);
				glColor3f(1,0,0);
				DrawLocator(pos);
				dJointGetHingeAnchor2(i->second->Joint, pos);
				DrawLocator(pos);
				dVector3 axis;
				dJointGetHingeAxis(i->second->Joint, axis);
				glColor3f(0,1,0);
				DrawAxis(pos,axis);
			}
			break;	
			case SliderJoint :   break;
			case ContactJoint   : break;	// no set param required
			case UniversalJoint : break;	
			case Hinge2Joint   : 
			{
				dVector3 pos;
				dJointGetHinge2Anchor(i->second->Joint, pos);
				glColor3f(1,0,0);
				DrawLocator(pos);
				dJointGetHinge2Anchor2(i->second->Joint, pos);
				DrawLocator(pos);
				dVector3 axis;
				dJointGetHinge2Axis1(i->second->Joint, axis);
				glColor3f(0,1,0);
				DrawAxis(pos,axis);
				dJointGetHinge2Axis2(i->second->Joint, axis);
				DrawAxis(pos,axis);
			}
			break;
			case FixedJoint     : break;	// no set param required
			case AMotorJoint    : break;
		} 	
	}	
	glEnable(GL_LIGHTING);
	glEnable(GL_DEPTH_TEST);
}

void Physics::SetGravity(const dVector &g)
{
	dWorldSetGravity(m_World,g.x,g.y,g.z);
}

void Physics::GroundPlane(dVector ori, float off)
{
	m_Ground = dCreatePlane(m_Space,ori.x,ori.y,ori.z,off);
	m_GroundCreated=true;
}

void Physics::SetupTransform(Primitive *p, dMatrix &rotation, dVector &Pos)
{
	State *ObState=p->GetState();

    // find the bounding box centre
	dMatrix temp;
    dBoundingBox old_box = p->GetBoundingBox(temp);

    // ODE position is that of the centre of mass
    Pos = ObState->Transform.transform( (old_box.min + (old_box.max - old_box.min)/2) );
    // center the object on it's bounding box centre
    ObState->Transform.settranslate(ObState->Transform.gettranslate() - Pos);
	
	// extract the rotation from the state
	rotation = ObState->Transform;
	rotation.remove_scale();
	rotation.settranslate(dVector(0,0,0));
	
    // inverse won't work if there are zero scales involved
    // so we need to check for that
    dVector scalecheck=rotation.get_scale();
    if (scalecheck.feq(dVector(1,1,1)))
    {
        // remove the rotation 
        ObState->Transform*=rotation.inverse();
    }
		
	// need to apply transform to object here, so we are left with an identity in the
	// state transform for the object, and the bounding volume will be correct.
	// can't undo this.	
	p->ApplyTransform(false);
	
    // make sure object's transform is in sync with physics right away, so we can use it in scripts directly
    ObState->Transform=rotation;
    ObState->Transform.settranslate(Pos);
}


void Physics::MakeActive(int ID, float Mass, BoundingType Bound)
{	
	if (m_ObjectMap.find(ID)!=m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::AddToGroup : Object ["<<ID<<"] already registered"<<endl;
		return;
	}
	
    Object *Ob = new Object;
    Ob->Type = ACTIVE;
	Ob->Prim = m_Renderer->GetPrimitive(ID);	
	
	if (!Ob->Prim) return;
	
	dMass m;
	Ob->Body = dBodyCreate(m_World);
	
    dMatrix rotation;
    dVector Pos;
    SetupTransform(Ob->Prim,rotation,Pos);
    dMatrix ident;
	  	
	// get the bounding box from the fluxus object
  	switch (Bound)
  	{
  		case BOX:
  		{
			dBoundingBox Box=Ob->Prim->GetBoundingBox(ident);
            Box.fudgenonzerovolume();
 			dVector BoxSize=Box.max-Box.min;
 			dMassSetBox(&m,1,BoxSize.x,BoxSize.y,BoxSize.z);
			dMassAdjust(&m,Mass);
 			dBodySetMass(Ob->Body,&m);
 			Ob->Bound = dCreateBox (m_Space,BoxSize.x,BoxSize.y,BoxSize.z);
 	    } break;
 	    case SPHERE:
 	    {
			dBoundingBox Box=Ob->Prim->GetBoundingBox(ident);
            Box.fudgenonzerovolume();
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
            dBoundingBox Box=Ob->Prim->GetBoundingBox(ident);
            Box.fudgenonzerovolume();
			float Radius=(Box.max.x-Box.min.x)/2;
			float Height=Box.max.y-Box.min.y;
			dMassSetCylinder(&m,1,2,Radius,Height);
			dMassAdjust(&m,Mass);
 			dBodySetMass(Ob->Body,&m);
 			Ob->Bound = dCreateCylinder(m_Space,Radius,Height);	
        } break;
		case MESH:
		{
			
			PolyPrimitive *pp = dynamic_cast<PolyPrimitive *>(Ob->Prim);
			if (pp!=NULL)
			{
				dTriMeshDataID TriMeshData=dGeomTriMeshDataCreate();
				if (pp->GetType()==PolyPrimitive::TRILIST && pp->IsIndexed())
				{                    
					float *verts = (float*)(&((*pp->GetDataVec<dVector>("p"))[0]));
					float *normals = (float*)(&((*pp->GetDataVec<dVector>("n"))[0]));
					unsigned int *idx = (unsigned int *)(&(pp->GetIndex()[0]));
				
					dGeomTriMeshDataBuildSingle1(TriMeshData, (void*)verts,
                    	   sizeof(dVector), pp->Size(),
                    	   (void*)idx, pp->GetIndex().size(),
                    	   sizeof(unsigned int)*3, (void*)normals);

					dBoundingBox Box=Ob->Prim->GetBoundingBox(ident);
                    Box.fudgenonzerovolume();
					dVector BoxSize=Box.max-Box.min;
                    
					dMassSetBox(&m,1,BoxSize.x,BoxSize.y,BoxSize.z);
					dMassAdjust(&m,Mass);
 					dBodySetMass(Ob->Body,&m);
					Ob->Bound = dCreateTriMesh(m_Space,TriMeshData,NULL,NULL,NULL);
				}
				else
				{
					Trace::Stream<<"Physics::MakeActive : PolyPrimitive ["<<ID<<"] needs to be an indexed triangle list"<<endl;
                    return;
				}
			}
			else
			{
				Trace::Stream<<"Physics::MakeActive : Object ["<<ID<<"] is not a polyprimitive, and mesh specified"<<endl;
                return;
			}

		} break;
 	}
	
	// set rotation into ode body
	dMatrix4 rot;
	rot[0]=rotation.m[0][0];
	rot[1]=rotation.m[1][0];
	rot[2]=rotation.m[2][0];
	rot[3]=rotation.m[3][0];
	
	rot[4]=rotation.m[0][1];
	rot[5]=rotation.m[1][1];
	rot[6]=rotation.m[2][1];
	rot[7]=rotation.m[3][1];
	
	rot[8]=rotation.m[0][2];
	rot[9]=rotation.m[1][2];
	rot[10]=rotation.m[2][2];
	rot[11]=rotation.m[3][2];

	rot[12]=rotation.m[0][3];
	rot[13]=rotation.m[1][3];
	rot[14]=rotation.m[2][3];
	rot[15]=rotation.m[3][3];
	
  	dBodySetRotation(Ob->Body,rot);
	
	// set position into ode body
  	dBodySetPosition(Ob->Body,Pos.x,Pos.y,Pos.z);

 	dGeomSetBody (Ob->Bound,Ob->Body);
	
	dBodySetAutoDisableFlag(Ob->Body, 1);

  	m_ObjectMap[ID]=Ob;
  	m_History.push_back(ID);
  	
  	// remove oldest object if neccesary
  	if ((int)m_ObjectMap.size()>m_MaxObjectCount)
  	{
        int ID=*m_History.begin();
        Free(ID);
        m_Renderer->RemovePrimitive(ID);
        m_History.pop_front();
  	}
}

void Physics::MakePassive(int ID, float Mass, BoundingType Bound)
{	
	if (m_ObjectMap.find(ID)!=m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::AddToGroup : Object ["<<ID<<"] already registered"<<endl;
		return;
	}
	
    Object *Ob = new Object;
    Ob->Type = PASSIVE;
	Ob->Prim = m_Renderer->GetPrimitive(ID);	
	
	if (!Ob->Prim) return;

    dMatrix rotation;
    dVector Pos;
    SetupTransform(Ob->Prim,rotation,Pos);
    dMatrix ident;
	
	// this tells ode to attach joints to the static environment if they are attached
	// to this joint
	Ob->Body = 0;

  	switch (Bound)
  	{
  		case BOX:
  		{
			dBoundingBox Box=Ob->Prim->GetBoundingBox(ident);
            Box.fudgenonzerovolume();
			dVector BoxSize=Box.max-Box.min;
 			Ob->Bound = dCreateBox(m_Space,BoxSize.x,BoxSize.y,BoxSize.z);
 	    } break;
 	    case SPHERE:
 	    {
			dBoundingBox Box=Ob->Prim->GetBoundingBox(ident);
            Box.fudgenonzerovolume();
			// Take the distance across the box in x divided by 2 to be the
			// radius. This works with a sphere well enough...
			float Radius=(Box.max.x-Box.min.x)/2;
 			Ob->Bound = dCreateSphere(m_Space,Radius);	
 	    } break;
 	    case CYLINDER:
 	    {
            dBoundingBox Box=Ob->Prim->GetBoundingBox(ident);
            Box.fudgenonzerovolume();
			float Radius=(Box.max.x-Box.min.x)/2;
			float Height=Box.max.y-Box.min.y;
 			Ob->Bound = dCreateCylinder(m_Space,Radius,Height);	
        } break;
		case MESH:
		{
			PolyPrimitive *pp = dynamic_cast<PolyPrimitive *>(Ob->Prim);
			if (pp!=NULL)
			{
				dTriMeshDataID TriMeshData=dGeomTriMeshDataCreate();
				if (pp->GetType()==PolyPrimitive::TRILIST && pp->IsIndexed())
				{
					float *verts = (float*)(&((*pp->GetDataVec<dVector>("p"))[0]));
					float *normals = (float*)(&((*pp->GetDataVec<dVector>("n"))[0]));
					unsigned int *idx = (unsigned int *)(&(pp->GetIndex()[0]));
				
					dGeomTriMeshDataBuildSingle1(TriMeshData, (void*)verts,
                    	   sizeof(dVector), pp->Size(),
                    	   (void*)idx, pp->GetIndex().size(),
                    	   sizeof(unsigned int)*3, (void*)normals);

					Ob->Bound = dCreateTriMesh(m_Space,TriMeshData,NULL,NULL,NULL);
				}
				else
				{
					Trace::Stream<<"Physics::MakePassive : PolyPrimitive ["<<ID<<"] needs to be an indexed triangle list"<<endl;
					return;
				}
			}
			else
			{
				Trace::Stream<<"Physics::MakePassive : Object ["<<ID<<"] is not a polyprimitive, and mesh specified"<<endl;
				return;
			}

		} break;
 	}
	
  	// set rotation into ode body
	dMatrix4 rot;
	rot[0]=rotation.m[0][0];
	rot[1]=rotation.m[1][0];
	rot[2]=rotation.m[2][0];
	rot[3]=rotation.m[3][0];
	
	rot[4]=rotation.m[0][1];
	rot[5]=rotation.m[1][1];
	rot[6]=rotation.m[2][1];
	rot[7]=rotation.m[3][1];
	
	rot[8]=rotation.m[0][2];
	rot[9]=rotation.m[1][2];
	rot[10]=rotation.m[2][2];
	rot[11]=rotation.m[3][2];

	rot[12]=rotation.m[0][3];
	rot[13]=rotation.m[1][3];
	rot[14]=rotation.m[2][3];
	rot[15]=rotation.m[3][3];
	
  	dGeomSetPosition(Ob->Bound,Pos.x,Pos.y,Pos.z);
  	dGeomSetRotation(Ob->Bound,rot);

  	
  	m_ObjectMap[ID]=Ob;
}

void Physics::SetMass(int ID, float mass)
{
	map<int,Object*>::iterator i = m_ObjectMap.find(ID);
	if (i==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::SetMass : Object ["<<ID<<"] doesn't exist"<<endl;
		return;
	}
	
	if (i->second->Type!=ACTIVE)
	{
		Trace::Stream<<"Physics::SetMass : Object ["<<ID<<"] isn't active"<<endl;
		return;
	}
	
	dMass m;
	dBodyGetMass(i->second->Body,&m);
	dMassAdjust(&m,mass);
	dBodySetMass(i->second->Body,&m);
}

void Physics::Free(int ID)
{
	map<int,Object*>::iterator i = m_ObjectMap.find(ID);
	if (i!=m_ObjectMap.end())
	{
        // clean up joints connected to this object
        vector<map<int,JointObject*>::iterator> toremove;
        for(map<int,JointObject*>::iterator j=m_JointMap.begin(); j!=m_JointMap.end(); ++j)
        {
            if (j->second->Ob1==ID || j->second->Ob2==ID)
            {
                toremove.push_back(j);
                delete j->second;
            }
        }

        for (vector<map<int,JointObject*>::iterator>::iterator j=toremove.begin(); j!=toremove.end(); ++j)
        {
            m_JointMap.erase(*j);
        }
        
        delete i->second;
        m_ObjectMap.erase(i);
    }

    Node *node = m_Renderer->GetSceneGraph().FindNode(ID);
    if (node!=NULL) // not certain this check is required...
    {
        // remove all the children of this object in the scenegraph
        for (vector<Node*>::const_iterator c=node->Children.begin(); c!=node->Children.end(); ++c)
        {
            Free((*c)->ID);
        }
    }
}

void Physics::Clear()
{
	for(map<int,Object*>::iterator i=m_ObjectMap.begin(); i!=m_ObjectMap.end(); ++i)
	{
		delete i->second;
	}
	m_ObjectMap.clear();

	for(map<int,JointObject*>::iterator i=m_JointMap.begin(); i!=m_JointMap.end(); ++i)
	{
		delete i->second;
	}
	m_JointMap.clear();

	m_History.clear();
	if (m_GroundCreated)
	{
		dGeomDestroy(m_Ground);
		m_GroundCreated=false;
	}

	m_NextJointID=0;
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
		Trace::Stream<<"Physics::Kick : Object ["<<ID<<"] doesn't exist"<<endl;
		return;
	}

	if (i->second->Type==ACTIVE)
	{
		const dReal *cv = dBodyGetLinearVel(i->second->Body);
		dBodySetLinearVel(i->second->Body,cv[0]+v.x,cv[1]+v.y,cv[2]+v.z);
	}
}

void Physics::Twist(int ID, dVector v)
{
    map<int,Object*>::iterator i = m_ObjectMap.find(ID);
	if (i==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::Twist : Object ["<<ID<<"] doesn't exist"<<endl;
		return;
	}

	if (i->second->Type==ACTIVE)
	{
		const dReal *cv = dBodyGetAngularVel(i->second->Body);
		dBodySetAngularVel(i->second->Body,cv[0]+v.x,cv[1]+v.y,cv[2]+v.z);
	}
}

void Physics::AddForce(int ID, dVector v)
{
    map<int,Object*>::iterator i = m_ObjectMap.find(ID);
	if (i==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::AddForce : Object ["<<ID<<"] doesn't exist"<<endl;
		return;
	}

	if (i->second->Type==ACTIVE)
	{
		dBodyAddForce(i->second->Body,v.x, v.y, v.z);
	}
}

void Physics::AddTorque(int ID, dVector v)
{
    map<int,Object*>::iterator i = m_ObjectMap.find(ID);
	if (i==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::AddTorque : Object ["<<ID<<"] doesn't exist"<<endl;
		return;
	}

	if (i->second->Type==ACTIVE)
	{
		dBodyAddTorque(i->second->Body,v.x, v.y, v.z);
	}
}

void Physics::SetGravityMode(int ID, bool mode)
{
    map<int,Object*>::iterator i = m_ObjectMap.find(ID);
	if (i==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::SetGravityMode : Object ["<<ID<<"] doesn't exist"<<endl;
		return;
	}

	if (i->second->Type==ACTIVE)
	{
		dBodySetGravityMode(i->second->Body, mode ? 1 : 0);
	}
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

		int n = dCollide(o1,o2,N,&contact[0].geom,sizeof(dContact));
		if (n > 0)
		{
			for (int i=0; i<n; i++)
			{
				contact[i].surface.mode = dContactSlip1 | dContactSlip2 | dContactSoftERP | dContactSoftCFM | dContactApprox1;
				contact[i].surface.mu = dInfinity;
				contact[i].surface.slip1 = m_Slip1;
				contact[i].surface.slip2 = m_Slip2;
				contact[i].surface.soft_erp = m_SoftErp;
				contact[i].surface.soft_cfm = m_SoftCfm;
				dJointID c = dJointCreateContact(m_World,m_ContactGroup,&contact[i]);
				dBodyID geom1 = dGeomGetBody(contact[i].geom.g1);
				dBodyID geom2 = dGeomGetBody(contact[i].geom.g2);
				dJointAttach(c,geom1,geom2);
				m_CollisionRecord.insert(geom1);
				m_CollisionRecord.insert(geom2);
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
		Trace::Stream<<"Physics::CreateJointHinge2 : Object ["<<Ob1<<"] doesn't exist"<<endl;
		return 0;
	}
	
	if (i2==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::CreateJointHinge2 : Object ["<<Ob2<<"] doesn't exist"<<endl;
		return 0;
	}
	
	if (i1->second->Body==0 || i2->second->Body==0)
	{
		Trace::Stream<<"Physics::CreateJointHinge2 : cant connect passive objects"<<endl;
		return 0;
	}

	dJointID j = dJointCreateHinge2(m_World,0);
	dJointAttach(j,i1->second->Body,i2->second->Body);
	dJointSetHinge2Anchor(j,Anchor.x,Anchor.y,Anchor.z);
	dJointSetHinge2Axis1(j,Hinge[0].x, Hinge[0].y, Hinge[0].z);
	dJointSetHinge2Axis2(j,Hinge[1].x, Hinge[1].y, Hinge[1].z);
	dJointSetHinge2Param(j,dParamFMax,100); // unlock the joint by default
	dJointSetHinge2Param(j,dParamFMax2,100); // unlock the joint by default
	
	JointObject *NewJoint = new JointObject;
	NewJoint->Joint=j;
	NewJoint->Type=Hinge2Joint;
	NewJoint->Ob1=Ob1;
	NewJoint->Ob2=Ob2;
	m_JointMap[m_NextJointID]=NewJoint;
	m_NextJointID++;
	return m_NextJointID-1;
}

int Physics::CreateJointHinge(int Ob1, int Ob2, dVector Anchor, dVector Hinge)
{
	map<int,Object*>::iterator i1 = m_ObjectMap.find(Ob1);
	map<int,Object*>::iterator i2 = m_ObjectMap.find(Ob2);
	
	if (i1==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::CreateJointHinge : Object ["<<Ob1<<"] doesn't exist"<<endl;
		return 0;
	}
	
	if (i2==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::CreateJointHinge : Object ["<<Ob2<<"] doesn't exist"<<endl;
		return 0;
	}
	
	if (i1->second->Body==0 || i2->second->Body==0)
	{
		Trace::Stream<<"Physics::CreateJointHinge : cant connect passive objects"<<endl;
		return 0;
	}
	
	dJointID j = dJointCreateHinge(m_World,0);
	dJointAttach (j,i1->second->Body,i2->second->Body);
	dJointSetHingeAnchor(j,Anchor.x,Anchor.y,Anchor.z);
	dJointSetHingeAxis(j,Hinge.x, Hinge.y, Hinge.z);
	dJointSetHingeParam(j,dParamFMax,100); // unlock the joint by default
	
	JointObject *NewJoint = new JointObject;
	NewJoint->Joint=j;
	NewJoint->Type=HingeJoint;
	NewJoint->Ob1=Ob1;
	NewJoint->Ob2=Ob2;
	m_JointMap[m_NextJointID]=NewJoint;
	m_NextJointID++;
	return m_NextJointID-1;
}

int Physics::CreateJointFixed(int Ob)
{
	map<int,Object*>::iterator i = m_ObjectMap.find(Ob);
	
	if (i==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::CreateJointFixed : Object ["<<Ob<<"] doesn't exist"<<endl;
		return 0;
	}
	
	if (i->second->Body==0)
	{
		Trace::Stream<<"Physics::CreateJointFixed : can't connect passive objects"<<endl;
		return 0;
	}

	dJointID j = dJointCreateFixed(m_World,0);
	dJointAttach (j,0,i->second->Body);
	dJointSetFixed(j);
	
	JointObject *NewJoint = new JointObject;
	NewJoint->Joint=j;
	NewJoint->Type=FixedJoint;
	NewJoint->Ob1=Ob;
	NewJoint->Ob2=0;
	m_JointMap[m_NextJointID]=NewJoint;
	m_NextJointID++;
	return m_NextJointID-1;
}

int Physics::CreateJointSlider(int Ob1, int Ob2, dVector Hinge)
{
	map<int,Object*>::iterator i1 = m_ObjectMap.find(Ob1);
	map<int,Object*>::iterator i2 = m_ObjectMap.find(Ob2);
	
	if (i1==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::CreateJointSlider : Object ["<<Ob1<<"] doesn't exist"<<endl;
		return 0;
	}
	
	if (i2==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::CreateJointSlider : Object ["<<Ob2<<"] doesn't exist"<<endl;
		return 0;
	}

	if (i1->second->Body==0 || i2->second->Body==0)
	{
		Trace::Stream<<"Physics::CreateJointSlider : cant connect passive objects"<<endl;
		return 0;
	}
	
	dJointID j = dJointCreateSlider(m_World,0);
	dJointAttach (j,i1->second->Body,i2->second->Body);
	dJointSetSliderAxis(j,Hinge.x, Hinge.y, Hinge.z);
	
	JointObject *NewJoint = new JointObject;
	NewJoint->Joint=j;
	NewJoint->Type=SliderJoint;
	NewJoint->Ob1=Ob1;
	NewJoint->Ob2=Ob2;
	m_JointMap[m_NextJointID]=NewJoint;
	m_NextJointID++;
	return m_NextJointID-1;
}

int Physics::CreateJointAMotor(int Ob1, int Ob2, dVector Axis)
{
	map<int,Object*>::iterator i1 = m_ObjectMap.find(Ob1);
	map<int,Object*>::iterator i2 = m_ObjectMap.find(Ob2);
	
	if (i1==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::CreateJointAMotor : Object ["<<Ob1<<"] doesn't exist"<<endl;
		return 0;
	}
	
	if (i2==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::CreateJointAMotor : Object ["<<Ob2<<"] doesn't exist"<<endl;
		return 0;
	}

	if (i1->second->Body==0 || i2->second->Body==0)
	{
		Trace::Stream<<"Physics::CreateJointAMotor : cant connect passive objects"<<endl;
		return 0;
	}

	dJointID j = dJointCreateAMotor(m_World,0);
	dJointAttach(j,i1->second->Body,i2->second->Body);

	dJointSetAMotorMode(j,dAMotorUser);
	dJointSetAMotorNumAxes(j,1);
	dJointSetAMotorAxis(j, 0, 1, Axis.x, Axis.y, Axis.z);
			  
	JointObject *NewJoint = new JointObject;
	NewJoint->Joint=j;
	NewJoint->Type=AMotorJoint;
	NewJoint->Ob1=Ob1;
	NewJoint->Ob2=Ob2;
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
		Trace::Stream<<"Physics::CreateJointBall : Object ["<<Ob1<<"] doesn't exist"<<endl;
		return 0;
	}
	
	if (i2==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::CreateJointBall : Object ["<<Ob2<<"] doesn't exist"<<endl;
		return 0;
	}

	if (i1->second->Body==0 || i2->second->Body==0)
	{
		Trace::Stream<<"Physics::CreateJointBall : cant connect passive objects"<<endl;
		return 0;
	}

	dJointID j = dJointCreateBall(m_World,0);
	dJointAttach(j,i1->second->Body,i2->second->Body);
	dJointSetBallAnchor(j,Anchor.x,Anchor.y,Anchor.z);

	JointObject *NewJoint = new JointObject;
	NewJoint->Joint=j;
	NewJoint->Type=BallJoint;
	NewJoint->Ob1=Ob1;
	NewJoint->Ob2=Ob2;
	m_JointMap[m_NextJointID]=NewJoint;
	m_NextJointID++;
	return m_NextJointID-1;
}

void Physics::SetJointAngle(int ID, float vel, float angle)
{
	map<int,JointObject*>::iterator i = m_JointMap.find(ID);
	if (i==m_JointMap.end())
	{
		Trace::Stream<<"Physics::SetJointAngle : Joint ["<<ID<<"] doesn't exist"<<endl;
		return;
	}
	
	if (i->second->Type==HingeJoint)
	{
		float cur=dJointGetHingeAngle(i->second->Joint);
		float diff=fabs(cur-angle);
		vel*=diff; // add some damping
		if (cur<angle) dJointSetHingeParam(i->second->Joint,dParamVel,vel);
		else dJointSetHingeParam(i->second->Joint,dParamVel,-vel);
	}
}

void Physics::JointSlide(int ID, float force)
{
	map<int,JointObject*>::iterator i = m_JointMap.find(ID);
	if (i==m_JointMap.end())
	{
		Trace::Stream<<"Physics::JointSlide : Joint ["<<ID<<"] doesn't exist"<<endl;
		return;
	}
	
	if (i->second->Type==SliderJoint)
	{
                dJointAddSliderForce(i->second->Joint, force);
	}
}

void Physics::SetJointParam(int ID, const string &Param, float Value)
{ 
	map<int,JointObject*>::iterator i = m_JointMap.find(ID);
	if (i==m_JointMap.end())
	{
		Trace::Stream<<"Physics::SetJointParam : Joint ["<<ID<<"] doesn't exist"<<endl;
		return;
	}

	int p=-1;
	if (Param=="LoStop")             p=dParamLoStop;
	else if (Param=="HiStop")        p=dParamHiStop;  		           
	else if (Param=="Vel")           p=dParamVel;	           
	else if (Param=="FMax")          p=dParamFMax;            
	else if (Param=="FudgeFactor")   p=dParamFudgeFactor;        
	else if (Param=="Bounce")        p=dParamBounce;           
	else if (Param=="CFM")           p=dParamCFM;  	           
 	else if (Param=="StopERP")       p=dParamStopERP;            
	else if (Param=="StopCFM")       p=dParamStopCFM;
 	else if (Param=="SuspensionERP") p=dParamSuspensionERP; 
	else if (Param=="SuspensionCFM") p=dParamSuspensionCFM;
	else if (Param=="Vel2")		     p=dParamVel2;
	else if (Param=="FMax2")  	     p=dParamFMax2; 
	else 
	{
		Trace::Stream<<"unknown parameter "<<Param<<endl;
		return;
	}
	
	switch (i->second->Type)
	{
		case BallJoint      : break; // no set param required	
		case HingeJoint     : dJointSetHingeParam(i->second->Joint,p,Value); break;	
		case SliderJoint    : dJointSetSliderParam(i->second->Joint,p,Value); break;	
		case ContactJoint   : break;	// no set param required
		case UniversalJoint : dJointSetUniversalParam(i->second->Joint,p,Value); break;	
		case Hinge2Joint    : dJointSetHinge2Param(i->second->Joint,p,Value); break;	
		case FixedJoint     : break;	// no set param required
		case AMotorJoint    : dJointSetAMotorParam(i->second->Joint,p,Value); break;
		default : Trace::Stream<<"unknown joint type "<<i->second->Type<<endl; return; break;	
	} 	
}

bool Physics::HasCollided(int Ob)
{
	map<int,Object*>::iterator i = m_ObjectMap.find(Ob);
	if (i==m_ObjectMap.end())
	{
		Trace::Stream<<"Physics::HasCollided : Object ["<<Ob<<"] doesn't exist"<<endl;
		return false;
	}
	
	// only active objects have bodies to get
	if (i->second->Type==ACTIVE && m_CollisionRecord.find(i->second->Body)!=m_CollisionRecord.end())
	{
		return true;
	}
	
	return false;
}

