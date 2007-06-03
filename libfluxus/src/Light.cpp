#include "Light.h"
#include "State.h"

using namespace Fluxus;

Light::Light() :
m_Index(0),
m_Ambient(0.2,0.2,0.2),
m_Diffuse(1,1,1),
m_Specular(1,1,1),
m_Position(0,0,0),
m_Direction(0,0,0),
m_Type(POINT),
m_CameraLock(false)
{
}

Light::~Light() 
{
	glDisable(GL_LIGHT0+m_Index);
}

void Light::SetIndex(int s) 
{ 
	m_Index=s; 
	glEnable(GL_LIGHT0+m_Index); 
}

void Light::SetAmbient(dColour s)  
{ 
	glLightfv(GL_LIGHT0+m_Index, GL_AMBIENT,  s.arr()); 
}

void Light::SetDiffuse(dColour s)  
{ 
	glLightfv(GL_LIGHT0+m_Index, GL_DIFFUSE,  s.arr()); 
}

void Light::SetSpecular(dColour s) 
{ 
	glLightfv(GL_LIGHT0+m_Index, GL_SPECULAR,  s.arr()); 
}

void Light::SetSpotAngle(float s) 
{ 
	if (m_Type==SPOT) glLightf(GL_LIGHT0+m_Index, GL_SPOT_CUTOFF,  s); 
}

void Light::SetSpotExponent(float s) 
{ 
	if (m_Type==SPOT) glLightf(GL_LIGHT0+m_Index, GL_SPOT_EXPONENT,  s); 
}

void Light::SetPosition(dVector s) 
{ 
	m_Position=s; 
}

void Light::SetAttenuation(int type, float s)
{
	switch (type)
	{
		case 0: glLightf(GL_LIGHT0+m_Index, GL_CONSTANT_ATTENUATION, s); break;
		case 1: glLightf(GL_LIGHT0+m_Index, GL_LINEAR_ATTENUATION, s); break;
		case 2: glLightf(GL_LIGHT0+m_Index, GL_QUADRATIC_ATTENUATION, s); break;
	}
}

void Light::SetDirection(dVector s) 
{ 
	m_Direction=s; 
}


void Light::Render()
{
	glPushMatrix();
	glTranslatef(m_Position.x,m_Position.y,m_Position.z);
	
	if (m_Type==DIRECTIONAL)
	{
		float pos[4] = { m_Direction.x,m_Direction.y,m_Direction.z,0 };
		glLightfv(GL_LIGHT0+m_Index, GL_POSITION, pos);
	}
	else
	{
		if (m_Type==SPOT)
		{
			float pos[4] = { m_Direction.x,m_Direction.y,m_Direction.z,1 };
			glLightfv(GL_LIGHT0+m_Index, GL_SPOT_DIRECTION, pos);
		}
		
		float pos[4] = { 0,0,0,1 };
		glLightfv(GL_LIGHT0+m_Index, GL_POSITION, pos);
	}
	
	glPopMatrix();	
}

