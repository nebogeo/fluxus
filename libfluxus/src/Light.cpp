#include "Light.h"
#include "State.h"

using namespace fluxus;

Light::Light() :
m_Ambient(0.2,0.2,0.2),
m_Diffuse(1,1,1),
m_Specular(1,1,1),
m_Position(0,0,0),
m_CameraLock(false)
{
}

void Light::Render(int index)
{
	glPushMatrix();
	glTranslatef(m_Position.x,m_Position.y,m_Position.z);
	
	//glDisable(GL_LIGHTING);
	//glutSolidSphere(1,10,10);
	//glEnable(GL_LIGHTING);

	glLightfv(GL_LIGHT0+index, GL_AMBIENT,  m_Ambient.arr());
 	glLightfv(GL_LIGHT0+index, GL_DIFFUSE,  m_Diffuse.arr());
  	glLightfv(GL_LIGHT0+index, GL_SPECULAR, m_Specular.arr());
	glLightfv(GL_LIGHT0+index, GL_POSITION, dVector(0,0,0).arr());
  	glEnable(GL_LIGHT0+index);
	glPopMatrix();	
}

