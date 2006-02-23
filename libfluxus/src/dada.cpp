/*  Dada
 *  Copyright (C) 2005 David Griffiths <dave@pawfal.org>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
*/ 
#include "dada.h"

static const int SINCOS_TABLESIZE = 2048;
static float SinTab[SINCOS_TABLESIZE];
static float CosTab[SINCOS_TABLESIZE];
static const float SINCOS_LOOKUP=SINCOS_TABLESIZE/(float)TWO_PI;

float RandFloat()
{
	return rand()%10000/10000.0f;
}

float RandRange(float L, float H)
{
	return ((rand()%10000/10000.0f)*(H-L))+L;
}

void InitDada()
{
	for (int n=0; n<SINCOS_TABLESIZE; n++)
	{
		float a=n*(TWO_PI/(float)SINCOS_TABLESIZE);
		SinTab[n]=sin(a);
		CosTab[n]=cos(a);
	}
}

void dSinCos(float a, float &s, float &c)
{
	int Index=(int)rint(a*SINCOS_LOOKUP)&SINCOS_TABLESIZE-1;
	s=SinTab[Index];
	c=CosTab[Index];	
}

dVector &dVector::operator=(dVector const &rhs)
{
    x=rhs.x; y=rhs.y; z=rhs.z; w=rhs.w;
    return *this;
}

dVector dVector::operator+(dVector const &rhs) const
{
    dVector t;
    t.x=x+rhs.x; t.y=y+rhs.y; t.z=z+rhs.z; //t.w=w+rhs.w;
    return t;
}

dVector dVector::operator-(dVector const &rhs) const
{
    dVector t;
    t.x=x-rhs.x; t.y=y-rhs.y; t.z=z-rhs.z; //t.w=w-rhs.w;
    return t;
}

dVector dVector::operator*(float rhs) const
{
    dVector t;
    t.x=x*rhs; t.y=y*rhs; t.z=z*rhs; //t.w=w*rhs;
    return t;
}

dVector dVector::operator/(float rhs) const
{
    dVector t;
    t.x=x/rhs; t.y=y/rhs; t.z=z/rhs; //t.w=w/rhs;
    return t;
}

dVector &dVector::operator+=(dVector const &rhs)
{
    x+=rhs.x; y+=rhs.y; z+=rhs.z; //w+=rhs.w;
    return *this;
}

dVector &dVector::operator-=(dVector const &rhs)
{
    x-=rhs.x; y-=rhs.y; z-=rhs.z; //w-=rhs.w;
    return *this;
}

dVector &dVector::operator*=(float rhs)
{
    x*=rhs; y*=rhs; z*=rhs; //w*=rhs;
    return *this;
}

dVector &dVector::operator/=(float rhs)
{
    if (rhs) {x/=rhs; y/=rhs; z/=rhs;}// w/=rhs;}
    return *this;
}

float dVector::dot(dVector const &rhs) const
{
    return x*rhs.x+y*rhs.y+z*rhs.z;
}

dVector dVector::cross(dVector const &rhs) const
{
    return dVector(y*rhs.z - z*rhs.y,
                      z*rhs.x - x*rhs.z,
                      x*rhs.y - y*rhs.x);
}

float dVector::dist(dVector const &rhs) const
{
    return sqrt((rhs.x-x)*(rhs.x-x)+
                (rhs.y-y)*(rhs.y-y)+
                (rhs.z-z)*(rhs.z-z));
}

void dVector::get_euler(float &rx, float &ry, float &rz) const
{
	if (z==0) rx=0;
	else rx=atan(y/z)*RAD_CONV;
	if (x==0) ry=0;
	else ry=atan(z/x)*RAD_CONV;
	if (y==0) rz=0;
	else rz=atan(x/y)*RAD_CONV;
}

float dVector::mag()
{
    return dist(dVector(0,0,0));
}

dVector operator-(dVector rhs)
{
    return dVector(-rhs.x,-rhs.y,-rhs.z);
}

ostream &operator<<(ostream &os, dVector const &om)
{
    os<<om.x<<" "<<om.y<<" "<<om.z<<" "<<om.w<<" ";
    return os;
}

istream &operator>>(istream &is, dVector &om)
{
    is>>om.x>>om.y>>om.z>>om.w;
    return is;
}

void dVector::get_rot(float m[16],dVector up)
	{
	dVector a,b,c;
	a.x=this->x; a.y=this->y; a.z=this->z;
	a.normalise();
	if (a==up) a.x+=0.01;
	b=a.cross(up);
	b.normalise();
	c=b.cross(a);
	c.normalise();
	
	for (int n=0; n<16; n++)
		m[n]=0;

	m[15]=1;

	m[0]=a.x; m[1]=a.y;	m[2]=a.z;	
	m[4]=b.x; m[5]=b.y;	m[6]=b.z;	
	m[8]=c.x; m[9]=c.y;	m[10]=c.z;
	}
	
////

dColour &dColour::operator=(dColour const &rhs)
{
    r=rhs.r; g=rhs.g; b=rhs.b; a=rhs.a;
    return *this;
}

dColour dColour::operator+(dColour const &rhs)
{
    dColour t;
    t.r=r+rhs.r; t.g=g+rhs.g; t.b=b+rhs.b; t.a=a+rhs.a;
    return t;
}

dColour dColour::operator-(dColour const &rhs)
{
    dColour t;
    t.r=r-rhs.r; t.g=g-rhs.g; t.b=b-rhs.b; t.a=a-rhs.a;
    return t;
}

dColour dColour::operator*(float rhs)
{
    dColour t;
    t.r=r*rhs; t.g=g*rhs; t.b=b*rhs; t.a=a*rhs;
    return t;
}

dColour dColour::operator/(float rhs)
{
    dColour t;
    t.r=r/rhs; t.g=g/rhs; t.b=b/rhs; t.a=a/rhs;
    return t;
}

dColour &dColour::operator+=(dColour const &rhs)
{
    r+=rhs.r; g+=rhs.g; b+=rhs.b; a+=rhs.a;
    return *this;
}

dColour &dColour::operator-=(dColour const &rhs)
{
    r-=rhs.r; g-=rhs.g; b-=rhs.b; a-=rhs.a;
    return *this;
}

dColour &dColour::operator*=(float rhs)
{
    r*=rhs; g*=rhs; b*=rhs; a*=rhs;
    return *this;
}

dColour &dColour::operator/=(float rhs)
{
    if (rhs) {r/=rhs; g/=rhs; b/=rhs; a/=rhs;}
    return *this;
}

ostream &operator<<(ostream &os, dColour const &om)
{
    os<<"r="<<om.r<<" g="<<om.g<<" b="<<om.b<<" a="<<om.a<<" ";
    return os;
}

////

dVertex const &dVertex::operator=(dVertex const &rhs)
{
    point=rhs.point;
    normal=rhs.normal;
    col=rhs.col;
    s=rhs.s;
    t=rhs.t;
    return rhs;
}

ostream &operator<<(ostream &os, dVertex const &v)
{
    os<<"Vertex : p="<<v.point<<" n="<<v.normal<<v.col<<" "<<v.s<<" "<<v.t<<endl;
    return os;
}

////

void dMatrix::init()
{
memset(m,0,sizeof(float)*16);
m[0][0]=m[1][1]=m[2][2]=m[3][3]=1;
}

const dMatrix &dMatrix::operator=(dMatrix const &rhs)
{
m[0][0]=rhs.m[0][0]; m[0][1]=rhs.m[0][1]; m[0][2]=rhs.m[0][2]; m[0][3]=rhs.m[0][3];
m[1][0]=rhs.m[1][0]; m[1][1]=rhs.m[1][1]; m[1][2]=rhs.m[1][2]; m[1][3]=rhs.m[1][3];
m[2][0]=rhs.m[2][0]; m[2][1]=rhs.m[2][1]; m[2][2]=rhs.m[2][2]; m[2][3]=rhs.m[2][3];
m[3][0]=rhs.m[3][0]; m[3][1]=rhs.m[3][1]; m[3][2]=rhs.m[3][2]; m[3][3]=rhs.m[3][3];
return rhs;
}

dMatrix dMatrix::operator+(dMatrix const &rhs)
{
    dMatrix t;
    for (int i=0; i<4; i++)
        for (int j=0; j<4; j++)
            t.m[i][j]=m[i][j]+rhs.m[i][j];
    return t;
}

dMatrix dMatrix::operator-(dMatrix const &rhs)
{
    dMatrix t;
    for (int i=0; i<4; i++)
        for (int j=0; j<4; j++)
            t.m[i][j]=m[i][j]-rhs.m[i][j];
    return t;
}

dMatrix dMatrix::operator*(dMatrix const &rhs)
{
    //dMatrix t;
    //for (int i=0; i<4; i++)
    //    for (int j=0; j<4; j++)
    //        t.m[i][j]=m[i][0]*rhs.m[0][j]+
    //                  m[i][1]*rhs.m[1][j]+
    //                  m[i][2]*rhs.m[2][j]+
    //                  m[i][3]*rhs.m[3][j];

    dMatrix t;
    /*for (int i=0; i<4; i++)
        for (int j=0; j<4; j++)
    t.m[i][j]=m[0][j]*rhs.m[i][0]+m[1][j]*rhs.m[i][1]+m[2][j]*rhs.m[i][2]+m[3][j]*rhs.m[i][3];
    */

    t.m[0][0]=m[0][0]*rhs.m[0][0]+m[1][0]*rhs.m[0][1]+m[2][0]*rhs.m[0][2]+m[3][0]*rhs.m[0][3];
    t.m[0][1]=m[0][1]*rhs.m[0][0]+m[1][1]*rhs.m[0][1]+m[2][1]*rhs.m[0][2]+m[3][1]*rhs.m[0][3];
    t.m[0][2]=m[0][2]*rhs.m[0][0]+m[1][2]*rhs.m[0][1]+m[2][2]*rhs.m[0][2]+m[3][2]*rhs.m[0][3];
    t.m[0][3]=m[0][3]*rhs.m[0][0]+m[1][3]*rhs.m[0][1]+m[2][3]*rhs.m[0][2]+m[3][3]*rhs.m[0][3];

    t.m[1][0]=m[0][0]*rhs.m[1][0]+m[1][0]*rhs.m[1][1]+m[2][0]*rhs.m[1][2]+m[3][0]*rhs.m[1][3];
    t.m[1][1]=m[0][1]*rhs.m[1][0]+m[1][1]*rhs.m[1][1]+m[2][1]*rhs.m[1][2]+m[3][1]*rhs.m[1][3];
    t.m[1][2]=m[0][2]*rhs.m[1][0]+m[1][2]*rhs.m[1][1]+m[2][2]*rhs.m[1][2]+m[3][2]*rhs.m[1][3];
    t.m[1][3]=m[0][3]*rhs.m[1][0]+m[1][3]*rhs.m[1][1]+m[2][3]*rhs.m[1][2]+m[3][3]*rhs.m[1][3];

    t.m[2][0]=m[0][0]*rhs.m[2][0]+m[1][0]*rhs.m[2][1]+m[2][0]*rhs.m[2][2]+m[3][0]*rhs.m[2][3];
    t.m[2][1]=m[0][1]*rhs.m[2][0]+m[1][1]*rhs.m[2][1]+m[2][1]*rhs.m[2][2]+m[3][1]*rhs.m[2][3];
    t.m[2][2]=m[0][2]*rhs.m[2][0]+m[1][2]*rhs.m[2][1]+m[2][2]*rhs.m[2][2]+m[3][2]*rhs.m[2][3];
    t.m[2][3]=m[0][3]*rhs.m[2][0]+m[1][3]*rhs.m[2][1]+m[2][3]*rhs.m[2][2]+m[3][3]*rhs.m[2][3];

    t.m[3][0]=m[0][0]*rhs.m[3][0]+m[1][0]*rhs.m[3][1]+m[2][0]*rhs.m[3][2]+m[3][0]*rhs.m[3][3];
    t.m[3][1]=m[0][1]*rhs.m[3][0]+m[1][1]*rhs.m[3][1]+m[2][1]*rhs.m[3][2]+m[3][1]*rhs.m[3][3];
    t.m[3][2]=m[0][2]*rhs.m[3][0]+m[1][2]*rhs.m[3][1]+m[2][2]*rhs.m[3][2]+m[3][2]*rhs.m[3][3];
    t.m[3][3]=m[0][3]*rhs.m[3][0]+m[1][3]*rhs.m[3][1]+m[2][3]*rhs.m[3][2]+m[3][3]*rhs.m[3][3];

    return t;
}

dMatrix dMatrix::operator/(dMatrix const &rhs)
{
    dMatrix t;
    for (int i=0; i<4; i++)
        for (int j=0; j<4; j++)
            t.m[i][j]=m[i][0]/rhs.m[0][j]+
                      m[i][1]/rhs.m[1][j]+
                      m[i][2]/rhs.m[2][j]+
                      m[i][3]/rhs.m[3][j];
    return t;
}

dMatrix &dMatrix::operator+=(dMatrix const &rhs)
{
    for (int i=0; i<4; i++)
        for (int j=0; j<4; j++)
            m[i][j]+=rhs.m[i][j];
    return *this;
}

dMatrix &dMatrix::operator-=(dMatrix const &rhs)
{
    for (int i=0; i<4; i++)
        for (int j=0; j<4; j++)
            m[i][j]-=rhs.m[i][j];
    return *this;
}

dMatrix &dMatrix::operator*=(dMatrix const &rhs)
{
    *this=*this*rhs;
    return *this;
}

dMatrix &dMatrix::operator/=(dMatrix const &rhs)
{
    *this=*this/rhs;
    return *this;
}

dMatrix &dMatrix::translate(float x, float y, float z)
{
    dMatrix t;
    t.m[3][0]=x;
    t.m[3][1]=y;
    t.m[3][2]=z;
    *this=*this*t;
    return *this;

}

dMatrix &dMatrix::translate(dVector &tr)
{
    dMatrix t;
    t.m[3][0]=tr.x;
    t.m[3][1]=tr.y;
    t.m[3][2]=tr.z;
    *this=*this*t;
    return *this;

}

void dMatrix::settranslate(dVector &tr)
{
    m[3][0]=tr.x;
    m[3][1]=tr.y;
    m[3][2]=tr.z;
}

dVector dMatrix::gettranslate()
{
    return dVector(m[3][0],m[3][1],m[3][2]);
}

#define USE_FAST_SINCOS

dMatrix &dMatrix::rotxyz(float x,float y,float z)
{
	dMatrix t;
	if (x)
	{
		x*=0.017453292;
		
		#ifdef USE_FAST_SINCOS
		float sx,cx;
		dSinCos(x,sx,cx);
		#else
		float sx=sin(x);
		float cx=cos(x);
		#endif
		
    	t.m[1][1]=cx;
    	t.m[2][1]=-sx;
		t.m[1][2]=sx;
		t.m[2][2]=cx;
		*this=*this*t;
	}
	
	if (y)
	{
		y*=0.017453292;
		
		#ifdef USE_FAST_SINCOS
		float sy,cy;
		dSinCos(y,sy,cy);
		#else
		float sy=sin(y);
		float cy=cos(y);
		#endif
		
		t.init();
		t.m[0][0]=cy;
	    t.m[2][0]=-sy;
		t.m[0][2]=sy;
		t.m[2][2]=cy;
		*this=*this*t;
    }

    if (z)
    {
    	z*=0.017453292;
    	
    	#ifdef USE_FAST_SINCOS
    	float sz,cz;
    	dSinCos(z,sz,cz);
    	#else
		float sz=sin(z);
		float cz=cos(z);
		#endif
		
		t.init();
    	t.m[0][0]=cz;
    	t.m[1][0]=-sz;
		t.m[0][1]=sz;
		t.m[1][1]=cz;
	    *this=*this*t;
    }

    return *this;
}

dMatrix &dMatrix::rotx(float a)
{
    a*=0.017453292;
    dMatrix t;

    t.m[1][1]=cos(a);
    t.m[2][1]=-sin(a);
	t.m[1][2]=sin(a);
	t.m[2][2]=cos(a);

    *this=*this*t;
    return *this;
}

dMatrix &dMatrix::roty(float a)
{
    a*=0.017453292;
    dMatrix t;

    t.m[0][0]=cos(a);
    t.m[2][0]=-sin(a);
	t.m[0][2]=sin(a);
	t.m[2][2]=cos(a);

    *this=*this*t;
    return *this;
}

dMatrix &dMatrix::rotz(float a)
{
    a*=0.017453292;
    dMatrix t;

    t.m[0][0]=cos(a);
    t.m[1][0]=-sin(a);
	t.m[0][1]=sin(a);
	t.m[1][1]=cos(a);

    *this=*this*t;
    return *this;
}

dMatrix &dMatrix::scale(float x, float y, float z)
{
    dMatrix t;

    t.m[0][0]=x;
    t.m[1][1]=y;
	t.m[2][2]=z;

    *this=*this*t;
    return *this;
}

dVector dMatrix::transform(dVector const &p) const
{
    dVector t;
    t.x=p.x*m[0][0] + p.y*m[1][0] + p.z*m[2][0] + p.w*m[3][0];
    t.y=p.x*m[0][1] + p.y*m[1][1] + p.z*m[2][1] + p.w*m[3][1];
    t.z=p.x*m[0][2] + p.y*m[1][2] + p.z*m[2][2] + p.w*m[3][2];
    t.w=p.x*m[0][3] + p.y*m[1][3] + p.z*m[2][3] + p.w*m[3][3];
    return t;
}

dVertex dMatrix::transform(dVertex const &p) const
{
    dVertex t=p;
    t.point=transform(p.point);
    t.normal=transform_no_trans(p.normal);
    return t;
}

dVector dMatrix::transform_no_trans(dVector const &p) const
{
    dVector t;
    t.x=p.x*m[0][0] + p.y*m[1][0] + p.z*m[2][0];
    t.y=p.x*m[0][1] + p.y*m[1][1] + p.z*m[2][1];
    t.z=p.x*m[0][2] + p.y*m[1][2] + p.z*m[2][2];
    t.w=p.w;
    return t;
}

/*void dMatrix::load_glmatrix(float glm[16])
{
	glm[0]= m[0][0]; glm[1]= m[1][0]; glm[2]= m[2][0]; glm[3]= m[3][0];
	glm[4]= m[0][1]; glm[5]= m[1][1]; glm[6]= m[2][1]; glm[7]= m[3][1];
	glm[8]= m[0][2]; glm[9]= m[1][2]; glm[10]=m[2][2]; glm[11]=m[3][2];
	glm[12]=m[0][3]; glm[13]=m[1][3]; glm[14]=m[2][3]; glm[15]=m[3][3];
}*/

void dMatrix::load_glmatrix(float glm[16])
{
	glm[0]= m[0][0]; glm[4]= m[1][0]; glm[8]= m[2][0]; glm[12]= m[3][0];
	glm[1]= m[0][1]; glm[5]= m[1][1]; glm[9]= m[2][1]; glm[13]= m[3][1];
	glm[2]= m[0][2]; glm[6]= m[1][2]; glm[10]=m[2][2]; glm[14]=m[3][2];
	glm[3]= m[0][3]; glm[7]= m[1][3]; glm[11]=m[2][3]; glm[15]=m[3][3];
}

void dMatrix::load_dMatrix(float glm[16])
{
	m[0][0]=glm[0]; m[1][0]=glm[4]; m[2][0]=glm[8]; m[3][0]=glm[12];
	m[0][1]=glm[1]; m[1][1]=glm[5]; m[2][1]=glm[9]; m[3][1]=glm[13];
	m[0][2]=glm[2]; m[1][2]=glm[6]; m[2][2]=glm[10]; m[3][2]=glm[14];
	m[0][3]=glm[3]; m[1][3]=glm[7]; m[2][3]=glm[11]; m[3][3]=glm[15];
}

void dMatrix::transpose()
{
	dMatrix t;
	for (int i=0; i<4; i++)
        for (int j=0; j<4; j++)
            t.m[i][j]=m[j][i];
    *this=t;
}

dMatrix dMatrix::inverse()
{
	dMatrix temp;
	temp.m[0][0] = m[1][2]*m[2][3]*m[3][1] - m[1][3]*m[2][2]*m[3][1] + m[1][3]*m[2][1]*m[3][2] - m[1][1]*m[2][3]*m[3][2] - m[1][2]*m[2][1]*m[3][3] + m[1][1]*m[2][2]*m[3][3];
	temp.m[0][1] = m[0][3]*m[2][2]*m[3][1] - m[0][2]*m[2][3]*m[3][1] - m[0][3]*m[2][1]*m[3][2] + m[0][1]*m[2][3]*m[3][2] + m[0][2]*m[2][1]*m[3][3] - m[0][1]*m[2][2]*m[3][3];
	temp.m[0][2] = m[0][2]*m[1][3]*m[3][1] - m[0][3]*m[1][2]*m[3][1] + m[0][3]*m[1][1]*m[3][2] - m[0][1]*m[1][3]*m[3][2] - m[0][2]*m[1][1]*m[3][3] + m[0][1]*m[1][2]*m[3][3];
	temp.m[0][3] = m[0][3]*m[1][2]*m[2][1] - m[0][2]*m[1][3]*m[2][1] - m[0][3]*m[1][1]*m[2][2] + m[0][1]*m[1][3]*m[2][2] + m[0][2]*m[1][1]*m[2][3] - m[0][1]*m[1][2]*m[2][3];
	temp.m[1][0] = m[1][3]*m[2][2]*m[3][0] - m[1][2]*m[2][3]*m[3][0] - m[1][3]*m[2][0]*m[3][2] + m[1][0]*m[2][3]*m[3][2] + m[1][2]*m[2][0]*m[3][3] - m[1][0]*m[2][2]*m[3][3];
	temp.m[1][1] = m[0][2]*m[2][3]*m[3][0] - m[0][3]*m[2][2]*m[3][0] + m[0][3]*m[2][0]*m[3][2] - m[0][0]*m[2][3]*m[3][2] - m[0][2]*m[2][0]*m[3][3] + m[0][0]*m[2][2]*m[3][3];
	temp.m[1][2] = m[0][3]*m[1][2]*m[3][0] - m[0][2]*m[1][3]*m[3][0] - m[0][3]*m[1][0]*m[3][2] + m[0][0]*m[1][3]*m[3][2] + m[0][2]*m[1][0]*m[3][3] - m[0][0]*m[1][2]*m[3][3];
	temp.m[1][3] = m[0][2]*m[1][3]*m[2][0] - m[0][3]*m[1][2]*m[2][0] + m[0][3]*m[1][0]*m[2][2] - m[0][0]*m[1][3]*m[2][2] - m[0][2]*m[1][0]*m[2][3] + m[0][0]*m[1][2]*m[2][3];
	temp.m[2][0] = m[1][1]*m[2][3]*m[3][0] - m[1][3]*m[2][1]*m[3][0] + m[1][3]*m[2][0]*m[3][1] - m[1][0]*m[2][3]*m[3][1] - m[1][1]*m[2][0]*m[3][3] + m[1][0]*m[2][1]*m[3][3];
	temp.m[2][1] = m[0][3]*m[2][1]*m[3][0] - m[0][1]*m[2][3]*m[3][0] - m[0][3]*m[2][0]*m[3][1] + m[0][0]*m[2][3]*m[3][1] + m[0][1]*m[2][0]*m[3][3] - m[0][0]*m[2][1]*m[3][3];
	temp.m[2][2] = m[0][1]*m[1][3]*m[3][0] - m[0][3]*m[1][1]*m[3][0] + m[0][3]*m[1][0]*m[3][1] - m[0][0]*m[1][3]*m[3][1] - m[0][1]*m[1][0]*m[3][3] + m[0][0]*m[1][1]*m[3][3];
	temp.m[2][3] = m[0][3]*m[1][1]*m[2][0] - m[0][1]*m[1][3]*m[2][0] - m[0][3]*m[1][0]*m[2][1] + m[0][0]*m[1][3]*m[2][1] + m[0][1]*m[1][0]*m[2][3] - m[0][0]*m[1][1]*m[2][3];
	temp.m[3][0] = m[1][2]*m[2][1]*m[3][0] - m[1][1]*m[2][2]*m[3][0] - m[1][2]*m[2][0]*m[3][1] + m[1][0]*m[2][2]*m[3][1] + m[1][1]*m[2][0]*m[3][2] - m[1][0]*m[2][1]*m[3][2];
	temp.m[3][1] = m[0][1]*m[2][2]*m[3][0] - m[0][2]*m[2][1]*m[3][0] + m[0][2]*m[2][0]*m[3][1] - m[0][0]*m[2][2]*m[3][1] - m[0][1]*m[2][0]*m[3][2] + m[0][0]*m[2][1]*m[3][2];
	temp.m[3][2] = m[0][2]*m[1][1]*m[3][0] - m[0][1]*m[1][2]*m[3][0] - m[0][2]*m[1][0]*m[3][1] + m[0][0]*m[1][2]*m[3][1] + m[0][1]*m[1][0]*m[3][2] - m[0][0]*m[1][1]*m[3][2];
	temp.m[3][3] = m[0][1]*m[1][2]*m[2][0] - m[0][2]*m[1][1]*m[2][0] + m[0][2]*m[1][0]*m[2][1] - m[0][0]*m[1][2]*m[2][1] - m[0][1]*m[1][0]*m[2][2] + m[0][0]*m[1][1]*m[2][2];
   float scale=1/temp.determinant();
   temp.scale(scale,scale,scale);
   return temp;
}

float dMatrix::determinant() 
{
   return 
   m[0][3] * m[1][2] * m[2][1] * m[3][0]-m[0][2] * m[1][3] * m[2][1] * m[3][0]-m[0][3] * m[1][1] * m[2][2] * m[3][0]+m[0][1] * m[1][3] * m[2][2] * m[3][0]+
   m[0][2] * m[1][1] * m[2][3] * m[3][0]-m[0][1] * m[1][2] * m[2][3] * m[3][0]-m[0][3] * m[1][2] * m[2][0] * m[3][1]+m[0][2] * m[1][3] * m[2][0] * m[3][1]+
   m[0][3] * m[1][0] * m[2][2] * m[3][1]-m[0][0] * m[1][3] * m[2][2] * m[3][1]-m[0][2] * m[1][0] * m[2][3] * m[3][1]+m[0][0] * m[1][2] * m[2][3] * m[3][1]+
   m[0][3] * m[1][1] * m[2][0] * m[3][2]-m[0][1] * m[1][3] * m[2][0] * m[3][2]-m[0][3] * m[1][0] * m[2][1] * m[3][2]+m[0][0] * m[1][3] * m[2][1] * m[3][2]+
   m[0][1] * m[1][0] * m[2][3] * m[3][2]-m[0][0] * m[1][1] * m[2][3] * m[3][2]-m[0][2] * m[1][1] * m[2][0] * m[3][3]+m[0][1] * m[1][2] * m[2][0] * m[3][3]+
   m[0][2] * m[1][0] * m[2][1] * m[3][3]-m[0][0] * m[1][2] * m[2][1] * m[3][3]-m[0][1] * m[1][0] * m[2][2] * m[3][3]+m[0][0] * m[1][1] * m[2][2] * m[3][3];
}

void dMatrix::remove_scale()
{
	dVector xvec = get_hori_i().normalise();
	dVector yvec = get_hori_j().normalise();
	dVector zvec = get_hori_k().normalise();
	
	m[0][0]=xvec.x; m[1][0]=xvec.y; m[2][0]=xvec.z;
	m[0][1]=yvec.x; m[1][1]=yvec.y; m[2][1]=yvec.z;
	m[0][2]=zvec.x; m[1][2]=zvec.y; m[2][2]=zvec.z;
}

void dMatrix::extract_euler(float &x, float &y, float &z)
{
	dMatrix t=*this;
	t.remove_scale();
	if (t.m[2][2]==0) x=0;
	else x = atan(t.m[1][2]/t.m[2][2])*RAD_CONV;
	y = asin(-t.m[0][2])*RAD_CONV;
 	if (t.m[0][0]==0) z=0;
	else z=atan(t.m[0][1]/t.m[0][0])*RAD_CONV;


	/*dVector xvec = get_hori_i().normalise();
	dVector yvec = get_hori_j().normalise();
	dVector zvec = get_hori_k().normalise();
	float d1,d2;
	xvec.get_euler(x,d1,d2);
	cerr<<x<<" "<<d1<<" "<<d2<<endl;
	yvec.get_euler(d1,y,d2);
	cerr<<d1<<" "<<y<<" "<<d2<<endl;
	zvec.get_euler(d1,d2,z);
	cerr<<d1<<" "<<d2<<" "<<z<<endl;*/
}
	
void dMatrix::aim(dVector v, dVector up)
{
	v.normalise();
	dVector l=v.cross(up);
	dVector u=v.cross(l);
	
	m[0][0]=v.x; m[0][1]=v.y; m[0][2]=v.z;
	m[1][0]=l.x; m[1][1]=l.y; m[1][2]=l.z;
	m[2][0]=u.x; m[2][1]=u.y; m[2][2]=u.z;	
}

ostream &operator<<(ostream &os, dMatrix const &om)
{
    for (int j=0; j<4; j++)
        for (int i=0; i<4; i++)
            os<<om.m[i][j]<<" ";

    return os;
}

/*
void dAxis::aimx(dVector a, dVector up)
{
	if (up.mag()!=1) up.normalise();
	if (a.mag()!=1) a.normalise();
	i=a;
	if (i==up) up.x+=0.000001;
	j=i.cross(up);
	k=i.cross(j);
}
*/
	
void dBoundingBox::expand(dVector v)
{
	if (m_Empty)
	{	
		min=v;
		max=v;
		m_Empty=false;
	}
	
	if (v.x<min.x) min.x=v.x;
	if (v.y<min.y) min.y=v.y;
	if (v.z<min.z) min.z=v.z;
	
	if (v.x>=max.x) max.x=v.x;
	if (v.y>=max.y) max.y=v.y;
	if (v.z>=max.z) max.z=v.z;
}

void dBoundingBox::expand(dBoundingBox v)
{
	expand(v.min);
	expand(dVector(v.max.x,v.min.y,v.min.z));
	expand(dVector(v.min.x,v.max.y,v.min.z));
	expand(dVector(v.max.x,v.max.y,v.min.z));
	expand(dVector(v.min.x,v.min.y,v.max.z));
	expand(dVector(v.max.x,v.min.y,v.max.z));
	expand(dVector(v.min.x,v.max.y,v.max.z));
	expand(v.max);
}

void dBoundingBox::expandby(float a)
{
	max.x+=a; max.y+=a; max.z+=a;
	min.x-=a; min.y-=a; min.z-=a; 
}

bool dBoundingBox::inside(dVector p) const
{ 
	return (p.x>min.x && p.x<max.x &&
			p.y>min.y && p.y<max.y &&
			p.z>min.z && p.z<max.z);
}
	
// conversions
dMatrix dQuat::toMatrix() const
{
	float Nq = x*x + y*y + z*z + w*w;
	float s = (Nq > 0.f) ? (2.0f / Nq) : 0.f;
	float xs = x*s, ys = y*s, zs = z*s;
	float wx = w*xs, wy = w*ys, wz = w*zs;
	float xx = x*xs, xy = x*ys, xz = x*zs;
	float yy = y*ys, yz = y*zs, zz = z*zs;
	return dMatrix(1.0f - (yy + zz),
			   xy + wz,
			   xz - wy,
			   0,
			   xy - wz,          
			   1.0f - (xx + zz),
			   yz + wx,
			   0,
			   xz + wy,          
			   yz - wx,          
			   1.0f - (xx + yy),
			   0, 0, 0, 0, 1.0f);

}

// operations
dQuat dQuat::conjugate() const
{
	return dQuat(-x,-y,-z,w);
}

// make multiply look like multiply
dQuat dQuat::operator* (const dQuat&qR) const
{
	dQuat qq;
	qq.w = w*qR.w - x*qR.x - y*qR.y - z*qR.z;
	qq.x = w*qR.x + x*qR.w + y*qR.z - z*qR.y;
	qq.y = w*qR.y + y*qR.w + z*qR.x - x*qR.z;
	qq.z = w*qR.z + z*qR.w + x*qR.y - y*qR.x;
	return (qq);
}

void dQuat::renorm() 
{
	float Nq = 1.f / (float) (x*x + y*y + z*z + w*w);
	x *= Nq;
	y *= Nq;
	z *= Nq;
	w *= Nq;
}
