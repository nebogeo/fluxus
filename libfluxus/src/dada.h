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
// Dada.h 
// A collection of handy classes for 3D graphics.
// (Mostly half finished)
//

#include <math.h>
#include <iostream>
#include <list>

using namespace std;

#ifndef DADA
#define DADA
static const float TWO_PI=3.141592654*2.0f;
static const float DEG_CONV = 0.017453292;
static const float RAD_CONV = 1/0.017453292;

inline void debug(char *s) {cerr<<"dada debug: "<<s<<endl;}

void  InitDada();
float RandFloat();
float RandRange(float L, float H);
void  dSinCos(float a, float &s, float &c);

class dVector
{
public:
       float x,y,z,w;
       dVector() {x=y=z=0; w=1;}
       dVector(float X, float Y, float Z, float W=1) {x=X; y=Y; z=Z; w=W;}
       dVector(dVector const &c) {*this=c;}
		
	float *arr() {return &x;}
	int operator==(dVector const &rhs) {return (x==rhs.x&&y==rhs.y&&z==rhs.z);}
       dVector &operator=(dVector const &rhs);
       dVector operator+(dVector const &rhs) const;
       dVector operator-(dVector const &rhs) const;
       dVector operator*(float rhs) const;
       dVector operator/(float rhs) const;
       dVector &operator+=(dVector const &rhs);
       dVector &operator-=(dVector const &rhs);
       dVector &operator*=(float rhs);
       dVector &operator/=(float rhs);
       dVector cross(dVector const &rhs) const;
       float dot(dVector const &rhs) const;
       float dist(dVector const &rhs) const;
       float mag();
       void get_euler(float &rx, float &ry, float &rz) const;
       void homog() {if (w && w!=1.0) {x/=w; y/=w; z/=w; w=1;}}
       dVector &normalise() {*this/=mag(); return *this;}
		 
		 void get_rot(float m[16],dVector up); // legacy func 
private:
};

dVector operator-(dVector rhs);
ostream &operator<<(ostream &os, dVector const &om);
istream &operator>>(istream &is, dVector &om);

////

class dColour
{
public:
       float r,g,b,a;
       dColour() {r=g=b=0; a=1;}
       dColour(float R, float G, float B, float A=1) {r=R; g=G; b=B; a=A;}
       dColour(dColour const &c) {*this=c;}
	float *arr() {return &r;}

       dColour &operator=(dColour const &rhs);
       dColour operator+(dColour const &rhs);
       dColour operator-(dColour const &rhs);
       dColour operator*(float rhs);
       dColour operator/(float rhs);
       dColour &operator+=(dColour const &rhs);
       dColour &operator-=(dColour const &rhs);
       dColour &operator*=(float rhs);
       dColour &operator/=(float rhs);

       void clamp()
       {
           if (r<0) r=0; if (g<0) g=0; if (b<0) b=0; if (a<0) a=0;
           if (r>1) r=1; if (g>1) g=1; if (b>1) b=1; if (a>1) a=1;
       }

private:
};

ostream &operator<<(ostream &os, dColour const &om);

////

class dVertex
{
public:
    dVertex() {}
    dVertex(dVector p, dVector n, float S=0, float T=0) {point=p; normal=n; s=S; t=T;}
    dVertex(dVector p, dVector n, dColour c, float S=0, float T=0) { point=p; normal=n; col=c; s=S; t=T;}
    dVertex(dVertex const &rhs) {*this=rhs;}
    dVertex const &operator=(dVertex const &rhs);
    void homog() {point.homog(); normal.homog();}
    friend ostream&operator<<(ostream &os, dVertex const &v);

    dVector point;
    dVector normal;
    dColour col;
    float s,t;
private:
};

class dMatrix
{
public:
    dMatrix() {init();}
	dMatrix(float m00, float m10, float m20, float m30, 
			float m01, float m11, float m21, float m31, 
			float m02, float m12, float m22, float m32, 
			float m03, float m13, float m23, float m33)
			{
			m[0][0]=m00; m[1][0]=m10; m[2][0]=m20; m[3][0]=m30;
			m[0][1]=m01; m[1][1]=m11; m[2][1]=m21; m[3][1]=m31;
			m[0][2]=m02; m[1][2]=m12; m[2][2]=m22; m[3][2]=m32;
			m[0][3]=m03; m[1][3]=m13; m[2][3]=m23; m[3][3]=m33;
			}

    void init();
    float *arr() {return &m[0][0];}
    const dMatrix &operator=(dMatrix const &rhs);
    dMatrix operator+(dMatrix const &rhs);
    dMatrix operator-(dMatrix const &rhs);
    dMatrix operator*(dMatrix const &rhs);
    dMatrix operator/(dMatrix const &rhs);
    dMatrix &operator+=(dMatrix const &rhs);
    dMatrix &operator-=(dMatrix const &rhs);
    dMatrix &operator*=(dMatrix const &rhs);
    dMatrix &operator/=(dMatrix const &rhs);
	dMatrix &translate(dVector &tr);
    dMatrix &translate(float x, float y, float z);
    void    settranslate(dVector &tr);
    dVector gettranslate();
    dMatrix &rotx(float a);
    dMatrix &roty(float a);
    dMatrix &rotz(float a);
    dMatrix &rotxyz(float x,float y,float z);
    dMatrix &scale(float x, float y, float z);
    dVector transform_no_trans(dVector const &p) const;
    dVector transform(dVector const &p) const;
    dVertex transform(dVertex const &p) const;
	void    transpose();
	dMatrix inverse();
	float   determinant();
	dVector get_hori_i() {return dVector(m[0][0],m[1][0],m[2][0]);}
	dVector get_hori_j() {return dVector(m[0][1],m[1][1],m[2][1]);}
	dVector get_hori_k() {return dVector(m[0][2],m[1][2],m[2][2]);}
	dVector get_vert_i() {return dVector(m[0][0],m[0][1],m[0][2]);}
	dVector get_vert_j() {return dVector(m[1][0],m[1][1],m[1][2]);}
	dVector get_vert_k() {return dVector(m[2][0],m[2][1],m[2][2]);}
	void    remove_scale();
	void    extract_euler(float &x, float &y, float &z);
	void    aim(dVector v, dVector up=dVector(0,0,1));
	void    blend(dMatrix other, float amount);
	
	void load_glmatrix(float glm[16]);
	void load_dMatrix(float glm[16]);
	
    friend ostream &operator<<(ostream &os, dMatrix const &om);
//private:
    float m[4][4];
};

ostream &operator<<(ostream &os, dMatrix const &om);

class dBoundingBox
{
public:
	dBoundingBox() : m_Empty(true) {}
	virtual ~dBoundingBox() {}
	
	bool empty() { return m_Empty; }
	void expand(dVector v);
	void expand(dBoundingBox v);
	void expandby(float a);
	bool inside(dVector point) const;
	
	dVector min;
	dVector max;
	
private:
	bool m_Empty;
};

class dQuat
{
public:
	dQuat():x(0),y(0),z(0),w(1){}
	dQuat(float x, float y, float z, float w):x(x),y(y),z(z),w(w){}
	dQuat(const dQuat& q):x(q.x),y(q.y),z(q.z),w(q.w){}
	
	// conversions
	dMatrix toMatrix() const;
	
	// operations
	dQuat conjugate() const;
	void setaxisangle(dVector axis, float angle);
	
	// make multiply look like multiply
	dQuat operator* (const dQuat&qR) const;
	
	void renorm();
	float *arr() {return &x;}
	
	// the data
	float x,y,z,w;
};

////



#endif // DADA
