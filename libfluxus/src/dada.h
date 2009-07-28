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
// A collection of classes for 3D graphics.

#ifndef DADA
#define DADA

#include <cstring>
#include <math.h>
#include <iostream>
#include "Trace.h"

using namespace std;

namespace Fluxus
{

static const float TWO_PI=3.141592654*2.0f;
static const float DEG_CONV = 0.017453292;
static const float RAD_CONV = 1/0.017453292;

inline void debug(char *s) {Trace::Stream<<"dada debug: "<<s<<endl;}

void  InitDada();
float RandFloat();
float RandRange(float L, float H);
void  dSinCos(float a, float &s, float &c);

template<class T> T clamp(T x, T mi, T ma) 
{
    return std::min( ma, std::max( mi, x));
}

class dVector
{
public:
		float x,y,z,w;
		dVector() { x=y=z=0; w=1; }
		dVector(float X, float Y, float Z, float W=1) { x=X; y=Y; z=Z; w=W; }
		dVector(dVector const &c) { *this=c; }
		
		float *arr() { return &x; }
		int operator==(dVector const &rhs) { return (x==rhs.x&&y==rhs.y&&z==rhs.z); }
		
		inline dVector &operator=(dVector const &rhs)
		{
    		x=rhs.x; y=rhs.y; z=rhs.z; w=rhs.w;
    		return *this;
		}

		inline dVector operator+(dVector const &rhs) const
		{
    		dVector t;
    		t.x=x+rhs.x; t.y=y+rhs.y; t.z=z+rhs.z; //t.w=w+rhs.w;
    		return t;
		}

		inline dVector operator-(dVector const &rhs) const
		{
    		dVector t;
    		t.x=x-rhs.x; t.y=y-rhs.y; t.z=z-rhs.z; //t.w=w-rhs.w;
    		return t;
		}

		inline dVector operator*(dVector const &rhs) const
		{
    		dVector t;
    		t.x=x*rhs.x; t.y=y*rhs.y; t.z=z*rhs.z; //t.w=w+rhs.w;
    		return t;
		}

		inline dVector operator/(dVector const &rhs) const
		{
    		dVector t;
    		t.x=x/rhs.x; t.y=y/rhs.y; t.z=z/rhs.z; //t.w=w-rhs.w;
    		return t;
		}

		inline dVector operator+(float rhs) const
		{
    		dVector t;
    		t.x=x+rhs; t.y=y+rhs; t.z=z+rhs; //t.w=w*rhs;
    		return t;
		}

		inline dVector operator-(float rhs) const
		{
    		dVector t;
    		t.x=x-rhs; t.y=y-rhs; t.z=z-rhs; //t.w=w/rhs;
    		return t;
		}

		inline dVector operator*(float rhs) const
		{
    		dVector t;
    		t.x=x*rhs; t.y=y*rhs; t.z=z*rhs; //t.w=w*rhs;
    		return t;
		}

		inline dVector operator/(float rhs) const
		{
    		dVector t;
    		t.x=x/rhs; t.y=y/rhs; t.z=z/rhs; //t.w=w/rhs;
    		return t;
		}

		inline dVector &operator+=(dVector const &rhs)
		{
    		x+=rhs.x; y+=rhs.y; z+=rhs.z; //w+=rhs.w;
    		return *this;
		}

		inline dVector &operator-=(dVector const &rhs)
		{
    		x-=rhs.x; y-=rhs.y; z-=rhs.z; //w-=rhs.w;
    		return *this;
		}

		inline dVector &operator*=(float rhs)
		{
    		x*=rhs; y*=rhs; z*=rhs; //w*=rhs;
    		return *this;
		}

		inline dVector &operator/=(float rhs)
		{
    		if (rhs) {x/=rhs; y/=rhs; z/=rhs;}// w/=rhs;}
    		return *this;
		}

		inline float dot(dVector const &rhs) const
		{
    		return x*rhs.x+y*rhs.y+z*rhs.z;
		}

		inline dVector cross(dVector const &rhs) const
		{
    		return dVector(y*rhs.z - z*rhs.y,
                    		  z*rhs.x - x*rhs.z,
                    		  x*rhs.y - y*rhs.x);
		}

		inline dVector reflect(dVector const &rhs) const
		{
			float vdn=dot(rhs)*2.0f;
    		return (*this)-rhs*vdn;
		}

		inline float dist(dVector const &rhs) const
		{
    		return sqrt((rhs.x-x)*(rhs.x-x)+
                		(rhs.y-y)*(rhs.y-y)+
                		(rhs.z-z)*(rhs.z-z));
		}

		inline float distsq(dVector const &rhs) const
		{
    		return (rhs.x-x)*(rhs.x-x)+
        		   (rhs.y-y)*(rhs.y-y)+
        		   (rhs.z-z)*(rhs.z-z);
		}

		inline void get_euler(float &rx, float &ry, float &rz) const
		{
			if (z==0) rx=0;
			else rx=atan(y/z)*RAD_CONV;
			if (x==0) ry=0;
			else ry=atan(z/x)*RAD_CONV;
			if (y==0) rz=0;
			else rz=atan(x/y)*RAD_CONV;
		}

		inline float mag()
		{
    		return dist(dVector(0,0,0));
		}
	
		inline bool feq(const dVector &other, float epsilon=0.001)
		{
			return (fabs(x-other.x)<epsilon && fabs(y-other.y)<epsilon && fabs(z-other.z)<epsilon);
		}

		inline void homog() { if (w && w!=1.0) { x/=w; y/=w; z/=w; w=1; } }
		inline dVector &normalise() { *this/=mag(); return *this; }
		void get_rot(float m[16],dVector up); // legacy func 
private:
};

dVector operator-(dVector rhs);
ostream &operator<<(ostream &os, dVector const &om);
istream &operator>>(istream &is, dVector &om);

// linear interpolation between two vectors. 
// t=0.0 => lhs
// t=1.0 => rhs
inline dVector lerp(const dVector& lhs, const dVector& rhs, float t)
{
    return lhs + (rhs - lhs) * t;
}

////

// colour modes
enum COLOUR_MODE
{
	MODE_RGB = 0,
	MODE_HSV
};

class dColour
{
public:
		float r,g,b,a;
		dColour() {r=g=b=0; a=1;}
		dColour(float x, float y, float z, float w=1, COLOUR_MODE mode=MODE_RGB);
		dColour(float *xyzw, COLOUR_MODE mode=MODE_RGB);
		dColour(float G, float A=1) {r=g=b=G; a=A;}
		dColour(dColour const &c) {*this=c;}

		float *arr() { return &r; }

		inline dColour &operator=(dColour const &rhs)
		{
			r=rhs.r; g=rhs.g; b=rhs.b; a=rhs.a;
			return *this;
		}

		inline dColour operator+(dColour const &rhs) const
		{
			dColour t;
			t.r=r+rhs.r; t.g=g+rhs.g; t.b=b+rhs.b; t.a=a+rhs.a;
			return t;
		}

		inline dColour operator-(dColour const &rhs) const
		{
			dColour t;
			t.r=r-rhs.r; t.g=g-rhs.g; t.b=b-rhs.b; t.a=a-rhs.a;
			return t;
		}

		inline dColour operator*(dColour const &rhs) const
		{
			dColour t;
			t.r=r*rhs.r; t.g=g*rhs.g; t.b=b*rhs.b; t.a=a*rhs.a;
			return t;
		}

		inline dColour operator/(dColour const &rhs) const
		{
			dColour t;
			t.r=r/rhs.r; t.g=g/rhs.g; t.b=b/rhs.b; t.a=a/rhs.a;
			return t;
		}

		inline dColour operator+(float rhs) const
		{
			dColour t;
			t.r=r+rhs; t.g=g+rhs; t.b=b+rhs; t.a=a+rhs;
			return t;
		}

		inline dColour operator-(float rhs) const
		{
			dColour t;
			t.r=r-rhs; t.g=g-rhs; t.b=b-rhs; t.a=a-rhs;
			return t;
		}

		inline dColour operator*(float rhs) const
		{
			dColour t;
			t.r=r*rhs; t.g=g*rhs; t.b=b*rhs; t.a=a*rhs;
			return t;
		}

		inline dColour operator/(float rhs) const
		{
			dColour t;
			t.r=r/rhs; t.g=g/rhs; t.b=b/rhs; t.a=a/rhs;
			return t;
		}

		inline dColour &operator+=(dColour const &rhs)
		{
			r+=rhs.r; g+=rhs.g; b+=rhs.b; a+=rhs.a;
			return *this;
		}

		inline dColour &operator-=(dColour const &rhs)
		{
			r-=rhs.r; g-=rhs.g; b-=rhs.b; a-=rhs.a;
			return *this;
		}

		inline dColour &operator*=(float rhs)
		{
			r*=rhs; g*=rhs; b*=rhs; a*=rhs;
			return *this;
		}

		inline dColour &operator/=(float rhs)
		{
			if (rhs) {r/=rhs; g/=rhs; b/=rhs; a/=rhs;}
			return *this;
		}

		inline void clamp()
		{
			if (r<0) r=0; if (g<0) g=0; if (b<0) b=0; if (a<0) a=0;
			if (r>1) r=1; if (g>1) g=1; if (b>1) b=1; if (a>1) a=1;
		}

		static void RGBtoHSV(float r, float g, float b, float *hsv);
		static void HSVtoRGB(float h, float s, float v, float *rgb);

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
    dMatrix() { init(); }
    dMatrix(const dMatrix &other) { (*this)=other; }

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
	
    inline float *arr() { return &m[0][0]; }

	inline void init()
	{
		zero();
		m[0][0]=m[1][1]=m[2][2]=m[3][3]=1;
	}

	inline void zero()
	{
		memset(m,0,sizeof(float)*16);
	}

	inline dVector get_hori_i() const {return dVector(m[0][0],m[1][0],m[2][0]);}
	inline dVector get_hori_j() const {return dVector(m[0][1],m[1][1],m[2][1]);}
	inline dVector get_hori_k() const {return dVector(m[0][2],m[1][2],m[2][2]);}
	inline dVector get_vert_i() const {return dVector(m[0][0],m[0][1],m[0][2]);}
	inline dVector get_vert_j() const {return dVector(m[1][0],m[1][1],m[1][2]);}
	inline dVector get_vert_k() const {return dVector(m[2][0],m[2][1],m[2][2]);}
	
    inline void set_hori_i(const dVector& v) { m[0][0] = v.x; m[1][0] = v.y; m[2][0] = v.z; }
    inline void set_hori_j(const dVector& v) { m[0][1] = v.x; m[1][1] = v.y; m[2][1] = v.z; }
    inline void set_hori_k(const dVector& v) { m[0][2] = v.x; m[1][2] = v.y; m[2][2] = v.z; }
    inline void set_vert_i(const dVector& v) { m[0][0] = v.x; m[0][1] = v.y; m[0][2] = v.z; }
    inline void set_vert_j(const dVector& v) { m[1][0] = v.x; m[1][1] = v.y; m[1][2] = v.z; }
    inline void set_vert_k(const dVector& v) { m[2][0] = v.x; m[2][1] = v.y; m[2][2] = v.z; }

	inline const dMatrix &operator=(dMatrix const &rhs)
	{
		m[0][0]=rhs.m[0][0]; m[0][1]=rhs.m[0][1]; m[0][2]=rhs.m[0][2]; m[0][3]=rhs.m[0][3];
		m[1][0]=rhs.m[1][0]; m[1][1]=rhs.m[1][1]; m[1][2]=rhs.m[1][2]; m[1][3]=rhs.m[1][3];
		m[2][0]=rhs.m[2][0]; m[2][1]=rhs.m[2][1]; m[2][2]=rhs.m[2][2]; m[2][3]=rhs.m[2][3];
		m[3][0]=rhs.m[3][0]; m[3][1]=rhs.m[3][1]; m[3][2]=rhs.m[3][2]; m[3][3]=rhs.m[3][3];
		return rhs;
	}

	inline dMatrix operator+(dMatrix const &rhs) const
	{
    	dMatrix t;
    	for (int i=0; i<4; i++)
		{
        	for (int j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]+rhs.m[i][j];
			}
		}
    	return t;
	}

	inline dMatrix operator-(dMatrix const &rhs) const
	{
    	dMatrix t;
    	for (int i=0; i<4; i++)
		{
        	for (int j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]-rhs.m[i][j];
			}
		}
    	return t;
	}

	inline dMatrix operator*(dMatrix const &rhs) const
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

	inline dMatrix operator/(dMatrix const &rhs) const
	{
    	dMatrix t;
    	for (int i=0; i<4; i++)
		{
        	for (int j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][0]/rhs.m[0][j]+
                    	  m[i][1]/rhs.m[1][j]+
                    	  m[i][2]/rhs.m[2][j]+
                    	  m[i][3]/rhs.m[3][j];
			}
		}
    	return t;
	}

	inline dMatrix operator+(float rhs) const
	{
		dMatrix t;
    	for (int i=0; i<4; i++)
		{
        	for (int j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]+rhs;
			}
		}
    	return t;
	}

	inline dMatrix operator-(float rhs) const
	{
		dMatrix t;
    	for (int i=0; i<4; i++)
		{
        	for (int j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]-rhs;
			}
		}
    	return t;
	}

	inline dMatrix operator*(float rhs) const
	{
		dMatrix t;
    	for (int i=0; i<4; i++)
		{
        	for (int j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]*rhs;
			}
		}
    	return t;
	}

	inline dMatrix operator/(float rhs) const
	{
		dMatrix t;
    	for (int i=0; i<4; i++)
		{
        	for (int j=0; j<4; j++)
			{
            	t.m[i][j]=m[i][j]/rhs;
			}
		}
    	return t;
	}

	inline dMatrix &operator+=(dMatrix const &rhs)
	{
    	for (int i=0; i<4; i++)
		{
        	for (int j=0; j<4; j++)
			{
            	m[i][j]+=rhs.m[i][j];
			}
		}
    	return *this;
	}

	inline dMatrix &operator-=(dMatrix const &rhs)
	{
    	for (int i=0; i<4; i++)
		{
        	for (int j=0; j<4; j++)
			{
            	m[i][j]-=rhs.m[i][j];
			}
		}
    	return *this;
	}

	inline dMatrix &operator*=(dMatrix const &rhs)
	{
    	*this=*this*rhs;
    	return *this;
	}

	inline dMatrix &operator/=(dMatrix const &rhs)
	{
    	*this=*this/rhs;
    	return *this;
	}

	inline dMatrix &translate(float x, float y, float z)
	{
    	dMatrix t;
    	t.m[3][0]=x;
    	t.m[3][1]=y;
    	t.m[3][2]=z;
    	*this=*this*t;
    	return *this;

	}

	inline dMatrix &translate(dVector &tr)
	{
    	dMatrix t;
    	t.m[3][0]=tr.x;
    	t.m[3][1]=tr.y;
    	t.m[3][2]=tr.z;
    	*this=*this*t;
    	return *this;

	}

	inline void settranslate(const dVector &tr)
	{
    	m[3][0]=tr.x;
    	m[3][1]=tr.y;
    	m[3][2]=tr.z;
	}

	inline dVector gettranslate() const
	{
    	return dVector(m[3][0],m[3][1],m[3][2]);
	}

	//#define USE_FAST_SINCOS

	inline dMatrix &rotxyz(float x,float y,float z)
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
	    	t.m[2][0]=sy;
			t.m[0][2]=-sy;
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

	inline dMatrix &rotx(float a)
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

	inline dMatrix &roty(float a)
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

	inline dMatrix &rotz(float a)
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

	inline dMatrix &scale(float x, float y, float z)
	{
    	dMatrix t;

    	t.m[0][0]=x;
    	t.m[1][1]=y;
		t.m[2][2]=z;

    	*this=*this*t;
    	return *this;
	}

    inline dMatrix& scale(const dVector& s)
    {
        return scale(s.x, s.y, s.z);
    }

	inline dVector transform(dVector const &p) const
	{
    	dVector t;
    	t.x=p.x*m[0][0] + p.y*m[1][0] + p.z*m[2][0] + p.w*m[3][0];
    	t.y=p.x*m[0][1] + p.y*m[1][1] + p.z*m[2][1] + p.w*m[3][1];
    	t.z=p.x*m[0][2] + p.y*m[1][2] + p.z*m[2][2] + p.w*m[3][2];
    	t.w=p.x*m[0][3] + p.y*m[1][3] + p.z*m[2][3] + p.w*m[3][3];
    	return t;
	}

	inline dVector transform_persp(dVector const &p) const
	{
    	dVector t;
    	t.x=p.x*m[0][0] + p.y*m[1][0] + p.z*m[2][0] + p.w*m[3][0];
    	t.y=p.x*m[0][1] + p.y*m[1][1] + p.z*m[2][1] + p.w*m[3][1];
    	t.z=p.x*m[0][2] + p.y*m[1][2] + p.z*m[2][2] + p.w*m[3][2];
    	t.w=p.x*m[0][3] + p.y*m[1][3] + p.z*m[2][3] + p.w*m[3][3];
		t.homog();
    	return t;
	}

	inline dVertex transform(dVertex const &p) const
	{
    	dVertex t=p;
    	t.point=transform(p.point);
    	t.normal=transform_no_trans(p.normal);
    	return t;
	}

	inline dVector transform_no_trans(dVector const &p) const
	{
    	dVector t;
    	t.x=p.x*m[0][0] + p.y*m[1][0] + p.z*m[2][0];
    	t.y=p.x*m[0][1] + p.y*m[1][1] + p.z*m[2][1];
    	t.z=p.x*m[0][2] + p.y*m[1][2] + p.z*m[2][2];
    	t.w=p.w;
    	return t;
	}

	/*void load_glmatrix(float glm[16])
	{
		glm[0]= m[0][0]; glm[1]= m[1][0]; glm[2]= m[2][0]; glm[3]= m[3][0];
		glm[4]= m[0][1]; glm[5]= m[1][1]; glm[6]= m[2][1]; glm[7]= m[3][1];
		glm[8]= m[0][2]; glm[9]= m[1][2]; glm[10]=m[2][2]; glm[11]=m[3][2];
		glm[12]=m[0][3]; glm[13]=m[1][3]; glm[14]=m[2][3]; glm[15]=m[3][3];
	}*/

	inline void load_glmatrix(float glm[16])
	{
		glm[0]= m[0][0]; glm[4]= m[1][0]; glm[8]= m[2][0]; glm[12]= m[3][0];
		glm[1]= m[0][1]; glm[5]= m[1][1]; glm[9]= m[2][1]; glm[13]= m[3][1];
		glm[2]= m[0][2]; glm[6]= m[1][2]; glm[10]=m[2][2]; glm[14]=m[3][2];
		glm[3]= m[0][3]; glm[7]= m[1][3]; glm[11]=m[2][3]; glm[15]=m[3][3];
	}

	inline void load_dMatrix(float glm[16])
	{
		m[0][0]=glm[0]; m[1][0]=glm[4]; m[2][0]=glm[8]; m[3][0]=glm[12];
		m[0][1]=glm[1]; m[1][1]=glm[5]; m[2][1]=glm[9]; m[3][1]=glm[13];
		m[0][2]=glm[2]; m[1][2]=glm[6]; m[2][2]=glm[10]; m[3][2]=glm[14];
		m[0][3]=glm[3]; m[1][3]=glm[7]; m[2][3]=glm[11]; m[3][3]=glm[15];
	}

	inline dMatrix getTranspose() const
	{
		dMatrix t;
		for (int i=0; i<4; i++)
		{
        	for (int j=0; j<4; j++)
			{
            	t.m[i][j]=m[j][i];
			}
		}
    	return t;
	}

    inline void transpose()
    {
        *this = getTranspose();
    }

	inline dMatrix inverse() const
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

	inline float determinant()  const
	{
	   return 
	   m[0][3] * m[1][2] * m[2][1] * m[3][0]-m[0][2] * m[1][3] * m[2][1] * m[3][0]-m[0][3] * m[1][1] * m[2][2] * m[3][0]+m[0][1] * m[1][3] * m[2][2] * m[3][0]+
	   m[0][2] * m[1][1] * m[2][3] * m[3][0]-m[0][1] * m[1][2] * m[2][3] * m[3][0]-m[0][3] * m[1][2] * m[2][0] * m[3][1]+m[0][2] * m[1][3] * m[2][0] * m[3][1]+
	   m[0][3] * m[1][0] * m[2][2] * m[3][1]-m[0][0] * m[1][3] * m[2][2] * m[3][1]-m[0][2] * m[1][0] * m[2][3] * m[3][1]+m[0][0] * m[1][2] * m[2][3] * m[3][1]+
	   m[0][3] * m[1][1] * m[2][0] * m[3][2]-m[0][1] * m[1][3] * m[2][0] * m[3][2]-m[0][3] * m[1][0] * m[2][1] * m[3][2]+m[0][0] * m[1][3] * m[2][1] * m[3][2]+
	   m[0][1] * m[1][0] * m[2][3] * m[3][2]-m[0][0] * m[1][1] * m[2][3] * m[3][2]-m[0][2] * m[1][1] * m[2][0] * m[3][3]+m[0][1] * m[1][2] * m[2][0] * m[3][3]+
	   m[0][2] * m[1][0] * m[2][1] * m[3][3]-m[0][0] * m[1][2] * m[2][1] * m[3][3]-m[0][1] * m[1][0] * m[2][2] * m[3][3]+m[0][0] * m[1][1] * m[2][2] * m[3][3];
	}

	inline void remove_scale()
	{
		set_hori_i( get_hori_i().normalise() );
		set_hori_j( get_hori_j().normalise() );
		set_hori_k( get_hori_k().normalise() );
	}

    inline dVector get_scale() const 
    {
        return dVector(
                get_hori_i().mag(),
                get_hori_j().mag(),
                get_hori_k().mag());
    }

	inline void extract_euler(float &x, float &y, float &z) const
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
		Trace::Stream<<x<<" "<<d1<<" "<<d2<<endl;
		yvec.get_euler(d1,y,d2);
		Trace::Stream<<d1<<" "<<y<<" "<<d2<<endl;
		zvec.get_euler(d1,d2,z);
		Trace::Stream<<d1<<" "<<d2<<" "<<z<<endl;*/
	}

	inline void aim(dVector v, const dVector& up)
	{
		v.normalise();
		dVector l=v.cross(up);
		dVector u=v.cross(l);
		l.normalise();
		u.normalise();

		m[0][0]=v.x; m[0][1]=v.y; m[0][2]=v.z;
		m[1][0]=l.x; m[1][1]=l.y; m[1][2]=l.z;
		m[2][0]=u.x; m[2][1]=u.y; m[2][2]=u.z;	
	}

	inline void blend(const dMatrix& other, float amount)
	{
		for (int j=0; j<4; j++)
		{
        	for (int i=0; i<4; i++)
			{
            	m[i][j]=(1-amount)*m[i][j]+amount*other.m[i][j];
			}
		}
	}

    void RigidBlend(const dMatrix& other, float amount);
	
    friend ostream &operator<<(ostream &os, dMatrix const &om);

    float m[4][4];
};

ostream &operator<<(ostream &os, dMatrix const &om);

class dPlane
{
public:
	float a;
	float b;
	float c;
	float d;
	
	float pointdistance(const dVector &p) const
	{
	    return a*p.x + b*p.y + c*p.z + d;
	}
	
	void normalise()
	{	
	    float mag;
	    mag=sqrt(a * a + b * b + c * c);
	    a=a/mag;
	    b=b/mag;
	    c=c/mag;
	    d=d/mag;
	}
};

class dBoundingBox
{
public:
	dBoundingBox() : m_Empty(true) {}
	dBoundingBox(const dVector &cmin, const dVector &cmax) : min(cmin), max(cmax) {}
	virtual ~dBoundingBox() {}
	
	bool empty() { return m_Empty; }
	void getvertices(dVector *out) const;
	void expand(dVector v);
	void expand(dBoundingBox v);
	void expandby(float a);
	bool inside(const dVector &point, float threshold=0) const;
	bool inside(const dBoundingBox &other, float threshold=0) const;
	bool inside(const dPlane &plane, float threshold=0) const;
	
	
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

    dQuat& operator=(const dQuat& q) 
    { x = q.x; y = q.y; z = q.z; w = q.w; return *this; }
	
	// conversions
	dMatrix toMatrix() const;
    dQuat(const dMatrix& m); // from matrix
    void toAxisAngle(dVector& axis, float& angle) const;
	
	// operations
	dQuat conjugate() const;
	void setAxisAngle(dVector axis, float angle);
    float dot(const dQuat& q) const 
    { return x*q.x + y*q.y + z*q.x + w*q.w; }
	
	// make multiply look like multiply
	dQuat operator* (const dQuat&qR) const;
    inline dQuat operator+ (const dQuat& q) const 
    { return dQuat(x+q.x, y+q.y, z+q.z, w+q.w); }
    inline dQuat operator- (const dQuat& q) const 
    { return dQuat(x-q.x, y-q.y, z-q.z, w-q.w); }
	inline dQuat operator* (float a) const
    { return dQuat(a*x, a*y, a*z, a*w); }
	
	void renorm();
    inline dQuat getNormlised() const
    {
        dQuat res = *this;
        res.renorm();
        return res;
    }

	float *arr() {return &x;}
	
	// the data
	float x,y,z,w;
};

dQuat slerp(const dQuat& from, const dQuat& to, float t);

inline float dot(const dQuat& a, const dQuat& b) { return a.dot(b); }
inline dQuat operator*(float a, const dQuat& q) { return q * a; }


////

}

#endif // DADA
