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

#include <cstdlib>
#include "dada.h"

using namespace Fluxus;

static const int SINCOS_TABLESIZE = 2048;
static float SinTab[SINCOS_TABLESIZE];
static float CosTab[SINCOS_TABLESIZE];
static const float SINCOS_LOOKUP=SINCOS_TABLESIZE/(float)TWO_PI;

///\todo optimisations! inline, sort out mag/normalise 

float Fluxus::RandFloat()
{
	return rand()%10000/10000.0f;
}

float Fluxus::RandRange(float L, float H)
{
	return ((rand()%10000/10000.0f)*(H-L))+L;
}

void Fluxus::InitDada()
{
	for (int n=0; n<SINCOS_TABLESIZE; n++)
	{
		float a=n*(TWO_PI/(float)SINCOS_TABLESIZE);
		SinTab[n]=sin(a);
		CosTab[n]=cos(a);
	}
}

void Fluxus::dSinCos(float a, float &s, float &c)
{
	int Index=(int)rint(a*SINCOS_LOOKUP)&SINCOS_TABLESIZE-1;
	s=SinTab[Index];
	c=CosTab[Index];	
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
	{
		m[n]=0;
	}

	m[15]=1;

	m[0]=a.x; m[1]=a.y;	m[2]=a.z;	
	m[4]=b.x; m[5]=b.y;	m[6]=b.z;	
	m[8]=c.x; m[9]=c.y;	m[10]=c.z;
}

dVector Fluxus::operator-(dVector rhs)
{
    return dVector(-rhs.x,-rhs.y,-rhs.z);
}

ostream &Fluxus::operator<<(ostream &os, dVector const &om)
{
    os<<om.x<<" "<<om.y<<" "<<om.z<<" "<<om.w<<" ";
    return os;
}

istream &Fluxus::operator>>(istream &is, dVector &om)
{
    is>>om.x>>om.y>>om.z>>om.w;
    return is;
}

////

dColour::dColour(float x, float y, float z, float w/*=1*/, COLOUR_MODE mode/*=MODE_RGB*/)
{
	if (mode == MODE_RGB)
	{
		r = x; g = y; b = z; a = w;
	}
	else
	{
		HSVtoRGB(x, y, z, arr());
		a = w;
	}
}

dColour::dColour(float *xyzw, COLOUR_MODE mode/*=MODE_RGB*/)
{
	if (mode == MODE_RGB)
	{
		r = xyzw[0]; g = xyzw[1]; b = xyzw[2]; a = xyzw[3];
	}
	else
	{
		HSVtoRGB(xyzw[0], xyzw[1], xyzw[2], arr());
		a = xyzw[3];
	}
}

void dColour::RGBtoHSV(float r, float g, float b, float *hsv)
{
	float h, s, v;

	float rgbmax = (r > g) ?
					((b > r) ? b : r) :
					((b > g) ? b : g);
	float rgbmin = (r < g) ?
					((b < r) ? b : r) :
					((b < g) ? b : g);
	float delta = rgbmax - rgbmin;

	v = rgbmax;

	if (rgbmax == 0)
	{
		hsv[0] = hsv[1] = hsv[2] = 0;
		return;
	}

	s = delta / rgbmax;

	if (s == 0)
		h = 0;
	else
	if (r == rgbmax)
		h = (g - b) / delta;
	else
	if (g == rgbmax)
		h = 2 + (b - r) / delta;
	else
		h = 4 + (r - g) / delta;

	if (h < 0)
		h += 6;

	h /= 6;

	hsv[0] = h;
	hsv[1] = s;
	hsv[2] = v;
}

void dColour::HSVtoRGB(float h, float s, float v, float *rgb)
{
	if (h >= 1) h = 0; // 360 degrees same as 0 degrees
	if (s > 1) s = 1;
	if (v > 1) v = 1;
	if (h < 0) h = 0;
	if (s < 0) s = 0;
	if (v < 0) v = 0;

	float h6 = h * 6;
	int i = (int)floor(h6);
	float f = h6 - i;

	float p = v * (1 - s);
	float q = v * (1 - f * s);
	float t = v * (1 - (1 - f) * s);

	switch (i)
	{
		case 0:
			rgb[0] = v; rgb[1] = t; rgb[2] = p;
			break;
		case 1:
			rgb[0] = q; rgb[1] = v; rgb[2] = p;
			break;
		case 2:
			rgb[0] = p; rgb[1] = v; rgb[2] = t;
			break;
		case 3:
			rgb[0] = p; rgb[1] = q; rgb[2] = v;
			break;
		case 4:
			rgb[0] = t; rgb[1] = p; rgb[2] = v;
			break;
		case 5:
			rgb[0] = v; rgb[1] = p; rgb[2] = q;
			break;
	}
}

ostream &Fluxus::operator<<(ostream &os, dColour const &om)
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

ostream &Fluxus::operator<<(ostream &os, dVertex const &v)
{
    os<<"Vertex : p="<<v.point<<" n="<<v.normal<<v.col<<" "<<v.s<<" "<<v.t<<endl;
    return os;
}

////

void dMatrix::RigidBlend(const dMatrix& other, float amount)
{
    dQuat qa = dQuat(*this), qb = dQuat(other);
    qa.renorm();
    qb.renorm();
    dMatrix mat = slerp(qa, qb, amount).toMatrix();
    mat.settranslate( gettranslate() * (1.0-amount) + other.gettranslate() * amount);
    *this = mat;
}

ostream &Fluxus::operator<<(ostream &os, dMatrix const &om)
{
    for (int j=0; j<4; j++)
	{
        for (int i=0; i<4; i++)
		{
            os<<om.m[i][j]<<" ";
		}
		os<<endl;
	}

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

bool dBoundingBox::inside(dVector p, float threshold)
{ 
	return (p.x>min.x-threshold && p.x<max.x+threshold &&
			p.y>min.y-threshold && p.y<max.y+threshold &&
			p.z>min.z-threshold && p.z<max.z+threshold);
}

bool dBoundingBox::inside(dBoundingBox &v, float threshold)
{
	return (v.max.x>min.x-threshold && v.min.x<max.x+threshold &&
	        v.max.y>min.y-threshold && v.min.y<max.y+threshold &&
	        v.max.z>min.z-threshold && v.min.z<max.z+threshold);	
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
	return 
        dMatrix(1.0f - (yy + zz), xy + wz, xz - wy, 0,
			   xy - wz, 1.0f - (xx + zz), yz + wx, 0,
			   xz + wy, yz - wx, 1.0f - (xx + yy), 0, 
               0, 0, 0, 1.0f);
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

void dQuat::setAxisAngle(dVector axis, float angle)
{ 
	angle*=0.017453292;
	w = cos(angle/2);
	axis.normalise();
    axis *= sin(angle/2);
	x=axis.x;
	y=axis.y;
	z=axis.z;
}

dQuat::dQuat(const dMatrix& mat)
{
    float tr, s, q[4];
    int i, j, k;

    int nxt[3] = {1, 2, 0};

    tr = mat.m[0][0] + mat.m[1][1] + mat.m[2][2] + 1.0f;

    if (tr > 0.0) {
        s = sqrt (tr);
        w = s / 2.0;
        s = 0.5 / s;
        x = (mat.m[2][1] - mat.m[1][2]) * s;
        y = (mat.m[0][2] - mat.m[2][0]) * s;
        z = (mat.m[1][0] - mat.m[0][1]) * s;
    } else {		
        i = 0;
        if (mat.m[1][1] > mat.m[0][0]) i = 1;
        if (mat.m[2][2] > mat.m[i][i]) i = 2;
        j = nxt[i];
        k = nxt[j];

        s = sqrt ((mat.m[i][i] - (mat.m[j][j] + mat.m[k][k])) + 1.0);

        q[i] = s * 0.5;

        if (s != 0.0) s = 0.5 / s;

        q[3] = (mat.m[k][j] - mat.m[j][k]) * s;
        q[j] = (mat.m[j][i] + mat.m[i][j]) * s;
        q[k] = (mat.m[k][i] + mat.m[i][k]) * s;

        x = q[0];
        y = q[1];
        z = q[2];
        w = q[3];
    }
}

dQuat Fluxus::slerp(const dQuat& from, const dQuat& to, float t)
{
    float cosa = dot(from,to);

    // too close
    if (cosa > 0.9995) {
        dQuat q = from + t*(to-from);
        q.renorm();
        return q;
    }

    cosa = clamp(cosa, -1.0f, 1.0f);
    float angle = acos(cosa) * t;

    dQuat q = to - from * cosa;
    q.renorm();

    return from * cos(angle) + to * sin(angle);
}


void dQuat::toAxisAngle(dVector& axis, float& angle) const
{
    dQuat n = *this;
    n.renorm();

    float cos_a = n.w;
    angle = acos( cos_a ) * 2.0;

    float sin_a = sqrt( 1.0 - cos_a * cos_a );
    if ( fabs( sin_a ) < 0.0005 ) sin_a = 1.0;
    axis.x = n.x / sin_a;
    axis.y = n.y / sin_a;
    axis.z = n.z / sin_a;
}


