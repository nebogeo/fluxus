// Copyright (C) 2008 Gabor Papp
//
// Perlin Noise based on the code by Karsten Schmidt
// http://toxiclibs.googlecode.com/svn/trunk/toxiclibs/src.geom/toxi/math/noise/PerlinNoise.java
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

#include <stdlib.h>
#include <math.h>
#include "Noise.h"

using namespace Fluxus;

bool Noise::inited = false;
float Noise::perlin[PERLIN_SIZE];
int Noise::perlin_octaves = 4;
float Noise::perlin_amp_falloff = 0.5;
unsigned Noise::perlin_seed = 1;

/**
 * Sets the random seed and reinitialises the random numbers that are used to
 * generete the Perlin noise.
 * \param seed random seed
 **/
void Noise::noise_seed(unsigned seed)
{
	perlin_seed = seed;
	inited = false;
}

/**
 * Adjusts the character and level of detail produced by the Perlin noise
 * function.
 * \param octaves number of octaves to be used by the noise() function
 * \param falloff falloff factor for each octave
 **/
void Noise::noise_detail(int octaves, float falloff /* = 0 */)
{
	if (octaves > 0)
		perlin_octaves = octaves;
	if (falloff > 0.0)
		perlin_amp_falloff = falloff;
}
/**
 * Returns the Perlin noise value at specified coordinates.
 * \param x x coordinate in noise space
 * \param y y coordinate in noise space
 * \param z z coordinate in noise space
 * \retval float Perlin noise
 **/
float Noise::noise(float x, float y /* = 0*/, float z /* = 0 */)
{
	if (!inited)
	{
		srand(perlin_seed);
		for (int i = 0; i < PERLIN_SIZE; i++)
		{
			perlin[i] = (float)rand() / (float)RAND_MAX;
		}
		inited = true;
	}

	if (x < 0.0)
		x = -x;
	if (y < 0.0)
		y = -y;
	if (z < 0.0)
		z = -z;

	int xi = (int)x;
	int yi = (int)y;
	int zi = (int)z;
	float xf = x - (float)xi;
	float yf = y - (float)yi;
	float zf = z - (float)zi;
	float rxf, ryf;

	float r = 0.0;
	float amp = 0.5;

	for (int i1 = 0; i1 < perlin_octaves; i1++)
	{
		int of = xi + (yi << PERLIN_YWRAPB) + (zi << PERLIN_ZWRAPB);

		float rxf = .5 * (1 - cos(xf * M_PI));
		float ryf = .5 * (1 - cos(yf * M_PI));

		float n1 = perlin[of & (PERLIN_SIZE - 1)];
		n1 += rxf * (perlin[(of + 1) & (PERLIN_SIZE - 1)] - n1);
		float n2 = perlin[(of + PERLIN_YWRAP) & (PERLIN_SIZE - 1)];
		n2 += rxf * (perlin[(of + PERLIN_YWRAP + 1) & (PERLIN_SIZE - 1)] - n2);
		n1 += ryf * (n2 - n1);

		of += PERLIN_ZWRAP;
		n2 = perlin[of & (PERLIN_SIZE - 1)];
		n2 += rxf * (perlin[(of + 1) & (PERLIN_SIZE - 1)] - n2);
		float n3 = perlin[(of + PERLIN_YWRAP) & (PERLIN_SIZE - 1)];
		n3 += rxf * (perlin[(of + PERLIN_YWRAP + 1) & (PERLIN_SIZE - 1)] - n3);
		n2 += ryf * (n3 - n2);

		n1 += .5 * (1 - cos(zf * M_PI)) * (n2 - n1);

		r += n1 * amp;
		amp *= perlin_amp_falloff;

		if (amp < PERLIN_MIN_AMPLITUDE)
			break;

		xi <<= 1;
		xf *= 2.0;
		yi <<= 1;
		yf *= 2.0;
		zi <<= 1;
		zf *= 2.0;

		if (xf >= 1.0)
		{
			xi++;
			xf--;
		}
		if (yf >= 1.0)
		{
			yi++;
			yf--;
		}
		if (zf >= 1.0)
		{
			zi++;
			zf--;
		}
	}

	return r;
}

