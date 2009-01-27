#ifndef N_NOISE
#define N_NOISE

namespace Fluxus
{

class Noise
{
	public:
		static void noise_detail(int octaves, float falloff = 0);
		static void noise_seed(unsigned seed);

		static float noise(float x, float y = 0, float z = 0);

	private:

		static const int PERLIN_YWRAPB = 4;
		static const int PERLIN_YWRAP = 1 << PERLIN_YWRAPB;
		static const int PERLIN_ZWRAPB = 8;
		static const int PERLIN_ZWRAP = 1 << PERLIN_ZWRAPB;
		static const int PERLIN_SIZE = PERLIN_YWRAP * PERLIN_ZWRAP;
		static const float PERLIN_MIN_AMPLITUDE = 0.001f;

		static bool inited;
		static float perlin[4096];
		static int perlin_octaves;
		static float perlin_amp_falloff;
		static unsigned perlin_seed;
};

}

#endif /* N_NOISE */
