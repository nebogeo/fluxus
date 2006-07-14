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

#include <fstream>
#include <deque>
#include <libguile.h>
#include "FluxusAudioBinding.h"
#include "SearchPaths.h"
#include "Repl.h"
#include <LinePrimitive.h>
#include <ParticlePrimitive.h>
#include <PixelPrimitive.h>
#include <LocatorPrimitive.h>

void FluxusAudioBinding::RegisterProcs()
{
	SchemePrim::Init();
	
	scm_c_define_gsubr("start-audio",3,0,0,(CALLBACK_CAST)  start_audio);	 
	scm_c_define_gsubr("smoothing-bias",1,0,0,(CALLBACK_CAST) smoothing_bias);   
	scm_c_define_gsubr("gain",1,0,0,(CALLBACK_CAST) 		gain);   
	scm_c_define_gsubr("get-harmonic",1,0,0,(CALLBACK_CAST) get_harmonic);
	scm_c_define_gsubr("gh", 1,0,0,(CALLBACK_CAST)  		get_harmonic);
	scm_c_define_gsubr("process",1,0,0,(CALLBACK_CAST)  	 process);

}


SCM FluxusAudioBinding::start_audio(SCM s_dev, SCM s_bs, SCM s_sr)
{
	SCM_ASSERT(scm_is_string(s_dev), s_dev, SCM_ARG1, "start-audio");
	SCM_ASSERT(scm_is_number(s_bs), s_bs, SCM_ARG2, "start-audio");
 	SCM_ASSERT(scm_is_number(s_sr), s_sr, SCM_ARG3, "start-audio");
	
	if (Audio==NULL)
	{
		char *name=scm_to_locale_string(s_dev);		
		Audio = new AudioCollector(name,(unsigned int)scm_to_int(s_bs),scm_to_int(s_sr));
		Fluxus->SetAudio(Audio);
		free(name);
	}
	return SCM_UNSPECIFIED;
}

SCM FluxusAudioBinding::get_harmonic(SCM s_harm)
{
	SCM_ASSERT(scm_is_number(s_harm), s_harm, SCM_ARG1, "get_harmonic");	
	if (Audio!=NULL)
	{	
    	return scm_from_double(Audio->GetHarmonic(scm_to_int(s_harm)));
	}
	return scm_from_double(0);
}

SCM FluxusAudioBinding::gain(SCM s_gain)
{
	SCM_ASSERT(scm_is_number(s_gain), s_gain, SCM_ARG1, "gain");	
	if (Audio!=NULL)
	{	
		Audio->SetGain(scm_to_double(s_gain));
	}
    return SCM_UNSPECIFIED;
}


SCM FluxusAudioBinding::process(SCM s_wavname)
{
	SCM_ASSERT(scm_is_string(s_wavname), s_wavname, SCM_ARG1, "process");	
	char *wavname=scm_to_locale_string(s_wavname);
	if (Audio!=NULL)
	{	
		Audio->Process(wavname);
	}
	free(wavname);
    return SCM_UNSPECIFIED;
}

SCM FluxusAudioBinding::smoothing_bias(SCM s_gain)
{
	SCM_ASSERT(scm_is_number(s_gain), s_gain, SCM_ARG1, "smoothing-bias");	
	if (Audio!=NULL)
	{	
		Audio->SetSmoothingBias(scm_to_double(s_gain));
	}
    return SCM_UNSPECIFIED;
}
