// Copyright (C) 2007 Dave Griffiths
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

#include <escheme.h>
#include "AudioCollector.h"

#undef MZ_GC_DECL_REG
#undef MZ_GC_UNREG
#define MZ_GC_DECL_REG(size) void *__gc_var_stack__[size+2] = { (void*)0, (void*)size };
#define MZ_GC_UNREG() (GC_variable_stack = (void**)__gc_var_stack__[0])

using namespace std;

AudioCollector *Audio = NULL;

// StartSectionDoc-en
// audio
// This part of fluxus is responsible for capturing the incoming sound, and processing it 
// into harmonic data, using fft (Fast Fourier Transform). The harmonics are bands of 
// frequency which the sound is split into, giving some indication of 
// the quality of the sound. It's the same as you see on a graphic equaliser - in 
// fact, one of the example scripts (bars.scm) acts as a graphic equaliser display, 
// and should be used to test the audio is working.
// Example:
// (start-audio "alsa_pcm:capture_1" 1024 44100)
// (define (animate)
//		(colour (vector (gh 1) (gh 2) (gh 3))) ; make a colour from the harmonics, and set it to be the current colour 
//		(draw-cube)) ; draw a cube with this colour
// (every-frame (animate))
// EndSectionDoc

// StartSectionDoc-pt
// audio
// Esta parte do fluxus é responsável por capturar o som entrando, e
// processar ele em dados harmonicos, usando fft (Fast Fourier
// Transform). As harmonicas são bandas de frequência em que o som é
// dividido, dando alguma indicação da qualidade do som. É o mesmo que
// você ve num equalisador gráfico - de fato, um dos scripts de
// exemplo (bars.scm) age como tal, e pode ser usado para testar se o
// áudio está funcionando.
// Exemplo:
// (start-audio "alsa_pcm:capture_1" 1024 44100)
// (define (animate)
//		(colour (vector (gh 1) (gh 2) (gh 3))) ; make a colour from the harmonics, and set it to be the current colour 
//		(draw-cube)) ; draw a cube with this colour
// (every-frame (animate))
// EndSectionDoc


// StartFunctionDoc-en
// start-audio jackport-string buffersize-number samplerate-number
// Returns: void
// Description:
// Starts up the audio with the specified settings, you'll need to call this first, or put it into 
// $HOME/.fluxus.scm to call it automatically at startup. Make the jack port name an empty 
// string and it won't try to connect to anything for you. You can use qjackctrl or equivelent to 
// do the connection manually. Fluxus reads a single mono source.
// Example:
// (start-audio "alsa_pcm:capture_1" 1024 44100)
// EndFunctionDoc

// StartFunctionDoc-pt
// start-audio string-porta-do-jack número-tamanho-buffer número-taxa-amostragem
// Retorna: void
// Descrição:
// Inicia o áudio com as configurações específicadas, você precisa
// chamar isto primeiro, ou colocar isto em $HOME/.fluxus.scm para
// chamar automaticamente na inícialização. Tenha a porta do jack como
// uma string vazia ("") e ele não vai tentar conectar em nada para
// você. Que pode então usar qjackctrl ou equivalente para fazer a
// conexão manualmente. Fluxus lè uma única fonte mono.
// Exemplo:
// (start-audio "alsa_pcm:capture_1" 1024 44100)
// EndFunctionDoc

Scheme_Object *start_audio(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_REG();	
	
	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("start-audio", "string", 0, argc, argv);
	if (!SCHEME_NUMBERP(argv[1])) scheme_wrong_type("start-audio", "number", 1, argc, argv);
	if (!SCHEME_NUMBERP(argv[2])) scheme_wrong_type("start-audio", "number", 2, argc, argv);
	
	if (Audio==NULL)
	{
		char *name = scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
		Audio = new AudioCollector(name,(unsigned int)scheme_real_to_double(argv[1]),(int)scheme_real_to_double(argv[2]));
	}
	
	MZ_GC_UNREG(); 
	return scheme_void;
}

// StartFunctionDoc-en
// gh harmonic-number
// Returns: harmonic-real
// Description:
// Fluxus converts incoming audio into harmonic frequencies, which can then be plugged into your 
// animations using this command. There are 16 harmonic bands availible, the harmonic-value argument 
// will be wrapped around if greater or less than 16, so you can use this command without worrying 
// about out of range errors.
// Example:
// (define (animate)
//		(colour (vector (gh 1) (gh 2) (gh 3))) ; make a colour from the harmonics, and set it to be the current colour 
//		(draw-cube)) ; draw a cube with this colour
// (every-frame (animate))
// EndFunctionDoc

// StartFunctionDoc-pt
// gh número-harmonico
// Retorna: void
// Descrição:
// Fluxus converte áudio entrando em frequências harmônicas, que pode
// então ser ligada em suas animações através desse comando. Existem
// 16 bandas harmônicas disponiveis, o argumento do valor-harmônico
// vai ser arredondado se maior ou menor que 16, então você pode usar
// esse comando sem se preocupar sobre erros fora do alcance.
// Exemplo:
// (define (animate)
//		(colour (vector (gh 1) (gh 2) (gh 3))) ; make a colour from the harmonics, and set it to be the current colour 
//		(draw-cube)) ; draw a cube with this colour
// (every-frame (animate))
// EndFunctionDoc

Scheme_Object *get_harmonic(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_REG();	
	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("gh", "number", 0, argc, argv);
	if (Audio!=NULL)
	{	
		MZ_GC_UNREG(); 
    	return scheme_make_double(Audio->GetHarmonic((int)scheme_real_to_double(argv[0])));
	}
	MZ_GC_UNREG(); 
	return scheme_make_double(0);
}

// StartFunctionDoc-en
// gain gain-number
// Returns: void
// Description:
// Sets the gain level for the fft sound, it's 1 by default.
// Example:
// (gain 100) ; too quiet?!
// EndFunctionDoc

// StartFunctionDoc-pt
// gain número-gain
// Retorna: void
// Descrição:
// Ajusta o nível de amplificação para o som fft, é 1 por padrão.
// Exemplo:
// (gain 100) ; muito quieto?!
// EndFunctionDoc

Scheme_Object *gain(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_REG();	
	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("gain", "number", 0, argc, argv);
	if (Audio!=NULL)
	{	
		Audio->SetGain(scheme_real_to_double(argv[0]));
	}
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// process wavfile-string
// Returns: void
// Description:
// This command temporarally disables the realtime reading of the input audio stream and reads a 
// wav file instead. For use with the framedump command to process audio offline to make music 
// videos. The advantage of this is that it locks the framerate so the right amount of audio gets
// read for each frame - making syncing of the frames and audio files possible.
// Example:
// (process "somemusic.wav") ; read a precorded audio file
// EndFunctionDoc

// StartFunctionDoc-pt
// process wavfile-string
// Retorna: void
// Descrição:
// Este comando desativa temporariamente a leitura em tempo real da
// entrada da pista de áudio e lê um arquivo wav ao invés. Para usar
// com o comando framedump para processar audio offline para fazer
// videos musicais. A vantagem disto é que ele trava a taxa de quadros
// então a quantidade certa de áudio é lida para cada quadro - fazendo
// com que a sincrônia entre quadros e audio seja possível.
// Exemplo:
// (process "somemusic.wav") ; read a precorded audio file
// EndFunctionDoc

Scheme_Object *process(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_REG();	
	if (!SCHEME_CHAR_STRINGP(argv[0])) scheme_wrong_type("process", "string", 0, argc, argv);
	char *wavname=scheme_utf8_encode_to_buffer(SCHEME_CHAR_STR_VAL(argv[0]),SCHEME_CHAR_STRLEN_VAL(argv[0]),NULL,0);
	if (Audio!=NULL)
	{	
		Audio->Process(wavname);
	}
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// smoothing-bias value-number
// Returns: void
// Description:
// A kind of weighted average for the harmonic bands which smooth them out over time. 
// This setting defaults to 1.5. The best value really depends on the quality of the music, 
// and the buffer sizes, and ranges from 0 -> 2. It's more obvious if you give it a try 
// with the bars.scm script
// Example:
// (smoothing-bias 0) ; no smoothing
// EndFunctionDoc

// StartFunctionDoc-pt
// smoothing-bias número-valor
// Retorna: void
// Descrição:
// Uma espécie de média balanceada para as bandas harmônicas que as
// acalmam com o tempo. Esta opção é por padrão definida como 1.5. O
// melhor valor realmente depende da qualidade da música, e do tamanho
// do buffer, e varia de 0 -> 2. Fica mais óbvio se você tentar com o
// script bars.scm
// Exemplo:
// (smoothing-bias 0) ; no smoothing
// EndFunctionDoc

Scheme_Object *smoothing_bias(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1); 
	MZ_GC_VAR_IN_REG(0, argv); 
	MZ_GC_REG();	
	if (!SCHEME_NUMBERP(argv[0])) scheme_wrong_type("smoothing-bias", "number", 0, argc, argv);
	if (Audio!=NULL)
	{	
		Audio->SetSmoothingBias(scheme_real_to_double(argv[0]));
	}
	MZ_GC_UNREG(); 
    return scheme_void;
}

// StartFunctionDoc-en
// update-audio
// Returns: void
// Description:
// Updates the audio subsytem. This function is called for you (per frame) in fluxus-canvas.ss.
// Example:
// (update-audio)
// EndFunctionDoc

// StartFunctionDoc-pt
// update-audio
// Retorna: void
// Descrição:
// Atualiza o subsistema de áudio. Esta função é chamada para você
// (por frame) no fluxus-canvas.ss
// Exemplo:
// (update-audio)
// EndFunctionDoc

Scheme_Object *update_audio(int argc, Scheme_Object **argv)
{
	if (Audio!=NULL)
	{	
		Audio->GetFFT();
	}
    return scheme_void;
}

/////////////////////

#ifdef STATIC_LINK
Scheme_Object *audio_scheme_reload(Scheme_Env *env)
#else
Scheme_Object *scheme_reload(Scheme_Env *env)
#endif
{
	Scheme_Env *menv=NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_VAR_IN_REG(1, menv);
	MZ_GC_REG();
	// add all the modules from this extension
	menv=scheme_primitive_module(scheme_intern_symbol("fluxus-audio"), env);

	scheme_add_global("start-audio", scheme_make_prim_w_arity(start_audio, "start-audio", 3, 3), menv);
	scheme_add_global("gh", scheme_make_prim_w_arity(get_harmonic, "gh", 1, 1), menv);
	scheme_add_global("gain", scheme_make_prim_w_arity(gain, "gain", 1, 1), menv);
	scheme_add_global("process", scheme_make_prim_w_arity(process, "process", 1, 1), menv);
	scheme_add_global("smoothing-bias", scheme_make_prim_w_arity(smoothing_bias, "smoothing-bias", 1, 1), menv);
	scheme_add_global("update-audio", scheme_make_prim_w_arity(update_audio, "update-audio", 0, 0), menv);

	scheme_finish_primitive_module(menv);	
 	MZ_GC_UNREG(); 
	
	return scheme_void;
}

#ifndef STATIC_LINK
Scheme_Object *scheme_initialize(Scheme_Env *env)
{
	return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
	return scheme_intern_symbol("fluxus-audio");
}
#endif
