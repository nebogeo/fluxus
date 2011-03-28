/*
 Copyright (C) 2008 Dave Griffiths, Gabor Papp

 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program. If not, see <http://www.gnu.org/licenses/>.
*/

#include <escheme.h>
#include <iostream>
#include "MIDIListener.h"
#include "MIDIOut.h"

using namespace std;

static MIDIListener *midilistener = NULL;
static MIDIOut *midiout = NULL;

// StartSectionDoc-en
// midi
// MIDI stands for Musical Instrument Digital Interface, and it enables
// electronic musical instruments, computers, and other equipment to
// communicate, control, and synchronize with each other.
// Fluxus can receive MIDI control change and note messages.
// Example:
// (display (midi-info))(newline)
//
// (midi-init 1)
//
// (define (midi-test)
//     (with-state
//         (scale (vector (+ 1 (midi-ccn 0 1))
//                        (+ 1 (midi-ccn 0 2))
//                        (+ 1 (midi-ccn 0 3))))
//         (draw-cube)))
//
// (every-frame (midi-test))
// EndSectionDoc

// StartSectionDoc-pt
// midi
// MIDI é Interface Digital de Intrumentos Musicais, e permite
// instrumentos musicais eletronicos, computadores, e outros
// equipamentos comunicar, controlar e sincronizar entre si. Fluxus
// pode receber controles e mensagens de notas MIDI.
// Example:
// (display (midi-info))(newline)
//
// (midi-init 1)
//
// (define (midi-test)
//     (with-state
//         (scale (vector (+ 1 (midi-ccn 0 1))
//                        (+ 1 (midi-ccn 0 2))
//                        (+ 1 (midi-ccn 0 3))))
//         (draw-cube)))
//
// (every-frame (midi-test))
// EndSectionDoc

// StartFunctionDoc-en
// midi-info
// Returns: a list of two lists of (midi-port-number . midi-port-name-string) pairs
// Description:
// Returns information about the available MIDI input and output ports.
// Example:
// (midi-info)
// EndFunctionDoc

// StartFunctionDoc-pt
// midi-info
// Retorna: uma lista de (numero-porta-mide . string-nome-porta-midi) pares.
// Descrição:
// Retorna informação sobre portas de entrada MIDI disponíveis.
// Exemplo:
// (midi-info)
// EndFunctionDoc

Scheme_Object *midi_info(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;

	// input ports
	if (midilistener == NULL)
	{
		midilistener = new MIDIListener();
	}

	vector<string> port_names = midilistener->info();
	int port_count = port_names.size();
	Scheme_Object **a = (Scheme_Object **)scheme_malloc(port_count *
							sizeof(Scheme_Object *));

	for (int i = 0; i < port_count; i++)
	{
		Scheme_Object *port_num = NULL;
		Scheme_Object *port_name = NULL;

		MZ_GC_DECL_REG(2);
		MZ_GC_VAR_IN_REG(0, port_num);
		MZ_GC_VAR_IN_REG(1, port_name);
		MZ_GC_REG();

		port_num = scheme_make_integer(i);
		port_name = scheme_make_symbol(port_names[i].c_str());

		a[i] = scheme_make_pair(port_num, port_name);

		MZ_GC_UNREG();
	}

	Scheme_Object *input_ports = scheme_build_list(port_count, a);

	// output ports
	if (midiout == NULL)
	{
		midiout = new MIDIOut();
	}

	port_names = midiout->info();
	port_count = port_names.size();
	Scheme_Object **b = (Scheme_Object **)scheme_malloc(port_count *
							sizeof(Scheme_Object *));

	for (int i = 0; i < port_count; i++)
	{
		Scheme_Object *port_num = NULL;
		Scheme_Object *port_name = NULL;

		MZ_GC_DECL_REG(2);
		MZ_GC_VAR_IN_REG(0, port_num);
		MZ_GC_VAR_IN_REG(1, port_name);
		MZ_GC_REG();

		port_num = scheme_make_integer(i);
		port_name = scheme_make_symbol(port_names[i].c_str());

		b[i] = scheme_make_pair(port_num, port_name);

		MZ_GC_UNREG();
	}

	Scheme_Object *output_ports = scheme_build_list(port_count, b);

	// combine the two lists
	ret = scheme_make_pair(input_ports, scheme_make_pair(output_ports, scheme_null));

	return ret;
}

// StartFunctionDoc-en
// midiin-open port-number
// Returns: void
// Description:
// Opens the specified MIDI input port.
// Example:
// (midiin-open 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// midiin-open número-porta
// Retorna: void
// Descrição:
// Abre a porta de entrada MIDI especificada.
// Exemplo:
// (midiin-open 1)
// EndFunctionDoc

Scheme_Object *midiin_open(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();

	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("midiin-open", "number", 0, argc, argv);
	int port = (int)scheme_real_to_double(argv[0]);

	if (!midilistener)
	{
		midilistener = new MIDIListener(port);
	}
	else
	{
		midilistener->close();
		midilistener->open(port);
	}

	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// midiout-open port-number
// Returns: void
// Description:
// Opens the specified MIDI output port.
// Example:
// (midiout-open 1)
// EndFunctionDoc

Scheme_Object *midiout_open(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();

	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("midiout-open", "number", 0, argc, argv);
	int port = (int)scheme_real_to_double(argv[0]);

	if (!midiout)
	{
		midiout = new MIDIOut(port);
	}
	else
	{
		midiout->close();
		midiout->open(port);
	}

	MZ_GC_UNREG();
    return scheme_void;
}

// StartFunctionDoc-en
// midiin-close
// Returns: void
// Description:
// Closes the MIDI input port opened.
// Example:
// (midiin-close)
// EndFunctionDoc

Scheme_Object *midiin_close(int argc, Scheme_Object **argv)
{
	if (midilistener != NULL)
	{
		midilistener->close();
	}

    return scheme_void;
}

// StartFunctionDoc-en
// midiout-close
// Returns: void
// Description:
// Closes the MIDI outpu port opened.
// Example:
// (midiout-close)
// EndFunctionDoc

Scheme_Object *midiout_close(int argc, Scheme_Object **argv)
{
	if (midiout != NULL)
	{
		midiout->close();
	}

    return scheme_void;
}

// StartFunctionDoc-en
// midi-set-cc-mode mode-symbol
// Returns: void
// Description:
// Sets the controller encoder mode. The mode can be 'absolute, 'doepfer or 'ableton.
// The default is 'absolute. 'doepfer and 'ableton are incremental encoding modes.
// Example:
// (midi-set-cc-mode 'ableton)
// EndFunctionDoc

Scheme_Object *midi_set_cc_mode(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();

	if (!SCHEME_SYMBOLP(argv[0]))
		scheme_wrong_type("midi-set-cc-mode", "symbol", 0, argc, argv);

	string mode_str = scheme_symbol_name(argv[0]);

	if (midilistener != NULL)
	{
		int mode = -1;

		if (mode_str == "absolute")
		{
			 mode = MIDIListener::MIDI_CC_ABSOLUTE;
		}
		else
		if (mode_str == "doepfer")
		{
			 mode = MIDIListener::MIDI_CC_DOEPFER;
		}
		else
		if (mode_str == "ableton")
		{
			 mode = MIDIListener::MIDI_CC_ABLETON;
		}
		else
		{
			cerr << "midi-set-cc-mode: unknown mode " << mode_str << endl;
		}

		if (mode != -1)
		{
			midilistener->set_cc_encoder_mode(mode);
		}
	}

	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// midi-get-cc-mode
// Returns: mode-symbol
// Description:
// Returns the controller encoder mode. The mode can be 'absolute, 'doepfer or 'ableton.
// Example:
// (define cc-mode (midi-get-cc-mode))
// EndFunctionDoc

Scheme_Object *midi_get_cc_mode(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();

	if (midilistener != NULL)
	{
		int mode = midilistener->get_cc_encoder_mode();

		switch (mode)
		{
			case MIDIListener::MIDI_CC_ABSOLUTE:
				ret = scheme_make_symbol("absolute");
				break;
			case MIDIListener::MIDI_CC_DOEPFER:
				ret = scheme_make_symbol("doepfer");
				break;
			case MIDIListener::MIDI_CC_ABLETON:
				ret = scheme_make_symbol("ableton");
				break;
			default:
				cerr << "midi-get-cc-mode: unknown mode " << mode << endl;
				ret = scheme_void;
				break;
		}
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// midi-cc channel-number controller-number
// Returns: controller-value-number
// Description:
// Returns the controller value.
// Example:
// (midi-cc 0 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// midi-cc número-canal número-controle
// Retorna: número-valor-controle
// Descrição:
// Retorna o valor do controle
// Exemplo:
// (midi-cc 0 1)
// EndFunctionDoc

Scheme_Object *midi_cc(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();

	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("midi-cc", "number", 0, argc, argv);
	if (!SCHEME_NUMBERP(argv[1]))
		scheme_wrong_type("midi-cc", "number", 1, argc, argv);

	int channel = (int)scheme_real_to_double(argv[0]);
	int index = (int)scheme_real_to_double(argv[1]);

	if (midilistener != NULL)
	{
		int val = midilistener->get_cc(channel, index);
		ret = scheme_make_integer(val);
	}
	else
	{
		ret = scheme_void;
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// midi-ccn channel-number controller-number
// Returns: controller-value-number
// Description:
// Returns the controller value normalised to the (0, 1) interval.
// Example:
// (midi-ccn 0 1)
// EndFunctionDoc

// StartFunctionDoc-pt
// midi-ccn número-canal número-controle
// Retorna: número-valor-controle
// Descrição:
// Retorna o valor normalisado do controle no intervalo (0, 1).
// Exemplo:
// (midi-ccn 0 1)
// EndFunctionDoc

Scheme_Object *midi_ccn(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();

	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("midi-ccn", "number", 0, argc, argv);
	if (!SCHEME_NUMBERP(argv[1]))
		scheme_wrong_type("midi-ccn", "number", 1, argc, argv);

	int channel = (int)scheme_real_to_double(argv[0]);
	int index = (int)scheme_real_to_double(argv[1]);

	if (midilistener != NULL)
	{
		float val = midilistener->get_ccn(channel, index);
		ret = scheme_make_float(val);
	}
	else
	{
		ret = scheme_make_float(0);
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// midi-note
// Returns: #(on-off-symbol channel note velocity) or #f
// Description:
// Returns the next event from the MIDI note event queue or #f if the queue is empty.
// Example:
// (midi-note)
// EndFunctionDoc

// StartFunctionDoc-pt
// midi-note
// Retorna: #(símbolo-ligado-desligado canal nota velocidade) ou #f
// Descrição:
// Retorna o próximo evento da fila de eventos de nota MIDI ou #f se a
// fila está vazia.
// Exemplo:
// (midi-note)
// EndFunctionDoc

Scheme_Object *midi_note(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(2, ret);
	MZ_GC_REG();

	ret = scheme_false;
	if (midilistener != NULL)
	{
		MIDINote *note = midilistener->get_note();
		if (note)
		{
			ret = scheme_make_vector(4, scheme_void);
			if (note->on_off == MIDIListener::MIDI_NOTE_OFF)
				SCHEME_VEC_ELS(ret)[0] = scheme_intern_symbol("note-off");
			else
				SCHEME_VEC_ELS(ret)[0] = scheme_intern_symbol("note-on");

			SCHEME_VEC_ELS(ret)[1] = scheme_make_integer(note->channel);
			SCHEME_VEC_ELS(ret)[2] = scheme_make_integer(note->note);
			SCHEME_VEC_ELS(ret)[3] = scheme_make_integer(note->velocity);
		}
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// midi-program channel-number
// Returns: program-value-number
// Description:
// Returns the program value.
// Example:
// (midi-program 0)
// EndFunctionDoc

Scheme_Object *midi_program(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_VAR_IN_REG(1, ret);
	MZ_GC_REG();

	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("midi-program", "number", 0, argc, argv);

	int channel = (int)scheme_real_to_double(argv[0]);

	if (midilistener != NULL)
	{
		int val = midilistener->get_program(channel);
		ret = scheme_make_integer(val);
	}
	else
	{
		ret = scheme_void;
	}

	MZ_GC_UNREG();
	return ret;
}

// midi-cc-event
// Returns: #(channel controller value) or #f
// Description:
// Returns the next event from the MIDI note event queue or #f if the queue is empty.
// Example:
// (midi-cc-event)
// EndFunctionDoc

Scheme_Object *midi_cc_event(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(2, ret);
	MZ_GC_REG();

	ret = scheme_false;
	if (midilistener != NULL)
	{
		MIDIEvent *evt = midilistener->get_cc_event();
		if (evt)
		{
			ret = scheme_make_vector(3, scheme_void);
			SCHEME_VEC_ELS(ret)[0] = scheme_make_integer(evt->channel);
			SCHEME_VEC_ELS(ret)[1] = scheme_make_integer(evt->controller);
			SCHEME_VEC_ELS(ret)[2] = scheme_make_integer(evt->value);
		}
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// midi-peek
// Returns: msg-string
// Description:
// Returns the name, and event type, and parameter bytes of the last MIDI
// event as a string for debugging purposes.
// Example:
// (display (midi-peek))(newline)
// EndFunctionDoc

// StartFunctionDoc-pt
// midi-peek
// Retorna: string-msg
// Descrição:
// Retorna o nome, tipo de evento e os bytes parâmetros do último
// evento de MIDI como uma string para propósitos de debugging.
// Exemplo:
// (display (midi-peek))(newline)
// EndFunctionDoc

Scheme_Object *midi_peek(int argc, Scheme_Object **argv)
{
	if (midilistener != NULL)
	{
		return scheme_make_utf8_string(midilistener->get_last_event().c_str());
	}
	else
	{
		return scheme_make_utf8_string("");
	}
}

// StartFunctionDoc-en
// midi-send
// Returns: void
// Description:
// Immediately send a single message out an open MIDI output port.
// Example:
// (midiout-open 0)
// (midi-send 144 64 90)
// (sleep 1)
// (midi-send 128 64 40)
// (midiout-close)
// EndFunctionDoc

Scheme_Object *midi_send(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();

	for (int n = 0; n < argc; n++)
	{
		if (!SCHEME_INTP(argv[n]))
		{
			scheme_wrong_type("midi-send", "int", n, argc, argv);
			break;
		}
	}

	if (midiout != NULL)
	{
		std::vector<unsigned char> message;
		for (int n = 0; n < argc; n++)
		{
			message.push_back(SCHEME_INT_VAL(argv[n]));
		}

		midiout->send(message);
	}

	MZ_GC_UNREG();
	return scheme_void;
}

// StartFunctionDoc-en
// midi-position
// Returns: #(bar beat ticks)
// Description:
// Returns the current position given by MIDI clocks.
// Example:
// (midiin-open 0)
// (midi-position)
// (midiin-close)
// EndFunctionDoc

Scheme_Object *midi_position(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(2, ret);
	MZ_GC_REG();

	if (midilistener != NULL)
	{
		ret = scheme_make_vector(3, scheme_void);
		SCHEME_VEC_ELS(ret)[0] = scheme_make_integer(midilistener->get_bar());
		SCHEME_VEC_ELS(ret)[1] = scheme_make_integer(midilistener->get_beat());
		SCHEME_VEC_ELS(ret)[2] = scheme_make_integer(midilistener->get_pulse());
	}
	else
	{
		ret = scheme_void;
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// midi-clocks-per-beat
// Returns: clocks-per-beat-value-number
// Description:
// Returns the number of MIDI clocks per beat, depending on the time signature.
// Example:
// (midi-clocks-per-beat)
// EndFunctionDoc

Scheme_Object *midi_clocks_per_beat(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, ret);
	MZ_GC_REG();

	if (midilistener != NULL)
	{
		ret = scheme_make_integer(midilistener->get_clocks_per_beat());
	}
	else
	{
		ret = scheme_void;
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// midi-beats-per-bar
// Returns: beats-per-bar-value-number
// Description:
// Returns the number of beats per bar, depending on the time signature.
// Example:
// (midi-beats-per-bar)
// EndFunctionDoc

Scheme_Object *midi_beats_per_bar(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, ret);
	MZ_GC_REG();

	if (midilistener != NULL)
	{
		ret = scheme_make_integer(midilistener->get_beats_per_bar());
	}
	else
	{
		ret = scheme_void;
	}

	MZ_GC_UNREG();
	return ret;
}

// StartFunctionDoc-en
// midi-set-signature
// Returns: void
// Description:
// Sets the time signature
// Calling this function resets the song position.
// Example:
// ;set the signature to 3/4
// (midi-set-signature 3 4)
// EndFunctionDoc

Scheme_Object *midi_set_signature(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();

	if (midilistener != NULL)
	{
		if (!SCHEME_NUMBERP(argv[0]))
			scheme_wrong_type("midi-set-signature", "number", 0, argc, argv);
		if (!SCHEME_NUMBERP(argv[1]))
			scheme_wrong_type("midi-set-signature", "number", 1, argc, argv);

		int bpb = (int)scheme_real_to_double(argv[0]);
		int cpb = (int)scheme_real_to_double(argv[1]);
		midilistener->set_signature(bpb, cpb);
	}

	MZ_GC_UNREG();
	return scheme_void;
}

#ifdef STATIC_LINK
Scheme_Object *midi_scheme_reload(Scheme_Env *env)
#else
Scheme_Object *scheme_reload(Scheme_Env *env)
#endif
{
	Scheme_Env *menv = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_VAR_IN_REG(1, menv);
	MZ_GC_REG();

	// add all the modules from this extension
	menv = scheme_primitive_module(scheme_intern_symbol("fluxus-midi"), env);

	scheme_add_global("midiin-open",
			scheme_make_prim_w_arity(midiin_open, "midiin-open", 1, 1), menv);
	scheme_add_global("midiin-close",
			scheme_make_prim_w_arity(midiin_close, "midiin-close", 0, 0), menv);

	scheme_add_global("midiout-open",
		scheme_make_prim_w_arity(midiout_open, "midiout-open", 1, 1), menv);
	scheme_add_global("midiout-close",
			scheme_make_prim_w_arity(midiout_close, "midiout-close", 0, 0), menv);

	scheme_add_global("midi-info",
			scheme_make_prim_w_arity(midi_info, "midi-info", 0, 0), menv);
	scheme_add_global("midi-set-cc-mode",
			scheme_make_prim_w_arity(midi_set_cc_mode, "midi-set-cc-mode", 1, 1), menv);
	scheme_add_global("midi-get-cc-mode",
			scheme_make_prim_w_arity(midi_get_cc_mode, "midi-get-cc-mode", 0, 0), menv);
	scheme_add_global("midi-cc",
			scheme_make_prim_w_arity(midi_cc, "midi-cc", 2, 2), menv);
	scheme_add_global("midi-ccn",
			scheme_make_prim_w_arity(midi_ccn, "midi-ccn", 2, 2), menv);
	scheme_add_global("midi-note",
			scheme_make_prim_w_arity(midi_note, "midi-note", 0, 0), menv);
	scheme_add_global("midi-peek",
			scheme_make_prim_w_arity(midi_peek, "midi-peek", 0, 0), menv);
	scheme_add_global("midi-program",
			scheme_make_prim_w_arity(midi_program, "midi-program", 1, 1), menv);
	scheme_add_global("midi-cc-event",
			scheme_make_prim_w_arity(midi_cc_event, "midi-cc-event", 0, 0), menv);

	scheme_add_global("midi-send",
			scheme_make_prim_w_arity(midi_send, "midi-send", 1, 3), menv);

	scheme_add_global("midi-position",
			scheme_make_prim_w_arity(midi_position, "midi-position", 0, 0), menv);

	scheme_add_global("midi-clocks-per-beat",
			scheme_make_prim_w_arity(midi_clocks_per_beat, "midi-clocks-per-beat", 0, 0), menv);

	scheme_add_global("midi-beats-per-bar",
			scheme_make_prim_w_arity(midi_beats_per_bar, "midi-beats-per-bar", 0, 0), menv);

	scheme_add_global("midi-set-signature",
			scheme_make_prim_w_arity(midi_set_signature, "midi-set-signature", 2, 2), menv);

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
	return scheme_intern_symbol("fluxus-midi");
}
#endif

