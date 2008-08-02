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

using namespace std;

MIDIListener *midilistener = NULL;

// StartSectionDoc-en
// midi
// MIDI stands for Musical Instrument Digital Interface, and it enables
// electronic musical instruments, computers, and other equipment to
// communicate, control, and synchronize with each other.
// Fluxus can receive MIDI control change messages (CCs).
// Example:
// (require fluxus-015/fluxus-midi)
//
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
// Returns: a list of (midi-port-number . midi-port-name-string) pairs
// Description:
// Returns information about the available MIDI input ports.
// Example:
// (midi-info)
// EndFunctionDoc

Scheme_Object *midi_info(int argc, Scheme_Object **argv)
{
	Scheme_Object *ret = NULL;
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

	ret = scheme_build_list(port_count, a);

	return ret;
}

// StartFunctionDoc-en
// midi-init port-number
// Returns: void
// Description:
// Opens the specified MIDI input port.
// Example:
// (midi-init 1)
// EndFunctionDoc

Scheme_Object *midi_init(int argc, Scheme_Object **argv)
{
	MZ_GC_DECL_REG(1);
	MZ_GC_VAR_IN_REG(0, argv);
	MZ_GC_REG();

	if (!SCHEME_NUMBERP(argv[0]))
		scheme_wrong_type("midi-init", "number", 0, argc, argv);
	int port = (int)scheme_real_to_double(argv[0]);

	if (!midilistener)
	{
		midilistener = new MIDIListener(port);
	}
	else
	{
		midilistener->open(port);
	}

	MZ_GC_UNREG();
    return scheme_void;

}

// StartFunctionDoc-en
// midi-cc channel-number controller-number
// Returns: controller-value-number
// Description:
// Returns the controller value.
// Example:
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
		ret = scheme_void;
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

Scheme_Object *scheme_reload(Scheme_Env *env)
{
	Scheme_Env *menv = NULL;
	MZ_GC_DECL_REG(2);
	MZ_GC_VAR_IN_REG(0, env);
	MZ_GC_VAR_IN_REG(1, menv);
	MZ_GC_REG();

	// add all the modules from this extension
	menv = scheme_primitive_module(scheme_intern_symbol("fluxus-midi"), env);

	scheme_add_global("midi-init",
			scheme_make_prim_w_arity(midi_init, "midi-init", 1, 1), menv);
	scheme_add_global("midi-info",
			scheme_make_prim_w_arity(midi_info, "midi-info", 0, 0), menv);
	scheme_add_global("midi-cc",
			scheme_make_prim_w_arity(midi_cc, "midi-cc", 2, 2), menv);
	scheme_add_global("midi-ccn",
			scheme_make_prim_w_arity(midi_ccn, "midi-ccn", 2, 2), menv);
	scheme_add_global("midi-peek",
			scheme_make_prim_w_arity(midi_peek, "midi-peek", 0, 0), menv);

	scheme_finish_primitive_module(menv);
	MZ_GC_UNREG();

	return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
	return scheme_reload(env);
}

Scheme_Object *scheme_module_name()
{
	return scheme_intern_symbol("fluxus-midi");
}

