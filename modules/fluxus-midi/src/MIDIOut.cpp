/*
 Copyright (C) 2010 Gabor Papp

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

#include <stdio.h>
#include <iostream>
#include <vector>

#include "MIDIOut.h"

using namespace std;

MIDIOut::MIDIOut(int port /*= -1*/) :
	midiout(NULL)
{
	init_midi();

	if (port >= 0)
		open(port);
}

MIDIOut::~MIDIOut()
{
	if (midiout)
		delete midiout;
}

void MIDIOut::init_midi(void)
{
	if (midiout == NULL)
	{
		try
		{
			midiout = new RtMidiOut("FluxusMidi Output Client");
		}
		catch (RtError &error)
		{
			error.printMessage();
			midiout = NULL;
		}
	}
}

/**
 * Returns MIDI output port information.
 * \retval vector<string> string identifiers for the available MIDI
 *			output ports
 **/
vector<string> MIDIOut::info(void)
{
	port_names.clear();
	if (midiout == NULL)
	{
		init_midi();
		if (midiout == NULL)
			return port_names;
	}

	int port_count = midiout->getPortCount();
	std::string port_name;
	for (int i = 0; i < port_count; i++)
	{
		try
		{
			port_name = midiout->getPortName(i);
			port_names.push_back(port_name);
		}
		catch (RtError &error)
		{
			error.printMessage();
			break;
		}
	}

	return port_names;
}

/**
 * Opens MIDI port.
 * \param port port to open
 **/
void MIDIOut::open(int port)
{
	if (midiout == NULL)
	{
		init_midi();
		if (midiout == NULL)
			return;
	}

	int port_count = midiout->getPortCount();
	if (port >= port_count)
	{
		cerr << "midi out: invalid port\n";
		return;
	}

	try
	{
		midiout->openPort(port, "FluxusMidi Output");
	}
	catch (RtError &error)
	{
		error.printMessage();
		return;
	}
}

/**
 * Closes MIDI port.
 **/
void MIDIOut::close()
{
	if (midiout == NULL)
	{
		return;
	}

	try
	{
		midiout->closePort();
	}
	catch (RtError &error)
	{
		error.printMessage();
		return;
	}
}

/**
 * Sends midi message
 **/
void MIDIOut::send(std::vector<unsigned char> &message)
{
	if (midiout == NULL)
	{
		return;
	}

	midiout->sendMessage(&message);
}

