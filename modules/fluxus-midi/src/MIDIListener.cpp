/*
 Copyright (C) 2008 Gabor Papp

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

#include "stdio.h"
#include "MIDIListener.h"

using namespace std;

void midi_callback(double deltatime, vector<unsigned char> *message,
		void *user_data)
{
	static_cast<MIDIListener *>(user_data)->callback(deltatime, message);
}

MIDIListener::MIDIListener(int port /*= -1*/) :
	midiin(NULL)
{
	init_midi();

	if (port >= 0)
		open(port);

	/* allocate array for controller values and clear it */
	cntrl_values = new unsigned char[MAX_CNTRL];
	fill(cntrl_values, cntrl_values + MAX_CNTRL, 0);
}

MIDIListener::~MIDIListener()
{
	if (midiin)
		delete midiin;

	delete [] cntrl_values;
}

void MIDIListener::init_midi(void)
{
	if (midiin == NULL)
	{
		try
		{
			midiin = new RtMidiIn();
		}
		catch (RtError &error)
		{
			error.printMessage();
			midiin = NULL;
		}
		/* ignore MIDI sysex, timing and active sensing messages */
		midiin->ignoreTypes(true, true, true);
	}
}

/**
 * Returns MIDI input port information.
 * \retval vector<string> string identifiers for the available MIDI
 *			input ports
 **/
vector<string> MIDIListener::info(void)
{
	port_names.clear();
	if (midiin == NULL)
	{
		init_midi();
		if (midiin == NULL)
			return port_names;
	}

	int port_count = midiin->getPortCount();
	std::string port_name;
	for (int i = 0; i < port_count; i++)
	{
		try
		{
			port_name = midiin->getPortName(i);
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
void MIDIListener::open(int port)
{
	static bool callback_set = false;

	if (midiin == NULL)
	{
		init_midi();
		if (midiin == NULL)
			return;
	}

	int port_count = midiin->getPortCount();
	if (port >= port_count)
	{
		cerr << "midi listener: invalid port\n";
		return;
	}

	try
	{
		midiin->openPort(port);
	}
	catch (RtError &error)
	{
		error.printMessage();
		return;
	}

	if (callback_set)
	{
		midiin->cancelCallback();
	}
	midiin->setCallback(&midi_callback, this);
	callback_set = true;
}

/**
 * Returns controller values.
 * \param channel MIDI channel
 * \param cntrl_number controller number
 * \retval int controller value
 **/
int MIDIListener::get_cc(int channel, int cntrl_number)
{
	if (midiin == NULL)
	{
		init_midi();
		if (midiin == NULL)
			return 0;
	}
	return cntrl_values[(channel << 7) + cntrl_number];
}

/**
 * Returns normalised controller values.
 * \param channel MIDI channel
 * \param cntrl_number controller number
 * \retval float controller value normalised to the [0, 1] interval
 **/
float MIDIListener::get_ccn(int channel, int cntrl_number)
{
	if (midiin == NULL)
	{
		init_midi();
		if (midiin == NULL)
			return 0;
	}
	return (float)cntrl_values[(channel << 7) + cntrl_number] / 127.0;
}

void MIDIListener::callback(double deltatime, vector<unsigned char> *message)
{
	unsigned int count = message->size();
	int cat = (*message)[0] >> 4;
	int ch = (*message)[0] & 0xf;

	switch (cat)
	{
		case 0xb:	/* controller */
			if (count == 3)
			{
				int cntrl_number = (*message)[1]; /* controller number */
				/* array index from channel and controller number */
				int i = (ch << 7) + cntrl_number;
				cntrl_values[i] = (*message)[2]; /* store controller value */
			}
			break;
	}

	/*
	for (unsigned int i = 0; i < count; i++)
	{
		int data = (int)message->at(i);
		if (i == 0)
		{
			printf("cat:%x ch:%x ", cat, ch);
		}
		else
			printf("%d: %02x, ", i, data);
	}
	*/
}

