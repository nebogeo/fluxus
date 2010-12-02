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

#ifndef __MIDI_OUT__
#define __MIDI_OUT__

#include <vector>

#include "RtMidi.h"
#include "MIDIListener.h"

using namespace std;

class MIDIOut
{
	public:
			MIDIOut(int port = -1);
			~MIDIOut();

			vector<string> info(void);

			void open(int port); /**< open a MIDI output connection */
			void close(void); /**< close MIDI port */

			void send(std::vector<unsigned char> &message);
	private:
			void init_midi(void);

			RtMidiOut *midiout; /**< handler of MIDI output */
			vector<string> port_names; /**< names of MIDI ports */
};

#endif

