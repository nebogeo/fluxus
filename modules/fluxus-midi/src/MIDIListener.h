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

#ifndef __MIDI_LISTENER__
#define __MIDI_LISTENER__

#include <vector>
#include "RtMidi.h"

using namespace std;

#define MAX_CNTRL (16*128)

class MIDIListener
{
	public:
			MIDIListener(int port = -1);
			~MIDIListener();

			vector<string> info(void);
			/** callback receiving MIDI data */
			void callback(double deltatime, vector< unsigned char > *message);
			void open(int port); /**< open a MIDI input connection */

			int get_cc(int channel, int cntrl_number);

			float get_ccn(int channel, int cntrl_number);
	private:
			void init_midi(void);

			RtMidiIn *midiin; /**< handler of realtime MIDI input */
			vector<string> port_names; /**< names of MIDI ports */

			/** array holding the current state of all, 16*128 controllers */
			unsigned char *cntrl_values;
};

#endif

