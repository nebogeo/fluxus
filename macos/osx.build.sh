#!/bin/bash
plt="/Applications/PLT"
scons -u Prefix=/opt/local/ PLTPrefix="$plt" PLTLib="$plt"/lib PLTInclude="$plt"/include
sudo scons -u Prefix=/opt/local/ PLTPrefix="$plt" PLTLib="$plt"/lib PLTInclude="$plt"/include install
