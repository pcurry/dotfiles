# Copyright (c) 2014, 2015 Sebastian Wiesner <swiesner@lunaryorn.com>
# Copyright (c) 2011-2014 Sorin Ionescu <sorin.ionescu@gmail.com>
# Copyright (c) 2009-2011 Robby Russell and contributors.

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# Disable global RCS files.  My configuration is carefully assembled, and I
# don't want the system mess in it
setopt no_global_rcs            # Disable global RCS files

# Some basic settings for all shell sessions
if [[ $HOSTNAME == *.uberspace.de ]]; then
  umask 077
else
  umask 022
fi

# Temporary files for Zsh
TMPPREFIX="${TMPDIR:-/tmp}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi
