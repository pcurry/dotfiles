# Copyright (c) 2014 Sebastian Wiesner <swiesner@lunaryorn.com>

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

# Set basic variables at login, before .zshrc.

if [[ $HOSTNAME == *.uberspace.de ]]; then
  umask 077
else
  umask 022
fi

# Paths
typeset -gU cdpath fpath mailpath path
path=(
  $HOME/bin                           # Personal tools
  $HOME/.cask/bin                     # Cask
  $HOME/.cabal/bin                    # Local Haskell packages
  ${PYTHONUSERBASE:-$HOME/.local}/bin # Local Python packages
  $HOME/Library/Python/*/bin(N)       # Local Python packages on OS X
  $HOME/.gem/ruby/*/bin(N)            # Local Ruby packages
  /usr/local/{bin,sbin}               # Local installations
  $path                               # The system path
)

# Locale
if [[ -z "$LANG" ]]; then
  # Default to US, unless the language is already configured
  export LANG=en_US.utf8
fi

# Basic tools
export EDITOR='emacsclient'
export VISUAL='emacsclient'
export PAGER='less'
if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

# less options:
#
# -g: Highlight search results
# -i: Ignore case when searching
# -M: Prompt verbosely
# -R: Print ANSI color sequences
# -S: Cut of long lines instead of wrapping them
# -w: Highlight the first new line after scrolling
# -z: Keep four lines when scrolling
export LESS='-g -i -M -R -S -w -z-4'

# Temporary files for Zsh
TMPPREFIX="${TMPDIR:-/tmp}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi

# Python packages on OS X
if [[ "$OSTYPE" == darwin* ]]; then
  export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"
fi

# OPAM
[[ -f "$HOME/.opam/opam-init/init.zsh" ]] && source "$HOME/.opam/opam-init/init.zsh"

# Personal information
export EMAIL=swiesner@lunaryorn.com
