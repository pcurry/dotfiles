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

# Defines environment variables and basic settings for all shell sessions

if [[ $HOSTNAME == *.uberspace.de ]]; then
  umask 077
else
  umask 022
fi

# Setup INFOPATH like manpath
typeset -T INFOPATH infopath ":"
export INFOPATH

# Paths
typeset -gU cdpath fpath mailpath path manpath infopath
path=(
  $HOME/bin                           # Personal tools
  $HOME/.cask/bin                     # Cask
  $HOME/.cabal/bin                    # Local Haskell packages
  ${PYTHONUSERBASE:-$HOME/.local}/bin # Local Python packages
  $HOME/Library/Python/*/bin(N)       # Local Python packages on OS X
  /usr/local/{bin,sbin}               # Local installations
  $path                               # The system path
)

manpath=(
  ${manpath:-""}                             # The default manpath
  /usr/local/texlive/*/texmf-dist/doc/man(N) # TeXLive manpages
  /Applications/ghc-*.app/Contents/share/man(N) # Portable GHC on OS X
)

infopath=(
  /usr/local/texlive/*/texmf-dist/doc/info(N) # TeXLive Info manuals
  ${infopath:-""}                             # The infopath or the default
)

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

# Java options
#
# Give Java programs more memory by default:
#
# * -Xmx2G        Increase the maximum heap size of the JVM from 1GB to 2GB
# * -Xss2M        Increase the thread stack size of the JVM from 1MB to 2MB
#
# For Java 7 we'd also set the following options to make sure that unreferenced
# class objects get freed:
#
# * -XX:+CMSClassUnloadingEnabled  Unload unreferenced classes
# * -XX:++UseConcMarkSweepGC       Required for CMSClassUnloadingEnabled
# * -XX:MaxPermSize=2G             Increase the maximum perm space for classes
#
# For reference about the GC settings, see
# https://blogs.oracle.com/poonam/entry/about_g1_garbage_collector_permanent
export JAVA_OPTS='-Xmx2G -Xss2M'

# Personal information
export EMAIL=swiesner@lunaryorn.com
