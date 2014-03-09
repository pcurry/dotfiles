#
# Executes commands at login pre-zshrc.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#   Sebastian Wiesner <lunaryorn@gmail.com>
#


#
# Browser
#

if [[ "$OSTYPE" == darwin* ]]; then
  export BROWSER='open'
fi

#
# Editors
#

export EDITOR='emacs -nw'
export VISUAL='emacsclient'
export PAGER='less'

#
# Language
#

if [[ -z "$LANG" ]]; then
  # Default to US, unless the language is already configured
  export LANG=en_US.utf8
fi

#
# Less
#

# Set the default Less options.
# Mouse-wheel scrolling has been disabled by -X (disable screen clearing).
# Remove -X and -F (exit if the content fits on one screen) to enable it.
export LESS='-F -g -i -M -R -S -w -X -z-4'

# Set the Less input preprocessor.
if (( $+commands[lesspipe.sh] )); then
  export LESSOPEN='| /usr/bin/env lesspipe.sh %s 2>&-'
fi

#
# Paths
#

typeset -gU cdpath fpath mailpath path

# Set the the list of directories that cd searches.
# cdpath=(
#   $cdpath
# )

# Set the list of directories that Zsh searches for programs.
path=(
  $HOME/bin
  /usr/local/{bin,sbin}
  $path
)

#
# Temporary Files
#

TMPPREFIX="${TMPDIR:-/tmp}/zsh"
if [[ ! -d "$TMPPREFIX" ]]; then
  mkdir -p "$TMPPREFIX"
fi

#
# Homebrew settings
#

if [[ "$OSTYPE" == darwin* ]]; then
  # Keep GNU Info files for all homebrew formulas
  export HOMEBREW_KEEP_INFO=1
  export PYTHONPATH="/usr/local/lib/python2.7/site-packages:$PYTHONPATH"
fi

# Personal information
export EMAIL=lunaryorn@gmail.com

