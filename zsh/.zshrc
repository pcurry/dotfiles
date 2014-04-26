# -*- mode: sh; -*-
#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sorin Ionescu <sorin.ionescu@gmail.com>
#

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Source OPAM
if [[ -f "$HOME/.opam/opam-init/init.zsh" ]]; then
  source "$HOME/.opam/opam-init/init.zsh"
fi

# Source Cask completions
if [[ -d "$HOME/.cask" ]]; then
  source "$HOME/.cask/etc/cask_completion.zsh" 2> /dev/null
fi

# Setup Virtualenvs
VIRTUAL_ENV_DISABLE_PROMPT=1    # Don't mess with my prompt!
if (( $+commands[virtualenvwrapper_lazy.sh] )); then
  export WORKON_HOME="$HOME/.virtualenvs"
  source "$commands[virtualenvwrapper_lazy.sh]"
fi
