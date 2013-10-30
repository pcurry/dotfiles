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

# Opam setup, see https://github.com/sorin-ionescu/prezto/pull/484
if (( $+commands[opam] )); then
  eval "$(opam config env)"
fi

# Step Prezto from mingling in GEM_HOME
unset GEM_HOME
