# -*- mode: sh; -*-
#
# Executes commands at the start of an interactive session.
#
# Authors:
#   Sebastian Wiesner <lunaryorn@gmail.com>
#

# Functions
fpath=($HOME/.zsh.d/functions $fpath)

# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Prompt setup
zstyle ':vcs_info:*:prompt_lunaryorn:*' enable git hg svn

autoload -Uz promptinit
promptinit
prompt 'lunaryorn'

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
