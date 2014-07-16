# Copyright (c) 2014 Sebastian Wiesner <swiesner@lunaryorn.com>
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

ZSHD="$HOME/.zsh.d"

fpath=(
  "$ZSHD/functions"               # My own functions
  "$ZSHD/plugins/zshfunctions"    # 3rd party functions
  "$ZSHD/plugins/completions/src" # 3rd party completions
  $fpath
)

MANPATH="$ZSHD/plugins/zshfunctions/man:${MANPATH}"

# Basic functions
autoload -Uz add-zsh-hook

# Basic options
setopt correct                  # Correct commands
setopt brace_ccl                # Always do brace expansion
setopt combining_chars          # Enable combining characters
setopt rc_quotes                # ' is a quote in single-quoted strings
setopt no_mail_warning          # Do not warn about mail file access

# Globbing
setopt case_glob                # Case sensitive globbing
setopt extended_glob            # Power up globbing

# Jobs control options
setopt long_list_jobs           # Long job listings
setopt auto_resume              # Auto-resume jobs on single word commands
setopt notify                   # Report job status immediately
setopt no_bg_nice               # Do not decrease priority of background jobs
setopt no_hup                   # Do not send SIGHUP when the shell exists
setopt no_check_jobs            # Do not check job status on exit

# Directories
setopt auto_cd                  # Change to dirs when typing the dir name
setopt auto_pushd               # Automatically push dirs on the stack
setopt pushd_ignore_dups        # Ignore duplicates on the stack
setopt pushd_silent             # Shut up
setopt pushd_to_home            # Bare pushd pushes $HOME
setopt cdable_vars
setopt auto_name_dirs           # Make params containing directories available #
                                # for ~foo

# Redirection
setopt multios                  # Redirect to multiple fds
setopt no_clobber               # Don't overwrite files with redirection

# History
HISTFILE="${HOME}/.zhistory"
HISTSIZE=10000                   # Size of internal history
SAVEHIST=10000                   # Size of history file
setopt bang_hist                 # '!' is special during expansion.
setopt extended_history          # Write more info to history
setopt inc_append_history        # Write history file immediately
setopt share_history             # Share history
setopt hist_expire_dups_first    # Expire duplicates first
setopt hist_ignore_dups          # Do not record duplicates,
setopt hist_ignore_all_dups      # and delete old duplicates
setopt hist_find_no_dups         # Do not show duplicates
setopt hist_ignore_space         # Ignore events with leading space
setopt hist_save_no_dups         # Do not save duplicates in history files
setopt hist_verify               # Do not execute immediately after expansion
setopt hist_beep                 # Beep when accessing non-existent history

# Aliases for builtins
alias cd='nocorrect cd'
alias po='popd'
alias pu='pushd'
alias type='type -a'

# VCS support
zstyle ':vcs_info:*' enable git hg svn

# Prompt
autoload -Uz promptinit
promptinit
prompt 'lunaryorn'

# Terminal title
autoload -Uz title_lunaryorn_setup
title_lunaryorn_setup

# Terminal working directory
autoload -Uz update_terminal_cwd
add-zsh-hook precmd update_terminal_cwd

# Editor and completion
source "$ZSHD/editor.zsh"
source "$ZSHD/completion.zsh"

# Syntax highlighting (KEEP BEFORE substring search!)
if [[ TERM == xterm* ]]; then
  source "$ZSHD/plugins/syntax-highlighting/zsh-syntax-highlighting.zsh"
fi
ZSH_HIGHLIGHT_HIGHLIGHTERS=(
  main                          # Basic highlighting
  brackets                      # Match parenthesis
  root                          # Highlight when root
  cursor                        # The cursor position
)

# Substring search
source "$ZSHD/plugins/history-substring-search/zsh-history-substring-search.zsh"
bindtermkey 'kcuu1' history-substring-search-up
bindtermkey 'kcud1' history-substring-search-down
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# Autojump
if (( $+commands[brew] )); then
  local autojump_directory="$(brew --prefix)/etc/"
else
  local autojump_directory="/usr/share/autojump/"
fi
if [[ -d "${autojump_directory}" ]]; then
  source "${autojump_directory}/autojump.zsh"
fi

# Command Not Found
if [[ -f /etc/zsh_command_not_found ]]; then
  source /etc/zsh_command_not_found
fi

# Tools
source "$ZSHD/coreutils.zsh"
source "$ZSHD/git.zsh"

if [[ $HOSTNAME == *.uberspace.de ]]; then
  source "$ZSHD/uberspace.zsh"
fi

# Misc
alias ag='nocorrect ag'
alias locate='noglob locate'
alias rsync='noglob rsync'
alias scp='noglob scp'
alias sftp='noglob sftp'
alias _='sudo'
alias b='${(z)BROWSER}'         # Browse
alias e='${(z)VISUAL:-${(z)EDITOR}}' # Edit
alias http-serve='python -m SimpleHTTPServer' # Simple HTTP server
alias o='open'
alias p='${(z)PAGER}'           # View with pager
alias pbc='pbcopy'
alias pbp='pbpaste'

# Utility aliases for Uberspace
alias quota='quota -gsl'

# Download files
if (( $+commands[curl] )); then
  alias get='curl --continue-at - --location --progress-bar --remote-name --remote-time'
elif (( $+commands[wget] )); then
  alias get='wget --continue --progress=bar --timestamping'
fi

# OS X everywhere
if [[ $OSTYPE != darwin* ]]; then
  alias open='xdg-open'
  alias pbcopy='xsel --clipboard --input'
  alias pbpaste='xsel --clipboard --output'
fi

# Hombebrew
alias brewc='brew cleanup'
alias brewC='brew cleanup --force'
alias brewi='brew install'
alias brewl='brew list'
alias brewq='brew info'
alias brewQ='brew home'
alias brews='brew search'
alias brewu='brew upgrade'
alias brewU='brew update && brew upgrade'
alias brewx='brew remove'

# DPKG
alias debc='sudo apt-get clean && sudo apt-get autoclean'
alias debf='apt-file search --regexp'
alias debi='sudo apt-get install'
alias debI='sudo dpkg -i'
alias debq='apt-cache show'
alias debs='apt-cache search'
alias debu='sudo apt-get update'
alias debU='sudo apt-get update && sudo apt-get dist-upgrade'
alias debx='sudo apt-get remove'
function debX {
  sudo apt-get remove --purge "$@" && sudo apt-get autoremove --purge
}

# Yum
alias yumc='sudo yum clean all'
alias yumi='sudo yum install'
alias yumq='yum info'
alias yums='yum search'
alias yumu='sudo yum update'
alias yumU='sudo yum upgrade'
alias yumx='sudo yum erase'
function yumX {
  sudo yum remove "$@" && sudo yum autoremove
}

# Cabal
alias cab='cabal'
alias cabb='cabal build'
alias cabc='cabal configure'
alias cabC='cabal clean'
alias cabd='cabal install --only-dependencies' # (d)ependencies
alias cabi='cabal install'
alias cabI='cabal copy'
alias cabp='cabal upload'       # (p)ush
alias cabq='cabal info'
alias cabs='cabal list'
alias cabSi='cabal sandbox init'
alias cabSI='cabSi && cabd'
alias cabSx='cabal sandbox delete'
alias cabt='cabal test'
alias cabU='cabal install --upgrade-dependencies --force-reinstalls world'
alias cabX='cabal clean && cabal sandbox delete'

# Cask
[[ -d "$HOME/.cask" ]] && source "$HOME/.cask/etc/cask_completion.zsh" 2> /dev/null

alias cai='cask install'
alias cau='cask update'
alias caU='cask install && cask update'

# Virtualenv
VIRTUAL_ENV_DISABLE_PROMPT=1    # Don't mess with my prompt!
if (( $+commands[virtualenvwrapper_lazy.sh] )); then
  export WORKON_HOME="$HOME/.virtualenvs"
  source "$commands[virtualenvwrapper_lazy.sh]"

  if [[ $HOSTNAME == *.uberspace.de ]]; then
    VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python2.7
  fi
fi
