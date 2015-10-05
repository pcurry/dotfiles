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

ZSHD="$HOME/.zsh.d"

fpath=(
  "$ZSHD/functions"               # My own functions
  "$ZSHD/plugins/zshfunctions"    # 3rd party functions
  "$ZSHD/plugins/completions/src" # 3rd party completions
  $fpath
)

# Basic functions
autoload -Uz add-zsh-hook

# Basic options
setopt correct                  # Correct commands
setopt brace_ccl                # Always do brace expansion
setopt combining_chars          # Enable combining characters
setopt rc_quotes                # ' is a quote in single-quoted strings

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

# Mail
unset MAILCHECK
setopt no_mail_warning          # Do not warn about mail file access

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

# Autojump
local -a autojump_candidates
autojump_candidates=("/usr/share/autojump/autojump.zsh"
                     "/etc/profile.d/autojump.zsh")
if (( $+commands[brew] )); then
  autojump_candidates+="$(brew --prefix)/etc/profile.d/autojump.sh"
fi
for candidate in "${autojump_candidates[@]}"; do
  if [[ -f "${candidate}" ]]; then
    source "${candidate}"
  fi
done

# Command Not Found
if [[ -f /etc/zsh_command_not_found ]]; then
  source /etc/zsh_command_not_found
fi

# Zsh online help
unalias run-help
autoload run-help
if [[ $OSTYPE != darwin* ]]; then
  HELPDIR=/usr/local/share/zsh/help
else
  HELPDIR=/usr/share/zsh/help
fi
bindkey -M emacs '^H' run-help

# Syntax highlighting (KEEP BEFORE substring search, and AFTER all zle calls!)
if [[ $TERM == xterm* ]]; then
  source "$ZSHD/plugins/syntax-highlighting/zsh-syntax-highlighting.zsh"
fi
ZSH_HIGHLIGHT_HIGHLIGHTERS=(
  main                          # Basic highlighting
  brackets                      # Match parenthesis
  root                          # Highlight when root
  cursor                        # The cursor position
)

# Substring search (KEEP AFTER substring search!)
source "$ZSHD/plugins/history-substring-search/zsh-history-substring-search.zsh"
bindtermkey 'kcuu1' history-substring-search-up
bindtermkey 'kcud1' history-substring-search-down
bindkey -M emacs '^P' history-substring-search-up
bindkey -M emacs '^N' history-substring-search-down

# Tools
source "$ZSHD/coreutils.zsh"
source "$ZSHD/git.zsh"

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

# Emacs
alias emacs-version="emacs -Q --batch --eval '(progn (princ (emacs-version)) (terpri))'"

# Java utilities
function java-gc-details {      # Print the GC details for the JVM
  java -XX:+PrintCommandLineFlags -XX:+PrintGCDetails "$@" -version
}

function java-final-flags {     # Print the final flags for the JVM
  java -XX:+PrintFlagsFinal "$@" -version
}

# Hombebrew
alias brewc='brew cleanup'
alias brewC='brew cleanup --force'
alias brewi='brew install'
alias brewl='brew list'
alias brewq='brew info'
alias brewQ='brew home'
alias brews='brew search'
alias brewu='brew upgrade'
alias brewU='brew update && brew upgrade --all'
alias brewx='brew remove'

# Arch Linux (aura)
alias pac='aura'
alias pacc='sudo aura --sync --clean'
alias pacC='sudo aura --orphans --abandon'
alias paci='sudo aura --sync'
alias pacl='aura --query --list'
alias paco='aura --orphans'
alias pacq='aura --sync --info'
alias pacQ='aura --query --info'
alias pacs='aura --sync --search'
alias pacS='aura --query --search'
alias pacu='sudo aura --sync --refresh'
alias pacU='sudo aura --sync --refresh --sysupgrade'
alias pacx='sudo aura --remove'
alias pacX='sudo aura --remove --nosave --recursive'
alias auri='sudo aura --aursync --delmakedeps'
alias aurq='aura --aursync --info'
alias aurs='aura --aursync --search'
alias aurU='sudo aura --aursync --sysupgrade'

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
alias yuml='repoquery --list'
alias yumq='yum info'
alias yums='yum search'
alias yumu='sudo yum update'
alias yumU='sudo yum upgrade'
alias yumx='sudo yum erase'
function yumX {
  sudo yum remove "$@" && sudo yum autoremove
}

# SBT aliases
alias sbtb='sbt compile'
alias sbtC='sbt clean'
alias sbtt='sbt test'

# Stack
alias sta=stack
alias staI='stack init'
alias stab='stack build'
alias stad='stack haddock'
alias stai='stack install'
alias stas='stack setup'
alias stat='stack test'
if (( $+commands[stack] )); then
  # Load stack completions
  eval "$(stack --bash-completion-script "$(which stack)")"
fi

# Ocaml & OPAM
if (( $+commands[rlwrap] )); then
  # Add readline support to ocaml, if available
  alias ocaml='rlwrap ocaml'
fi

# Load OPAM and add some nice aliases
[[ -f "$HOME/.opam/opam-init/init.zsh" ]] &&
  source "$HOME/.opam/opam-init/init.zsh"
alias opme='eval "$(opam config env)"'
alias opmi='opam install'
alias opml='opam list'
alias opmq='opam show'
alias opmS='opam switch'
alias opms='opam search'
alias opmU='opam upgrade'
alias opmu='opam update'
alias opmx='opam remove'

# Rubygems
alias gemi='gem install --user-install --no-document'
alias gems='gem search'
alias gemU='gem update --user-install --no-document'

# NPM
alias npmi='npm install --global'
alias npmU='npm upgrade --global'

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

  if [[ -x /usr/local/bin/python3 ]]; then
    # Use Python 3 for virtualenv if it exists
    VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
  fi
fi

# TexLive
alias tlmi='sudo tlmgr install'
alias tlml='tlmgr info'
alias tlmq='tlmgr info'
alias tlms='tlmgr search --global'
alias tlmS='tlmgr search'
alias tlmU='sudo tlmgr update --self --all'

# CocoaPods
alias podi='pod install'
alias podI='pod init'
alias podU='pod update'

# Vagrant aliases
alias vag='vagrant'
alias vags='vagrant ssh'
alias vagu='vagrant up'
alias vagU='vagrant up --provision'
alias vagr='vagrant rsync'
alias vagra='vagrant rsync-auto'
alias vagR='vagrant reload'
alias vagh='vagrant suspend'
alias vagH='vagrant halt'
alias vagi='vagrant provision'
alias vagX='vagrant destroy'

# Build Textual
function build-textual {
  xctool \
    -workspace Textual.xcworkspace \
    -scheme 'Textual (Standard Release)' \
    CONFIGURATION_BUILD_DIR='Build Results/' && open 'Build Results/'
}

# Media
function avi-to-mp4 {           # Convert AVI to MP4 with ffmpeg
  if [[ $# -lt 1 ]]; then
    echo "Missing input" >&2
    return 1
  fi

  local input="$1"
  shift 1
  local output="${input:r}.mp4"

  ffmpeg -i "${input}" -map 0 -codec copy -f mov "${@}" "${output}"
}
