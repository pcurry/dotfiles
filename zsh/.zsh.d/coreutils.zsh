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

# Configure coreutils

# Colors
export GREP_COLOR='37;45'       # Grep colors
export GREP_OPTIONS='--color=auto'

if (( $+commands[dircolors] )); then
  # GNU coreutils
  eval "$(dircolors)"
  alias ls='ls --group-directories-first --color=auto'
else
  # BSD coreutils
  export LSCOLORS='exfxcxdxbxGxDxabagacad'
  export LS_COLORS= 'di=34:ln=35:so=32:pi=33:ex=31:bd=36;01:cd=33;01:su=31;40;07:sg=36;40;07:tw=32;40;07:ow=33;40;07:'
  alias ls='ls -G'
fi

if [[ $TERM == dumb ]]; then
  # If the terminal doesn't support colors, make ls disable the file type by
  # characters instead by colors
  alias ls="${aliases[ls]} -F"
fi

# Misc coreutils
alias cp='nocorrect cp -i'      # Safe copy
alias df='df -kh'               # Nicer df
alias du='du -kh'               # Nicer du
alias ln='nocorrect ln -i'      # Safe ln
alias mv='nocorrect mv -i'      # Safe mv
alias mkdir='nocorrect mkdir -p' # Automatically make parent dirs
alias rm='nocorrect rm -i'       # Safe rm
alias man='nocorrect man'
alias grep='nocorrect grep'
alias find='noglob find'

# ls shortcuts
alias l='ls -1A'         # One column, hidden files
alias ll='ls -lh'        # Human readable
alias lr='ll -R'         # Human readable, recursively
alias la='ll -A'         # Human readablem hidden files
alias lm='la | "$PAGER"' # Human readable, hidden files, in pager
alias lx='ll -XB'        # By extension (GNU only).
alias lk='ll -Sr'        # By size, largest last.
alias lt='ll -tr'        # By date, most recent last.
alias lc='lt -c'         # By date, most recent last, change time.
alias lu='lt -u'         # By date, most recent last, access time.

# Convenience functions
function mkdcd {
  [[ -n "$1" ]] && mkdir -p "$1" && builtin cd "$1"
}

function cdls {
  builtin cd "$argv[-1]" && ls "${(@)argv[1,-2]}"
}

function cde {
  # Idea graciously taken from
  # http://chneukirchen.org/blog/archive/2015/02/10-fancy-zsh-tricks-you-may-not-know.html,
  # slightly improved.  We use `quote' instead of ' for default-directory to
  # avoid the hassle of escaping single quotes in single quotes…
  local expr='(buffer-local-value (quote default-directory) (window-buffer))'
  local directory="$(emacsclient -e "${expr}")"
  cd "${(Q)directory}"
}
