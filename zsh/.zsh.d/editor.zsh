# Copyright (c) 2014 Sebastian Wiesner <lunaryorn@gmail.com>
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

# Configure the ZSH line editor

# Register additional widgets
autoload -Uz url-quote-magic   && zle -N self-insert url-quote-magic
autoload -Uz edit-command-line && zle -N edit-command-line

function expand-dot-to-parent-directory-path {
  if [[ $LBUFFER = *.. ]]; then
    LBUFFER+='/..'
  else
    LBUFFER+='.'
  fi
}
zle -N expand-dot-to-parent-directory-path

function prepend-sudo {
  if [[ "$BUFFER" != su(do|)\ * ]]; then
    BUFFER="sudo $BUFFER"
    (( CURSOR += 5 ))
  fi
}
zle -N prepend-sudo

unsetopt beep                   # Don't beep

# Conservative word characters
WORDCHARS='_-'

zmodload zsh/terminfo

function bindtermkey {
  local key="${terminfo[$1]}"
  if [[ -n $key ]]; then
    bindkey -M emacs "$key" "${2}"
  fi
}

#  Keybindings
bindkey -M emacs '\C-X\C-E' edit-command-line
bindkey -M emacs '\C-I' expand-or-complete
bindkey -M emacs ' ' magic-space
bindkey -M emacs '.' expand-dot-to-parent-directory-path
bindkey -M isearch . self-insert 2> /dev/null # Not in isearch
bindkey -M emacs '\C-x\C-S' prepend-sudo
bindkey -M emacs '\ee' expand-cmd-path

# Bind terminal keys
bindtermkey 'kcbt' reverse-menu-complete # Previous item on Shift+Tab

bindkey -e                      # Emacs keybindings
