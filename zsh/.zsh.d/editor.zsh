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

# Cursor management
function set-cursor-shape() {
  local shape="$1"

  case "${shape}" in
    block) local code=0;;
    bar) local code=1;;
    line) local code=2;;
    *) return 1
  esac

  printf '\e]50;CursorShape=%s\x7' "${code}"
}

# Track editor information# Updates editor information when the keymap changes.
# Exposes information about the Zsh Line Editor via the $editor_info associative
# array.
function editor-info {
  # Clean up previous $editor_info.
  unset editor_info
  typeset -gA editor_info

  editor_info[keymap]="$KEYMAP"

  if [[ "$KEYMAP" == 'viins' || "$KEYMAP" == 'main' ]]; then
    editor_info[mode]='insert'
    set-cursor-shape bar

    if [[ "$ZLE_STATE" == *overwrite* ]]; then
      editor_info[overwrite]='overwrite'
    else
      editor_info[overwrite]='insert'
    fi
  elif [[ "$KEYMAP" == "viopp" ]]; then
    editor_info[mode]='operator'
    set-cursor-shape line
  else
    editor_info[mode]='command'
    set-cursor-shape block
  fi
  zle reset-prompt
  zle -R
}
zle -N editor-info

function zle-keymap-select {
  zle editor-info
}
zle -N zle-keymap-select

function zle-line-init {
  zle editor-info
}
zle -N zle-line-init

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

# Keybindings
# Vi
bindkey -M viins 'fd' vi-cmd-mode
bindkey -M viins ' ' magic-space
bindkey -M viins '.' expand-dot-to-parent-directory-path
# Space is my leader in Zsh, too :) Spacemacs everywhere!
bindkey -M vicmd '\ e' edit-command-line
bindkey -M vicmd '\ i' expand-or-complete
bindkey -M vicmd '\ s' prepend-sudo
bindkey -M vicmd '\ \\' expand-cmd-path

# Emacs
bindkey -M emacs '\C-X\C-E' edit-command-line
bindkey -M emacs '\C-I' expand-or-complete
bindkey -M emacs ' ' magic-space
bindkey -M emacs '.' expand-dot-to-parent-directory-path
bindkey -M emacs '\C-x\C-S' prepend-sudo
bindkey -M emacs '\ee' expand-cmd-path

# Other maps
bindkey -M isearch . self-insert 2> /dev/null # Not in isearch

# Bind terminal keys
bindtermkey 'kcbt' reverse-menu-complete # Previous item on Shift+Tab

# Use vi bindings by default
bindkey -v
