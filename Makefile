STOW = stow
STOWFLAGS =
STOWVERBOSE = 0

STOW-CMD = $(STOW) --target $(HOME) -v $(STOWVERBOSE) $(STOWFLAGS)
STOW-INSTALL = $(STOW-CMD) -R
STOW-UNINSTALL = $(STOW-CMD) -D

BASEPKGS = zsh emacs ssh mercurial git
PROGPKGS = clojure ruby
OSXPKGS = git.osx
LINUXPKGS =
GNOMEPKGS = git.gnome
UNITYPKGS = git.gnome

UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
SYSTEMTARGETS += osx
else
SYSTEMTARGETS += linux
endif
ifeq ($(XDG_CURRENT_DESKTOP),GNOME)
SYSTEMTARGETS += gnome
endif
ifeq ($(XDG_CURRENT_DESKTOP),Unity)
SYSTEMTARGETS += gnome
endif

.PHONY: base prog osx linux gnome proper

proper: base prog $(SYSTEMTARGETS)

gnome:
	$(STOW-INSTALL) $(GNOMEPKGS)

linux:
	$(STOW-UNINSTALL) $(OSXPKGS)

osx:
	$(STOW-INSTALL) $(OSXPKGS)
	$(STOW-UNINSTALL) $(GNOMEPKGS)

prog:
	$(STOW-INSTALL) $(PROGPKGS)

base:
	$(STOW-INSTALL) $(BASEPKGS)
