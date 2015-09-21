STOW = stow
STOWFLAGS =
STOWVERBOSE = 0
ANSIBLE-PLAYBOOK = ansible-playbook
ANSIBLE_FLAGS =
ANSIBLE_TAGS =

STOW-CMD = $(STOW) --target $(HOME) -v $(STOWVERBOSE) $(STOWFLAGS)
STOW-INSTALL = $(STOW-CMD) -R
STOW-UNINSTALL = $(STOW-CMD) -D

BASEPKGS = zsh emacs ssh git
PROGPKGS = scala ruby haskell ocaml ansible
OSXPKGS = git.osx

UNAME := $(shell uname -s)
ifeq ($(UNAME),Darwin)
SYSTEMTARGETS += osx
else
$(warn "$(UNAME) is not supported")
endif

.PHONY: base prog osx proper ansible

proper: base prog $(SYSTEMTARGETS)

osx:
	$(STOW-INSTALL) $(OSXPKGS)

prog:
	$(STOW-INSTALL) $(PROGPKGS)

base:
	$(STOW-INSTALL) $(BASEPKGS)
