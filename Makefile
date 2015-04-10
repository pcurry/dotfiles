STOW = stow
STOWFLAGS =
STOWVERBOSE = 0
ANSIBLE-PLAYBOOK = ansible-playbook
ANSIBLE_FLAGS =
ANSIBLE_TAGS =

STOW-CMD = $(STOW) --target $(HOME) -v $(STOWVERBOSE) $(STOWFLAGS)
STOW-INSTALL = $(STOW-CMD) -R
STOW-UNINSTALL = $(STOW-CMD) -D
ANSIBLE-PLAY = $(ANSIBLE-PLAYBOOK) -i playbooks/inventory \
	$(addprefix -t,$(ANSIBLE_TAGS)) $(ANSIBLEFLAGS)

BASEPKGS = zsh emacs ssh mercurial git
PROGPKGS = scala ruby haskell ocaml
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

.PHONY: base prog osx linux gnome proper ansible

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

ansible:
	$(ANSIBLE-PLAY) playbooks/site.yml
