=============
 My Dotfiles
=============

My personal dotfiles, for use with `GNU Stow`_, and a bunch of Puppet_ manifests
for provisioning

.. default-role:: code

.. contents:: Table of Contents
   :local:
   :depth: 2

Usage
=====

Clone this repository::

   git clone --recursive https://github.com/lunaryorn/dotfiles.git ~/dotfiles

Don't forget `--recursive` to bring all submodules in, and clone to
`~/dotfiles`, really.

Provision the system with any of the `Puppet manifests`_::

   make -C puppets

Install any of the `available Stow packages`_::

   $ stow emacs git mercurial clojure ruby zsh

Puppet manifests
================

The directory `puppets/` provides Puppet manifests to provision my systems.
Refer to the `README <puppets/README.rst>`_ for more information.

Available Stow packages
=======================

Most other directories are `GNU Stow`_ packages:

`arch`
  Configuration for Arch Linux, e.g. `makepkg`
`clojure`
  Configuration for Clojure, e.g. Leiningen Profiles
`emacs`
  Emacs configuration, using `Stante Pede`_ and Cask_
`fontconfig`
  Font configuration for Linux
`git`
  Configuration for Git
`git.gnome`
  Configuration for Git, specific to Gnome 3
`git.osx`
  Configuration for Git, specific to OS X
`mercurial`
  Configuration for Mercurial
`ruby`
  Configuration for Ruby
`ssh`
  Configuration for SSH
`x11`
  Configuration for X11, e.g. X11 Resources
`zsh`
  Zsh configuration, using Prezto_

Use `make` to install the best set of Stow packages for your system.


Other configuration files
=========================

`VisualStudio`
  Settings files and extension list for Visual Studio


.. _GNU Stow: http://www.gnu.org/software/stow/
.. _Prezto: https://github.com/sorin-ionescu/prezto
.. _Stante Pede: https://github.com/lunaryorn/stante-pede
.. _Cask: https://github.com/cask/cask
.. _Puppet: http://puppetlabs.com/
