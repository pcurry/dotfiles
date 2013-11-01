=============
 My Dotfiles
=============

My personal dotfiles, for use with `GNU Stow`_

.. default-role:: code

Usage
=====

Each directory is a package for `GNU Stow`_.

Use `gnu stow` to install the package you want, e.g.::

   $ stow git mercurial clojure ruby

Available packages
==================

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
`mercurial`
  Configuration for Mercurial
`ruby`
  Configuration for Ruby
`x11`
  Configuration for X11, e.g. X11 Resources
`zsh`
  Zsh configuration, using Prezto_

Puppet manifests
================

The directory `puppets` provides Puppet_ manifests to setup my systems.


.. _GNU Stow: http://www.gnu.org/software/stow/
.. _Prezto: https://github.com/sorin-ionescu/prezto
.. _Stante Pede: https://github.com/lunaryorn/stante-pede
.. _Cask: https://github.com/cask/cask
.. _Puppet: http://puppetlabs.com/
