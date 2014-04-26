=========
 Puppets
=========

Puppet_ manifests to provision my systems.

.. default-role:: code

Usage
=====

Install Puppet_ with `bundle install` from the provided `Gemfile`.

Use `make` to run apply all manifests::

   make PUPPETARGS=--debug

The variable `PUPPETARGS` passes additional arguments to `puppet apply`.

Available modules
=================

The directory `modules/` contains all modules.  The module `lunaryorn` contains
all my personal configuration.

The other modules provide additional resources and facts:

`apps`
  Classes to install various applications

`gnome`
  Install and configure Gnome 3

`homebrew`
  A `package` provider for Homebrew Formulae, a class to install Homebrew on the
  system, and some resources to manage Homebrew.

`osx`
  Additional facts and resources for OS X

`x11`
  Install and configure X11

Additionally the following 3rd party modules are available as Git Submodules:

`stdlib`
  The `Puppet Standard Library`_

.. _Puppet: http://puppetlabs.com/
.. _Puppet Standard Library: https://github.com/puppetlabs/puppetlabs-stdlib
