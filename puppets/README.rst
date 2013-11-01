=========
 Puppets
=========

Puppet_ manifests to provision my systems.

.. default-role:: code

Usage
=====

Install Puppet_ with `bundle install` from the provided `Gemfile`.

Use `make MANIFEST` to run any of the following manifests.  Be sure to omit the
``.pp`` file extension in `MANIFEST`, e.g.::

   make PUPPETARGS=--debug osx

The variable `PUPPETARGS` passes additional arguments to `puppet apply`.


Available manifests
===================

The following manifests are available in the `manifests/` directory:

`osx.pp`
  Provision my OS X system:

  - Warn about FileVault being disabled
  - Install Homebrew, and a good selection of Homebrew packages
  - Switch my shell to the awesome Zsh
  - Disable the annoying beep sound on boot
  - Set the host name
  - Configure German locale and timezone
  - Change a number of OS X defaults (e.g. Dock behaviour, Finder settings,
    etc.)
  - Change the Terminal.app color theme to Solarized Light
  - Configure Twitter.app


Available modules
=================

The following modules are available in the `modules/` directory:

`osx`
  Additional facts resources to manage OS X

`homebrew`
  A `package` provider for Homebrew Formulae, a class to install Homebrew on the
  system, and some resources to manage Homebrew.

Additionally the following 3rd party modules are available as Git Submodules:

`stdlib`
  The `Puppet Standard Library`_

`vcsrepo`
  The vcsrepo_ module to manage version control repositories with Git,
  Mercurial, etc.


.. _Puppet: http://puppetlabs.com/
.. _Puppet Standard Library: https://github.com/puppetlabs/puppetlabs-stdlib
.. _vcsrepo: https://github.com/puppetlabs/puppetlabs-vcsrepo
