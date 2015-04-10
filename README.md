My Dotfiles
===========

My personal dotfiles, for use with [GNU Stow][], and a bunch of [Puppet][]
manifests for provisioning

Usage
-----

Clone this repository:

```console
$ git clone --recursive https://github.com/lunaryorn/dotfiles.git ~/dotfiles
```

Don't forget `--recursive` to bring all submodules in, and clone to
`~/dotfiles`, really.

Install dotfiles for this system:

```console
$ make
```

Available Stow packages
=======================

Most other directories are [GNU Stow][] packages:

- `clojure`:  Configuration for Clojure, e.g. Leiningen Profiles
- `emacs`: Emacs configuration, using [Stante Pede][] and [Cask][]
- `git`: Configuration for Git
- `git.gnome`: Configuration for Git, specific to Gnome 3
- `git.osx`: Configuration for Git, specific to OS X
- `mercurial`: Configuration for Mercurial
- `ruby`: Configuration for Ruby
- `scala`: Configuration for Scala
- `ssh`: Configuration for SSH
- `zsh`: Zsh configuration, using [Prezto][]

Use `make` to install the best set of Stow packages for your system.

[GNU Stow]: http://www.gnu.org/software/stow/
[Prezto]: https://github.com/sorin-ionescu/prezto
[Stante Pede]: https://github.com/lunaryorn/stante-pede
[Cask]: https://github.com/cask/cask

Other configuration files
=========================

`VisualStudio`
Settings files and extension list for Visual Studio
