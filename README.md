My Dotfiles
===========

My personal dotfiles and settings for OS X.

Usage
-----

Clone this repository:

```console
$ git clone --recursive https://github.com/lunaryorn/dotfiles.git ~/dotfiles
```

You **must** clone to a sub-directory of `~`, otherwise `stow` fails to link the
dotfiles properly.  Don't forget `--recursive` to bring all submodules in.

Install and configure the system:

```sh
$ sudo rake conf:system:all
$ rake install:all conf:user:all dotfiles:install
```

Make sure to install [Homebrew][] first.

[Homebrew]: http://brew.sh
