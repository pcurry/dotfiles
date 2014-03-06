# Class: apps::zsh
#
# This class installs the Zsh shell and some additional plugins.
#
# Actions:
# - Install Zsh
# - Install FASD for fast directory switching
class apps::zsh {

  $plugins = [ 'fasd', ]

  if $::operatingsystem != 'Darwin' {
    # Zsh is pre-installed on OS X
    package { 'zsh':
      ensure => latest,
      before => Package[$plugins]
    }
  }
  else {
    # Use Homebrew on OS X
    Package {
      provider => homebrew
    }
  }

  package { $plugins:
    ensure => latest
  }
}
