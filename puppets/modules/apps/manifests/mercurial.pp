# Class: apps::mercurial
#
# This class installs Mercurial.
#
# Actions:
# - Install Mercurial
class apps::mercurial {

  if $::operatingsystem == 'Darwin' {
    Package {
      provider => homebrew
    }
  }

  package { 'mercurial':
    ensure   => latest,
  }
}
