# Class: apps::git
#
# This class installs Git.
#
# Actions:
# - Install Git
class apps::git {

  if $::operatingsystem == 'Darwin' {
    Package {
      provider => homebrew
    }
  }

  package { 'git':
    ensure   => latest,
  }
}
