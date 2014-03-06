# Class: apps::git
#
# This class installs Git.
#
# Actions:
# - Install Git
class apps::git {
  package { 'git':
    ensure   => latest,
  }
}
