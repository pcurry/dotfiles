# Class: apps::mercurial
#
# This class installs Mercurial.
#
# Actions:
# - Install Mercurial
class apps::mercurial {
  package { 'mercurial':
    ensure   => latest,
  }
}
