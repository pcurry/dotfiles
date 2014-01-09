# Class: apps::mu4
#
# This class installs mu4e, an Emacs mail client.
#
# Actions:
# - Install mu4e
#
# Requires:
# - Package['emacs']
class apps::mu4e {
  case $::operatingsystem {
    'Darwin': {
      package { 'mu':
        ensure          => latest,
        install_options => ['--HEAD', '--with-emacs'],
        require         => Package['emacs'],
      }
    }
    default: {
      fail("Cannot install Emacs snapshot on ${::operatingsystem}")
    }
  }
}
