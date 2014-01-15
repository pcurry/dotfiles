# Class: apps::python2
#
# This class installs Python 2.
#
# Actions:
# - Install Python 2, and all associated tools (e.g. Pip)
class apps::python2 {
  case $::operatingsystem {
    'Darwin': {
      # OS X comes with Python 2 preinstalled
    }
    'Archlinux': {
      package { 'python2': ensure => latest }

      package { 'python2-pip':
        ensure  => latest,
        require => Package['python2'],
      }
    }
    default: {
      fail("Cannot install Python 2 on ${::operatingsystem}")
    }
  }
}
