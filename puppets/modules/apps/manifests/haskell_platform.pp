# Class: apps::haskell_platform
#
# This class installs Haskell Platform.
#
# On some operating systems, namely Arch Linux, this class only installs an
# incomplete subset of Haskell Platform, because the operating system does not
# provide complete packages.
#
# Parameters:
#
# Actions:
# - Install Haskell Platform.
class apps::haskell_platform {
  case $::operatingsystem {
    'Darwin': {
      package { 'haskell-platform': ensure => latest }
    }
    'Archlinux': {
      $haskell_packages = [ 'ghc',
                            'haddock',
                            # Some basic libraries
                            'haskell-regex-posix',
                            'haskell-parsec',
                            'haskell-random']
      package { $haskell_packages: ensure => latest }
    }
    default: {
      fail("Cannot install Haskell Platform on ${::operatingsystem}")
    }
  }
}
