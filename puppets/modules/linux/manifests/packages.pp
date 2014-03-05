# Class: linux::packages
#
# This class sets up and configures Linux package managers.
class linux::packages {
  case $::operatingsystem {
    'Archlinux': {
      require linux::arch::pacman
    }
    default: {
      fail("Package manager of $::operatingsystem not supported!")
    }
  }
}
