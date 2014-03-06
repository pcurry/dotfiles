# Class: linux::package_manager
#
# This class sets up and configures the package manager of the system.
class linux::package_manager {
  case $::operatingsystem {
    'Archlinux': {
      require linux::arch::pacman
    }
    default: {
      fail("Package manager of $::operatingsystem not supported!")
    }
  }
}
