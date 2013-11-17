# Class: apps::dropbox
#
# Install Dropbox
class apps::dropbox {
  case $::operatingsystem {
    'Darwin': {
      # Dropbox has a silly non-standard OS X installer
      warning('Please download and install Dropbox manually from https://www.dropbox.com/downloading?os=mac.')
    }
    'Archlinux': {
      package { 'dropbox': ensure => latest }
    }
    default: {
      fail("Cannot install Dropbox on ${::operatingsystem}")
    }
  }
}
