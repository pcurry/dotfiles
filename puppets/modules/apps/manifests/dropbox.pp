# Class: apps::dropbox
#
# This class installs Dropbox.  On OS X, this class merely refers the user to
# the Dropbox website, because the Dropbox installer for OS X is a non-standard
# blob which cannot easily be used from within Puppet
#
# Parameters:
#
# Actions:
# - Install Dropbox
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
