# Class: apps::inkscape
#
# This class installs and configures Inkscape.
#
# Actions:
# - Install Inkscape
class apps::inkscape {
  if $::operatingsystem == 'Darwin' {
    notice('Not installing Inkscape on OS X, because it requires X11!')
  }
  else {
    package { 'inkscape':
      ensure => latest
    }
  }
}
