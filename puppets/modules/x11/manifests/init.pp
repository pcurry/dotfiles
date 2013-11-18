# Class: x11
#
# This class installs X11.  On Linux, it installs Xorg.  On OS X, it uses
# XQuartz.
#
# Parameters:
#
# Actions:
# - Install an X11 distribution appropriate for the provisioned system
class x11 {
  case $::operatingsystem {
    'Darwin': {
      fail('You need to install XQuartz')
    }
    default: {
      include x11::xorg
    }
  }
}
