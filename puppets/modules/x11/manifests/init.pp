# Class: x11
#
# Install X11
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
