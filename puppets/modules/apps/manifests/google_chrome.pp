# Class: apps::google_chrome
#
# This class installs Google Chrome
#
# Parameters:
#
# Actions:
# - Install Google Chrome
class apps::google_chrome {
  case $::operatingsystem {
    'Darwin': {
      package { 'google-chrome':
        ensure   => installed,
        provider => appdmg,
        source   => 'https://dl.google.com/chrome/mac/stable/GGRO/googlechrome.dmg',
      }
    }
    'Archlinux': {
      package { 'google-chrome': ensure => latest }
    }
    default: {
      warning("Don't know how to install Chrome on ${::operatingsystem}")
    }
  }
}
