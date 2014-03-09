# Class: linux::drivers::wlan
#
# This class installs additional WLAN drivers for Linux.
#
# Actions:
# - Install the Realtek 8192cu driver, which works much better than Linux'
#   built-in rtl8192cu driver
class linux::drivers::wlan {
  require linux::dkms

  package { 'dkms-8192cu':  # A much better driver for my Edimax WLAN device
    ensure => latest,
  }
}
