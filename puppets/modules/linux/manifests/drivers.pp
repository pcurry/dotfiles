# Class: linux::drivers
#
# This class installs additional drivers for Linux.
#
# Actions:
# - Install additional WLAN drivers
# - Install additional file-system drivers
class linux::drivers {
  include linux::drivers::wlan
  include linux::drivers::fs
}
