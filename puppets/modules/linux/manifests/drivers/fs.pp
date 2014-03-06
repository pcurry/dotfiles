# Class: linux::drivers::fs
#
# This class installs additional file-system drivers for Linux.
#
# Actions:
# - Install NTFS-3G for NTFS support
class linux::drivers::fs {
  package { 'ntfs-3g':
    ensure => latest,
  }
}
