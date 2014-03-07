# Class: desktop::cups::systemconfig
#
# This class installs the system configurations tools for printer management.
#
# Actions:
# - Install system-config-printer for printer-driver auto-detection
class desktop::cups::systemconfig {
  package { 'system-config-printer':
    ensure => latest,
  }
}
