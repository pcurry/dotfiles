# Class: linux::dkms
#
# This class sets up the Dynamic Kernel Module System, to dynamically rebuild
# modules after kernel upgrades.
#
# Actions:
# - Install DKMS
# - Enable the DKMS build service
class linux::dkms {
  package { 'dkms':
    ensure => latest
  }

  # Save and restore mixer volume on boot
  service { 'dkms':
    ensure  => running,
    enable  => true,
    require => Package['dkms'],
  }
}
