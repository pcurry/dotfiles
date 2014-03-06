# Class: linux::alsa
#
# This class sets up ALSA, the Linux sound architecture.
#
# Actions:
# - Install ALSA tools
# - Enable restoring of mixer levels at boot
class linux::alsa {
  package { 'alsa-utils':
    ensure => latest
  }

  # Save and restore mixer volume on boot
  service { ['alsa-restore', 'alsa-store']:
    enable  => true,
    require => Package['alsa-utils'],
  }
}
