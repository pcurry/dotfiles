# Class: lunaryorn::packages::linux
#
# Install packages specific to Linux
#
# Actions:
# - Install X11
# - Install KDE and related tools
# - Install NetworkManager and related tools
class lunaryorn::packages::linux {
  # Package manager configuration

  require linux::packages

  # Access to NTFS file systems
  package { 'ntfs-3g': ensure => latest }

  # My preferred shell
  package { 'zsh': ensure => latest }

  # Audio tools
  package { ['alsa-utils', 'pavucontrol']: ensure => installed }
  # Save and restore mixer volume on boot
  service { ['alsa-restore', 'alsa-store']:
    enable  => true,
    require => Package['alsa-utils'],
  }

  # OpenSSH for remote access
  package { 'openssh': ensure => latest }
  service { 'sshd.socket':
    ensure  => running,
    enable  => true,
    require => Package['openssh'],
  }

  # Sudo
  package { 'sudo': ensure => latest }

  # Network utilities and management
  include desktop::networkmanager
  package { ['wireless_tools', 'net-tools']:
    ensure => latest
  }

  # Desktop services
  include desktop::telepathy
  include desktop::gstreamer
  include desktop::gstreamer::legacy
  include desktop::fonts

  # KDE
  include kde
  include kde::networkmanager
  include kde::telepathy
  include kde::kdm
  include kde::tools
  include kde::k3b
  include kde::amarok
  include kde::dropbox
  kde::l10n { [ 'de', 'en_gb' ]: }

  # Puppet for provisioning.  On OS X, we install it as user-local Gem via
  # bundler, so we install it only on Linux
  package { 'puppet': ensure => latest }

  # Install Texinfo.  On OS X, Texinfo is already included.
  package { 'texinfo': ensure => latest }
}
