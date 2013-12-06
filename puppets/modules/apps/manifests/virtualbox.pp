# Class: apps::virtualbox
#
# This class installs Oracle VirtualBox and related utilities.
#
# Parameters:
#
# Actions:
# - Install Oracle VirtualBox
# - Install Vagrant
class apps::virtualbox {
  case $::operatingsystem {
    'Darwin': {

      $vbox_version = '4.3.4'
      $vbox_url = "http://download.virtualbox.org/virtualbox/${vbox_version}/VirtualBox-${vbox_version}-91027-OSX.dmg"

      package { "virtualbox-${vbox_version}":
        ensure   => installed,
        provider => appdmg,
        source   => $vbox_url,
        alias    => 'virtualbox'
      }

      $vagrant_version = '1.3.5'
      $vagrant_url = "http://files.vagrantup.com/packages/a40522f5fabccb9ddabad03d836e120ff5d14093/Vagrant-${vagrant_version}.dmg"

      package { "vagrant-${vagrant_version}":
        ensure   => installed,
        provider => appdmg,
        source   => $vagrant_url,
        require  => Package['virtualbox'],
        alias    => 'vagrant'
      }
    }
    'Archlinux': {
      package { 'virtualbox':
        ensure  => latest,
        require => Package['net-tools'], # Required for bridged networking
      }

      file { '/etc/modules-load.d/virtualbox.conf':
        source  => 'puppet:///modules/apps/modules-load.d/virtualbox.conf',
        owner   => 'root',
        group   => 'root',
        mode    => '0644',
        require => Package['virtualbox'],
      }

      package { 'vagrant':
        ensure  => latest,
        require => Package['virtualbox'],
      }
    }
    default: {
      warning("Don't know how to install Virtualbox on ${::operatingsystem}")
    }
  }
}
