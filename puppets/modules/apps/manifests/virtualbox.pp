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

      $vbox_version = '4.3.6'
      $vbox_url = "http://download.virtualbox.org/virtualbox/${vbox_version}/VirtualBox-${vbox_version}-91406-OSX.dmg"

      package { "virtualbox-${vbox_version}":
        ensure   => installed,
        provider => pkgdmg,
        source   => $vbox_url,
        alias    => 'virtualbox'
      }

      $vagrant_version = '1.4.2'
      $vagrant_url = "https://dl.bintray.com/mitchellh/vagrant/Vagrant-${vagrant_version}.dmg"

      package { "vagrant-${vagrant_version}":
        ensure   => installed,
        provider => pkgdmg,
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
        source  => 'puppet:///modules/apps/virtualbox/modules-load.d/virtualbox.conf',
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
