# Class: desktop::networkmanager
#
# This class installs the NetworkManager service.
#
# Actions:
# - Install NetworkManager
# - Enable the NetworkManager service
class desktop::networkmanager {

  $dependencies = [ 'dhclient',   # DHCPv6 support
                    'openresolv', # Support for resolvconf
                    ]

  package { $dependencies:
    ensure => latest
  }

  package { 'networkmanager':
    ensure  => latest,
    require => Package[$dependencies]
  }

  service { 'NetworkManager':
    ensure  => running,
    enable  => true,
    require => Package['networkmanager'],
  }
}
