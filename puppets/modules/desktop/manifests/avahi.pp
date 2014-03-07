# Class:: desktop::avahi
#
# This class installs and configures the Avahi zeroconf system.
#
# Actions:
# - Install Avahi and enable its daemon
# - Enable mDNS name resolution
class desktop::avahi {
  package { 'avahi':
    ensure => latest,
  }

  service { 'avahi-daemon':
    ensure  => running,
    enable  => true,
    require => Package['avahi']
  }

  package { 'nss-mdns':
    ensure  => latest,
    require => Service['avahi-daemon'],
  }

  file_line { 'desktop::avahi::enable-mdns':
    path    => '/etc/nsswitch.conf',
    match   => '^hosts: files dns myhostname',
    line    => 'hosts: files dns myhostname mdns',
    require => Package['nss-mdns']
  }
}
