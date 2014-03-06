# Class: apps::sudo
#
# This class installs and configures sudo.
#
# Actions:
# - Install sudo
# - Set up sudo to allow administrator authentication
class apps::sudo {
  if $::operatingsystem != 'Darwin' {
    # On OS X, sudo is already included and configured
    package { 'sudo':
      ensure => latest
    }

    # Allow root authentication for the wheel group
    file { '/etc/sudoers.d/10-wheel':
      source  => 'puppet:///modules/apps/sudo/10-wheel',
      owner   => 'root',
      group   => 'root',
      mode    => '0600',
      require => Package['sudo'],
    }
  }
}
