# Class: system::ntp
#
# This class enables NTP time synchronization.
class system::ntp {
  if $::operatingsystem != 'Darwin' {
    package {'chrony':
      ensure => latest
    }

    file { '/etc/chrony.conf':
      source => 'puppet:///modules/system/ntp/chrony.conf',
      owner  => 'root',
      group  => 'root',
      mode   => '0644',
      notify => Service['chrony'],
    }

    service { 'chrony':
      ensure  => running,
      enable  => true,
      require => [Package['chrony'], File['/etc/chrony.conf']]
    }

    exec { 'system::ntp::set-ntp':
      command => 'timedatectl set-ntp yes',
      unless  => 'timedatectl status | grep -q "NTP enabled: yes"',
      path    => ['/usr/bin', '/bin'],
      require => Service['chrony'],
    }
  }
  else {
    # FIXME: Enable NTP sychronization on OS X
    fail('NTP sychronization on OS X not supported!')
  }
}
