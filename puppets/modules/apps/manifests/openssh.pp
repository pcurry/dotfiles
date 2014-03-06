# Class: apps::openssh
#
# This class installs and enables OpenSSH access.
class apps::openssh {
  # OpenSSH is included on OS X. FIXME: Enable OpenSSH on OS X?
  if $::operatingsystem != 'Darwin' {
    $openssh = $::operatingsystem ? {
      'Archlinux' => 'openssh',
      default     => fail("OpenSSH package name not known on ${::operatingsystem}")
    }

    package { $openssh:
      ensure => latest,
    }

    service { 'sshd.socket':
      ensure  => running,
      enable  => true,
      require => Package['openssh']
    }

    # TODO: Configure OpenSSH!
  }
}
