# Class: linux::arch::pacman
#
# This class bootstraps and configures pacman and yaourt.
#
# Actions:
# - Configure pacman
# - Sync package repositories
# - Update pacman
# - Install yaourt
class linux::arch::pacman {
  File {
    owner => 'root',
    group => 'root',
    mode  => '0644',
  }

  # The Pacman configuration file
  file { '/etc/pacman.conf':
    source => 'puppet:///modules/lunaryorn/pacman.conf',
    notify => Exec['pacman -Sy'],
  }

  # Refresh package lists if the configuration changed
  exec { 'pacman -Sy':
    path        => ['/usr/bin/'],
    refreshonly => true,
  }

  # Install current Pacman first
  package { 'pacman':
    ensure  => latest,
    require => [File['/etc/pacman.conf'], Exec['pacman -Sy']]
  }

  # Now generate a list of fast mirrors, with reflector
  package { 'reflector':
    ensure  => latest,
    require => Package['pacman'],
  }

  # We don't use "creates" here, because we want to regenerate the mirror
  # list each time before provisioning
  exec { 'reflector -c Germany -p https -n 10 -f 5 --sort rate --save /etc/pacman.d/mirrorlist':
    path    => ['/usr/bin/', '/bin/'],
    require => Package['reflector'],
    before  => File['/etc/pacman.d/mirrorlist'],
  }

  # Resource anchor for the mirrorlist
  file { '/etc/pacman.d/mirrorlist':
    ensure => present,
  }

  # Now install Yaourt, to automatically install packages from AUR
  file { '/etc/yaourtrc':
    source => 'puppet:///modules/lunaryorn/yaourtrc',
  }

  package { 'yaourt':
    ensure  => latest,
    require => [File['/etc/yaourtrc'],
                Package['pacman'],
                File['/etc/pacman.conf'],
                File['/etc/pacman.d/mirrorlist']],
  }
}
