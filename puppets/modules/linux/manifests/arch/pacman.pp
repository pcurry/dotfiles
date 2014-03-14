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

  # The path of the generated mirror list
  $generated_mirrorlist = '/etc/pacman.d/mirrorlist.reflector'

  file { '/etc/pacman.conf':
    content => template('linux/arch/pacman.conf'),
    notify  => Exec['linux::arch::pacman::initialize-repos']
  }

  # Install a default mirrorlist
  file { '/etc/pacman.d/mirrorlist':
    source => 'puppet:///modules/linux/arch/mirrorlist',
  }

  exec { 'linux::arch::pacman::initialize-repos':
    command     => '/usr/bin/pacman -Sy',
    refreshonly => true,
    require     => File['/etc/pacman.d/mirrorlist'],
  }

  # Update pacman
  package { 'pacman':
    ensure  => latest,
    require => [File['/etc/pacman.conf'],
                Exec['linux::arch::pacman::initialize-repos']]
  }

  # Now generate a list of fast mirrors, with reflector
  package { 'reflector':
    ensure  => latest,
    require => Package['pacman'],
  }

  exec { 'linux::arch::pacman::generate-mirrorlist':
    command => "reflector -c Germany -l 10 -f 5 --sort score --save ${generated_mirrorlist}",
    path    => ['/usr/bin/', '/bin/'],
    creates => $generated_mirrorlist,
    notify  => Exec['linux::arch::pacman::update-repos'],
    require => Package['reflector'],
  }

  exec { 'linux::arch::pacman::update-repos':
    command     => '/usr/bin/pacman -Sy',
    refreshonly => true,
    require     => Exec['linux::arch::pacman::generate-mirrorlist']
  }

  # Resource anchor for the generated mirrorlist
  file { $generated_mirrorlist:
    ensure  => present,
    require => [Exec['linux::arch::pacman::generate-mirrorlist'],
                Exec['linux::arch::pacman::update-repos']]
  }

  # Install Yaourt
  file { '/etc/yaourtrc':
    source => 'puppet:///modules/linux/arch/yaourtrc',
  }

  package { 'yaourt':
    ensure  => latest,
    require => [File['/etc/yaourtrc'],
                File[$generated_mirrorlist]],
  }
}
