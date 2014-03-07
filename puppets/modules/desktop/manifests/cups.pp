# Class: desktop::cups
#
# This class installs the CUPS printing system.
class desktop::cups {
  if $::operatingsystem != 'Darwin' {
    package { 'cups':
      ensure => latest,
    }

    service { 'cups':
      ensure  => running,
      enable  => true,
      require => Package['cups'],
    }

    # PDF printer for CUPS
    package { 'cups-pdf':
      ensure  => latest,
      require => Package['cups']
    }

    file_line { 'desktop::cups::systemgroup':
      path    => '/etc/cups/cups-files.conf',
      match   => '^SystemGroup sys root',
      line    => 'SystemGroup sys root wheel',
      require => Package['cups'],
    }
  }
}
