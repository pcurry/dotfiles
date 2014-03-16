# Class: apps::texlive
#
# This class installs Texlive.
#
# Actions:
# - Install Texlive on Linux
class apps::texlive {
  case $::operatingsystem {
    'Archlinux': {
      $core = 'texlive-core'
      $packages = [ 'texlive-games',
                    'texlive-htmlxml',
                    'texlive-humanities',
                    'texlive-music',
                    'texlive-pictures',
                    'texlive-pstricks',
                    'texlive-publishers',
                    'texlive-science',
                    ]
      $extra = [
                'texlive-bibtexextra',
                'texlive-fontsextra',
                'texlive-formatsextra',
                'texlive-genericextra',
                'texlive-latexextra',
                'texlive-plainextra',
                ]

      package { $core:
        ensure => latest
      }

      package { $packages:
        ensure  => latest,
        require => Package[$core]
      }

      package { $extra:
        ensure  => latest,
        require => Package[$core]
      }
    }
    default: {
      fail("Cannot install Texlive on ${::operatingsystem}!")
    }
  }
}
