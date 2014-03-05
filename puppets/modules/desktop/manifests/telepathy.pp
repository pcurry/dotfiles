# Class: desktop::telepathy
#
# This class installs the Telepathy service framework.
#
# Actions:
# - Install all Telepathy protocols
class desktop::telepathy {
  $protocols = ['telepathy-gabble',
                'telepathy-haze',
                'telepathy-idle',
                'telepathy-rakia',
                'telepathy-salut',]
  package { $protocols:
    ensure => latest,
  }
}
