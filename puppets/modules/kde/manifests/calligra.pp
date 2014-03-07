# Class: kde::calligra
#
# This class installs the KDE office suite calligra.
class kde::calligra {
  require kde

  package { 'calligra-meta':
    ensure => latest
  }
}
