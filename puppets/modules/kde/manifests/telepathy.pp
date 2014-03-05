# Class: kde::telephaty
#
# This class installs Telepathy support for KDE
#
# Actions:
# - Install Telepathy
# - Install the KDE frontends for Telepathy
class kde::telepathy {
  include desktop::telepathy

  package { 'kde-telepathy-meta':
    ensure  => latest,
    require => [Package['kde-meta-kdebase'], Class['desktop::telepathy']],
  }
}
