# Class: kde::tools
#
# This class installs 3rd party tools for KDE.
#
# Actions:
# - Install Yakuake, a Quake-like drop-down terminal
# - Install Gwenview plugins
class kde::tools {
  require kde

  $packages = [ 'yakuake',
                'kipi-plugins', ]

  package { $packages:
    ensure  => latest,
  }
}
