# Class: kde::tools
#
# This class installs 3rd party tools for KDE.
#
# Actions:
# - Install KCM module for Gtk applications
# - Install Yakuake, a Quake-like drop-down terminal
class kde::tools {
  require kde

  $packages = [ 'yakuake', ]

  package { $packages:
    ensure  => latest,
  }
}
