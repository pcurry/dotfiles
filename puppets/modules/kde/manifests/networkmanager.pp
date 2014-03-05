# Class: kde::networkmanager
#
# This class adds support for NetworkManager to KDE.
#
# Parameters:
#
# Actions:
# - Setup NetworkManager via desktop::networkmanager
# - Install the KDE applet to manage NetworkManager
class kde::networkmanager {
  require desktop::networkmanager

  package { 'kdeplasma-applets-plasma-nm':
    ensure => latest
  }
}
