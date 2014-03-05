# Class: kde::appmenu
#
# This class installs support for global application menus in KDE.
#
# Actions:
# - Install a plasma applet to show application menus.
class kde::appmenu {
  require kde
  require desktop::appmenu

  package { 'kdeplasma-applets-menubar':
    ensure => latest,
  }
}
