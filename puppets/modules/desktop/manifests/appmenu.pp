# Class: desktop::appmenu
#
# This class installs support for global application menus.
#
# Actions:
# - Install appmenu support for Qt and Gtk
class desktop::appmenu {
  $packages = [ 'appmenu-qt', ]

  package { $packages:
    ensure => latest,
  }
}
