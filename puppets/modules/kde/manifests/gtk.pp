# Class: kde::gtk
#
# This class provides Gtk support in Gtk
#
# Actions:
# - Install KCM module for Gtk applications
# - Instal KDE themes for Gtk applications
class kde::gtk {
  require kde

  $packages = [ 'kde-gtk-config',
                'oxygen-gtk2',
                'oxygen-gtk3']

  package { $packages:
    ensure  => latest,
  }
}
