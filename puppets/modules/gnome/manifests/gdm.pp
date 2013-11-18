# Class: gnome::gdm
#
# This class enables GDM.
#
# Parameters:
#
# Actions:
# - Start and enable the GDM service
class gnome::gdm {
  service { 'gdm':
    ensure  => running,
    enable  => true,
    require => [Class['gnome'], Class['x11']],
  }
}
