# Class: gnome::gdm
#
# Enable GDM
class gnome::gdm {
  service { 'gdm':
    ensure  => running,
    enable  => true,
    require => [Class['gnome'], Class['x11']],
  }
}
