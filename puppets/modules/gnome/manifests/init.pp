# Class: gnome
#
# This class installs Gnome.
#
# Parameters:
#
# Actions:
# - Install Telepathy and Gnome Base
class gnome {
  require x11

  $telepathy_packages = ['telepathy-gabble',
                         'telepathy-haze',
                         'telepathy-idle',
                         'telepathy-rakia',
                         'telepathy-salut',]
  package { $telepathy_packages: ensure => latest }

  $base_packages = ['baobab',
                    'empathy',
                    'eog',
                    'epiphany',
                    'evince',
                    'gdm',
                    'gnome-backgrounds',
                    'gnome-calculator',
                    'gnome-contacts',
                    'gnome-control-center',
                    'gnome-desktop',
                    'gnome-dictionary',
                    'gnome-disk-utility',
                    'gnome-font-viewer',
                    'gnome-icon-theme',
                    'gnome-icon-theme-extras',
                    'gnome-icon-theme-symbolic',
                    'gnome-keyring',
                    'gnome-screenshot',
                    'gnome-session',
                    'gnome-settings-daemon',
                    'gnome-shell',
                    'gnome-shell-extensions',
                    'gnome-system-log',
                    'gnome-system-monitor',
                    'gnome-terminal',
                    'gnome-themes-standard',
                    'gnome-user-docs',
                    'gnome-user-share',
                    'grilo-plugins',
                    'gucharmap',
                    'mousetweaks',
                    'mutter',
                    'nautilus',
                    'sushi',
                    'totem',
                    'totem-plugin',
                    'tracker',
                    'vino',
                    'xdg-user-dirs-gtk',
                    'yelp',]

  package { $base_packages: ensure => latest }
}
