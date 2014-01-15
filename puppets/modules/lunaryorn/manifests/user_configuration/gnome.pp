# Class: lunaryorn::user_configuration::gnome
#
# This class sets my Gnome settings.
#
# Parameters:
# - The $user whose settings to change
#
# Actions:
# - Disable the blinking cursor
# - Set document and monospace fonts
# - Set keyboard layouts and options
# - Configure the Gnome Shell
# - Configure NetworkManager, PackageKit and Eye of Gnome
class lunaryorn::user_configuration::gnome(
  $user = $lunaryorn::params::user_name
  ) inherits lunaryorn::params {

  Gnome::Settings {
    user => $user,
  }

  # Desktop settings
  gnome::settings { 'Disable blinking cursor':
    schema => 'org.gnome.desktop.interface',
    key    => 'cursor-blink',
    value  => false,
  }

  gnome::settings { 'Show date in top bar':
    schema => 'org.gnome.desktop.interface',
    key    => 'clock-show-date',
    value  => true,
  }

  # Fonts
  gnome::settings { 'Use DejaVu Sans as document font':
    schema => 'org.gnome.desktop.interface',
    key    => 'document-font-name',
    value  => "'DejaVu Sans 11'",
  }

  gnome::settings { 'Use Source Code Pro as monospace font':
    schema => 'org.gnome.desktop.interface',
    key    => 'monospace-font-name',
    value  => "'Source Code Pro Medium 10'",
  }

  # Keyboard settings
  gnome::settings { 'Emacs keybindings in Gnome':
    schema => 'org.gnome.desktop.interface',
    key    => 'gtk-key-theme',
    value  => 'Emacs',
  }

  gnome::settings { 'Select input sources':
    schema => 'org.gnome.desktop.input-sources',
    key    => 'sources',
    value  => "[('xkb', 'us+altgr-intl'), ('xkb', 'de')]",
  }

  gnome::settings { 'Remap Capslock to Control':
    schema => 'org.gnome.desktop.input-sources',
    key    => 'xkb-options',
    value  => "['ctrl:nocaps']",
  }

  # Shell
  gnome::settings { 'Switch between windows of the current workspace only':
    schema => 'org.gnome.shell.app-switcher',
    key    => 'current-workspace-only',
    value  => true,
  }

  gnome::settings { 'Always show logout item':
    schema => 'org.gnome.shell',
    key    => 'always-show-log-out',
    value  => true,
  }

  # Network manager
  gnome::settings { 'Disable notification about new Wifis':
    schema => 'org.gnome.nm-applet',
    key    => 'suppress-wireless-networks-available',
    value => true,
  }

  # EoG
  gnome::settings { 'Disable EoG trash confirmation':
    schema => 'org.gnome.eog.ui',
    key    => 'disable-trash-confirmation',
    value  => true,
  }

  # PackageKit settings
  gnome::settings { 'Enable auto-remove in PackageKit':
    schema => 'org.gnome.packagekit',
    key    => 'enable-autoremove',
    value  => true,
  }

  gnome::settings { 'Show all repositories in PackageKit':
    schema => 'org.gnome.packagekit',
    key    => 'repo-show-details',
    value  => true,
  }

  gnome::settings { 'Search in package details':
    schema => 'org.gnome.packagekit',
    key    => 'search-mode',
    value  => "'details'",
  }
}
