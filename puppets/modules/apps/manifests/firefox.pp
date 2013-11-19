# Class: apps::firefox
#
# This class installs Mozilla Firefox
#
# Parameters:
# - The $language of Firefox
#
# Actions:
# - Install Mozilla Firefox
class apps::firefox($language) {
  case $::operatingsystem {
    'Darwin': {

      $version = '25.0.1'
      $url = "http://download-installer.cdn.mozilla.net/pub/firefox/releases/${version}/mac/${language}/Firefox%20${version}.dmg"

      package { "firefox-${version}":
        ensure   => installed,
        provider => appdmg,
        source   => $url,
        alias    => 'firefox',
      }
    }
    'Archlinux': {
      package { 'firefox': ensure => latest }

      if $language {
        # Firefox and the German language
        $lang_code = downcase($language)
        $lang_pack = "firefox-i18n-${lang_code}"

        package { $lang_pack:
          ensure  => latest,
          require => Package['firefox'],
        }
      }

      if defined(Class['gnome']) {
        # Plug the Gnome keyring into Firefox if Gnome is available
        package { 'firefox-gnome-keyring':
          ensure  => latest,
          require => [Class['gnome'], Package['firefox']]
        }
      }
    }
    default: {
      warning("Don't know how to install Firefoxx on ${::operatingsystem}")
    }
  }
}
