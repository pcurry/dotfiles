# Class: apps::aspell
#
# This class installs aspell, a spell checker for Emacs
#
# Parameters:
# - The $languages to install
#
# Actions:
# - Install Aspell and the selected language dictionaries
#
# Requires:
# - Puppet Standard Library
# - Homebrew on OS X
class apps::aspell($languages = ['en']) {
  case $::operatingsystem {
    'Darwin': {
      $language_options = prefix($languages, '--with-lang-')

      package { 'aspell':
        ensure          => latest,
        install_options => $language_options
      }
    }
    default: {
      $language_pkgs = prefix($languages, 'aspell-')

      package { 'aspell': ensure => latest}
      package { $language_pkgs:
        ensure  => latest,
        require => Package['aspell'],
      }
    }
  }
}
