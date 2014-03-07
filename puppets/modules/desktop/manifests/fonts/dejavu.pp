# Class: desktop::fonts::dejavu
#
# This class installs and configures the Dejavu fonts.
#
# Actions:
# - Install the DejaVu font package
# - Enable the DejaVu fontconfig settings
class desktop::fonts::dejavu {
  package { 'ttf-dejavu':
    ensure => latest,
  }

  $configs = ['20-unhint-small-dejavu-sans',
              '20-unhint-small-dejavu-sans-mono',
              '20-unhint-small-dejavu-serif',
              # Aliases
              '57-dejavu-sans',
              '57-dejavu-sans-mono',
              '57-dejavu-serif',
              ]

  desktop::fonts::fontconfig { $configs:
    ensure  => enabled,
    require => Package['ttf-dejavu']
  }
}
