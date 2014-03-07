# Class: desktop::fonts::droid
#
# This class installs and configures the Google Droid fonts.
#
# Parameters:
# - Whether to enable $generic_aliases or not.  Set the parameter to false, if
#   you wish to use the default DejaVu font for generic families
#
# Actions:
# - Install the Droid font package
# - Enable the Droid fontconfig settings, if $generic_aliases are enabled
class desktop::fonts::droid($generic_aliases = true) {
  package { 'ttf-droid':
    ensure => latest,
  }

  $configs = ['60-ttf-droid-sans-mono-fontconfig',
              '65-ttf-droid-kufi-fontconfig',
              '65-ttf-droid-sans-fontconfig',
              '65-ttf-droid-serif-fontconfig',
              ]

  if $generic_aliases {
    $ensure = enabled
  }
  else {
    $ensure = disabled
  }

  desktop::fonts::fontconfig { $configs:
    ensure  => $ensure,
    require => Package['ttf-droid']
  }
}
