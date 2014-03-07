# Class: desktop::fonts::liberation
#
# This class installs and configures the Liberation fonts.
#
# Parameters:
# - Whether to enable $generic_aliases or not.  Set the parameter to false, if
#   you wish to use the original Microsoft fonts.
#
# Actions:
# - Install the Liberation font package
# - Enable the Liberation fontconfig settings, if $generic_aliases is true
class desktop::fonts::liberation($generic_aliases = true) {
  package { 'ttf-liberation':
    ensure => latest,
  }

  $configs = ['30-ttf-liberation-mono.conf',
              '30-ttf-liberation-sans.conf',
              '30-ttf-liberation-serif.conf',
              ]

  if $generic_aliases {
    $ensure = enabled
  }
  else {
    $ensure = disabled
  }

  desktop::fonts::fontconfig { $configs:
    ensure  => $ensure,
    require => Package['ttf-liberation']
  }
}
