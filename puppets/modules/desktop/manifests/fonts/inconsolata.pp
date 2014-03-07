# Class: desktop::fonts::inconsolata
#
# This class installs and configures the Inconsolata font.
#
# Parameters:
# - Whether to enable the $generic_alias of Inconsolata (defaults to false)
#
# Actions:
# - Install the Inconsolata font
# - Alias Inconsolata for the generic monospace font, if $generic_alias are
#   enabled
class desktop::fonts::inconsolata($generic_alias = false) {
  package { 'ttf-inconsolata':
    ensure => latest,
  }

  if $generic_alias {
    $ensure = enabled
  }
  else {
    $ensure = disabled
  }

  desktop::fonts::fontconfig { '75-ttf-inconsolata':
    ensure  => $ensure,
    require => Package['ttf-inconsolata']
  }
}
