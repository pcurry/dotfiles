# Class: desktop::fonts::terminus
#
# This class installs and configures the Terminus font.
#
# Parameters:
# - Whether to $enable Terminus as desktop font (defaults to false). If false,
#   the font is only available as console font.
#
# Actions:
# - Install the Terminus font
# - Enable Terminus, if $enable is true.
class desktop::fonts::terminus($enable = false) {
  package { 'terminus-font':
    ensure => latest,
  }

  if $enable {
    $ensure = enabled
  }
  else {
    $ensure = disabled
  }

  desktop::fonts::fontconfig { '75-ttf-inconsolata':
    ensure  => $ensure,
    require => Package['terminus-font']
  }
}
