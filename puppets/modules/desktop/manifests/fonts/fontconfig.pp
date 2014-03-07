# Definition: desktop::fonts::fontconfig
#
# This class enables or disables fontconfig files.
#
# Parameters:
# - The name of the $config file, without directory and extension
# - How to $ensure the resource
#
# Actions:
# - Enable or disable the specified $config according to $ensure
define desktop::fonts::fontconfig($ensure = installed, $config = $title) {
  if $config =~ /\// {
    fail("Config is a pathname: ${config}!")
  }

  if $config =~ /^(.+)(\..+)?$/ {
    $filename = "${1}.conf"
  }
  $link = "/etc/fonts/conf.d/${filename}"
  $target = "../conf.avail/${filename}"

  case $ensure {
    installed, present, enabled: {
      file { $link:
        ensure => link,
        target => $target
      }
    }
    absent, disabled: {
      file { $link:
        ensure => absent
      }
    }
    default: {
      fail("Unexpected argument to ensure: ${ensure}")
    }
  }
}
