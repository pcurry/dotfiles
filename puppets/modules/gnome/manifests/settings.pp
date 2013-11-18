# Definition: gnome::settings
#
# This class changes Gnome settings using gsettings.
define gnome::settings(
  $schema,
  $key,
  $value = undef,
  $user = undef,
  $ensure = present) {

  $exec_user = $::id ? {
    $user   => undef,
    default => $user,
  }

  Exec {
    user   => $exec_user,
    path   => ['/usr/bin/', '/bin/'],
  }

  if defined(Class['gnome']) {
    require gnome
  }

  case $ensure {
    present: {
      if $value == undef {
        fail("Cannot write undefined value to ${key} in ${schema}")
      }

      exec { "gsettings set '${schema}' '${key}' \"${value}\"":
        unless => "gsettings get '${schema}' '${key}' | grep -F -x \"${value}\"",
      }
    }
    reset: {
      if $value != undef {
        warning("Ignoring value ${value} when deleting ${key} in ${schema}")
      }

      exec { "gsettings reset '${schema}' '${key}'":
      }
    }
    default: {
      fail("Unsupported argument: ensure => ${ensure}")
    }
  }
}
