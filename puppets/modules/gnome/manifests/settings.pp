# Definition: gnome::settings
#
# This class changes Gnome settings using gsettings.
#
# Parameters:
# - The $schema the $key belongs to
# - The $key to set
# - The new $value, as boolean, or string containing a serialized GVariant
# - The $user whose settings to change
# - The state to $ensure, either present or reset
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
