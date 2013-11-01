# Definition: osx::defaults
#
# This class changes OS X using the defaults system.
#
# Parameters:
# - The $domain of the setting
# - The $key of the setting
# - The $type of the new $value
# - The new $value of the setting
# - The $user to set the setting for
# - How to $ensure the setting.  Either present to change the setting, or
#   deleted to delete the setting
#
# Actions:
# - Write or delete OS X defaults
#
# Requires:
#
# Sample Usage:
#  osx::defaults { 'Enable battery time':
#    ensure => present,
#    domain => 'com.apple.menuextra.battery',
#    key    => 'ShowTime',
#    type   => string,
#    value  => 'YES',
#  }
define osx::defaults(
  $domain,
  $key,
  $type = undef,
  $value = undef,
  $user = undef,
  $ensure = present) {

  $defaults = '/usr/bin/defaults'
  $real_user = $::id ? {
    $user   => undef,
    default => $user,
  }

  if $::operatingsystem != 'Darwin' {
    fail('Defaults are only supported on OS X')
  }

  case $ensure {
    present : {
      if $value == undef {
        fail("Cannot write undefined value to ${key} in ${domain}")
      }

      # Determine the proper type argument
      $typearg = $type ? {
        string  => '-string',
        boolean => '-boolean',
        integer => '-integer',
        array   => '-array',
        undef   => '',
        default => undef
      }

      # The value as returned by "defaults", to check whether it is already set
      $expected_value = $type ? {
        boolean => $value ? {
          true    => '1',
          false   => '0',
          default => fail("${value} for ${key} in ${domain} is not a valid boolean")
        },
        default => $value
      }

      # Escape values properly
      $valuearg = $type ? {
        string  => "\"${value}\"",
        default => $value
      }

      if $typearg == undef {
        fail("Unsupported type ${type} for ${key} in ${domain}")
      }
      else {
        exec { "${defaults} write ${domain} ${key} ${typearg} ${valuearg}":
          # FIXME: Doesn't handle boolean values
          unless  => "${defaults} read ${domain} ${key} | egrep '^${expected_value}$'",
          user    => $real_user,
        }
      }
    }
    deleted : {
      if $value != undef {
        warning("Ignoring value ${value} when deleting ${key} in ${domain}")
      }
      if $type != undef {
        warning("Ignoring type ${type} when deleting ${key} in ${domain}")
      }

      exec { "${defaults} delete ${domain} ${key}":
        onlyif  => "${defaults} read ${domain} | grep ${key}",
        user    => $real_user
      }

    }
    default: {
      fail("Unsupported argument: ensure => ${ensure}")
    }
  }
}
