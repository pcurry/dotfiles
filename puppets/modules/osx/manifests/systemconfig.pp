# Definition: osx::systemconfig
#
# This class changes system configuration settings.
#
# Parameters:
# - The $preference setting (title parameter)
# - The new $value for $preference
#
# Actions:
# - Change system preferences
#
# Requires:
#
# Sample Usage:
define osx::systemconfig($value, $preference = $title) {
  if $::operatingsystem != 'Darwin' {
    fail('Systemconfig is only supported on OS X')
  }

  if $::id != 'root' {
    warning('Cannot change system preferences as non-root')
  }
  else {
    exec { "scutil -set ${preference} ${value}":
      unless => "scutil -get ${preference} | egrep '^${value}$'",
      path   => ['/usr/sbin', '/usr/bin'],
    }
  }
}
