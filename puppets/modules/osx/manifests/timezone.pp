# Definition: osx::timezone
#
# This class changes the OS X timezone
#
# Parameters:
# - The new $timezone
#
# Actions:
# - Change the timezone
#
# Requires:
#
# Sample Usage:
#  osx::timezone { 'Europe/Berlin': }
define osx::timezone($timezone = $title) {
  if $::operatingsystem != 'Darwin' {
    fail('Timezone is only supported on OS X')
  }

  if $::id != 'root' {
    warning('Cannot change timezone as non-root')
  }
  else {
    exec { "systemsetup -settimezone ${timezone}":
      unless => "systemsetup -gettimezone | grep ${timezone}",
      path   => ['/usr/sbin', '/usr/bin'],
    }
  }
}
