# Definition: system::timezone
#
# This definition sets the timezone of the system.
#
# Parameters:
# - The $timezone (title parameter)
define system::timezone($timezone = $title) {

  $command = $::operatingsystem ? {
    'Darwin' => "systemsetup -settimezone ${timezone}",
    default  => "timedatectl set-timezone ${timezone}",
  }
  $is_enabled = $::operatingsystem ? {
    'Darwin' => "systemsetup -gettimezone | grep -q ${timezone}",
    default  => "timedatectl status | grep -q 'Time zone: ${timezone} '"
  }

  exec { 'system::timezone':
    command => $command,
    unless  => $is_enabled,
    path    => ['/usr/bin', '/usr/sbin', '/bin', '/sbin'],
  }
}
