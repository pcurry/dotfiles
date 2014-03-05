# Definition: system::timezone
#
# This definition sets the timezone of the system.
#
# Parameters:
# - The $timezone (title parameter)
define system::timezone($timezone = $title) {
  case $::operatingsystem {
    'Darwin': {
      exec { "systemsetup -settimezone ${timezone}":
        unless => "systemsetup -gettimezone | grep ${timezone}",
        path   => ['/usr/sbin', '/usr/bin'],
      }
    }
    default: {
      file { '/etc/localtime':
        ensure => link,
        target => "/usr/share/zoneinfo/${timezone}",
        owner  => 'root',
        group  => 'root',
        mode   => '0644',
      }
    }
  }

}
