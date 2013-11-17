# Class: lunaryorn::params
#
# Sets parameters
class lunaryorn::params {
  $timezone = 'Europe/Berlin'
  $hostname = $::operatingsystem ? {
    'Darwin'    => 'lunaryorn-air',
    'Archlinux' => 'lunaryorn-arch',
    default     => undef,
  }

  $user_name = 'swiesner'

  $home_directory = $::operatingsystem ? {
    'Darwin' => "/Users/${user_name}",
    default  => "/home/${user_name}"
  }
}
