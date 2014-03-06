# Class: lunaryorn
#
# Information about my user account
class lunaryorn {
  $user_name = 'swiesner'

  $home_directory = $::operatingsystem ? {
    'Darwin' => "/Users/${user_name}",
    default  => "/home/${user_name}"
  }
}
