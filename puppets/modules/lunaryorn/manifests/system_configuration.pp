# Class: lunaryorn::system_configuration
#
# This class sets general settings for my systems.
#
# Parameters:
#
# Actions:
# - Set the hostname of the system
# - Set the system timezone
# - On Linux, set the system locale
# - On Linux, allow users of the wheel group to use sudo
# - On OS X, disable the boot beep
class lunaryorn::system_configuration(
  $hostname = $lunaryorn::params::hostname,
  $timezone = $lunaryorn::params::timezone
) inherits lunaryorn::params
{
  if $::id == 'root' {
    unless $hostname {
      fail('No hostname set for this system')
    }

    system::hostname { $hostname: }
    system::timezone { $timezone: }

    if $::operatingsystem != 'Darwin' {
      # Configure system locales on Linux
      $locales = ['de_DE.UTF-8 UTF-8',
                  'de_DE ISO-8859-1',
                  'de_DE@euro ISO-8859-15',
                  'en_GB.UTF-8 UTF-8',
                  'en_GB ISO-8859-1',
                  'en_US.UTF-8 UTF-8',
                  'en_US ISO-8859-1',
                  ]

      class { 'linux::locale':
        system_language => 'de_DE.utf8',
        enabled_locales => $locales,
      }
    }
    else {
      # Disable the annoying beep on boot
      exec { 'Disable beep on boot':
        command => '/usr/sbin/nvram SystemAudioVolume=" "',
      }
    }
  }
  else {
    notice('You need root privileges to configure the system')
  }
}
