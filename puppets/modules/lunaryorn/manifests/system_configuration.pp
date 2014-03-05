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

    # Resource defaults
    File {
      owner  => 'root',
      group  => 'root',
      mode   => '0644',
    }

    if $::operatingsystem != 'Darwin' {
      # Linux specific configuration

      # Locale generation
      file { '/etc/locale.gen':
        source => 'puppet:///modules/lunaryorn/locale.gen',
        notify => Exec['locale-gen'],
      }

      # Re-generate locales if changed.
      exec { 'locale-gen':
        path        => ['/usr/bin', '/bin'],
        refreshonly => true,
      }

      # On OS X, we set the locale settings per user
      file { '/etc/locale.conf':
        source => 'puppet:///modules/lunaryorn/locale.conf'
      }

      # Configure Sudo
      file { '/etc/sudoers.d/10-wheel':
        source  => 'puppet:///modules/lunaryorn/sudo-wheel',
        mode    => '0600',      # Sudo wants very restrictive modes
        require => Package['sudo'],
      }
    }

    if $::operatingsystem == 'Darwin' {
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
