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

    # Timezone
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
        }
      }
    }

    # Host name
    case $::operatingsystem {
      'Darwin': {
        osx::systemconfig { 'ComputerName':  value => $hostname }
        osx::systemconfig { 'HostName':      value => $hostname }
        osx::systemconfig { 'LocalHostName': value => hostname }
        osx::defaults { 'Set SMB host name':
          ensure => present,
          domain => '/Library/Preferences/SystemConfiguration/com.apple.smb.server',
          key    => 'NetBIOSName',
          type   => string,
          value  => $hostname,
          user   => 'root',
        }
      }
      default: {
        # Linux systems
        file { '/etc/hostname':
          content => "${hostname}"
        }
      }
    }

    if $::operatingsystem != 'Darwin' {
      # Linux specific configuration

      # Locale generation
      file { '/etc/locale.gen':
        content => "de_DE.UTF-8 UTF-8
de_DE ISO-8859-1
de_DE@euro ISO-8859-15
en_GB.UTF-8 UTF-8
en_GB ISO-8859-1
es_US.UTF-8 UTF-8
es_US ISO-8859-1
",
        notify  => Exec['locale-gen'],
      }

      # Re-generate locales if changed.
      exec { 'locale-gen':
        path        => ['/usr/bin', '/bin'],
        refreshonly => true,
      }

      # On OS X, we set the locale settings per user
      file { '/etc/locale.conf':
        content => 'LANG="de_DE.utf8"',
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
