# Class: lunaryorn::user_configuration
#
# This class sets my personal user settings
#
# Parameters:
# - The $user_name
# - The $home_directory
#
# Actions:
# - Change my login shell to Zsh
# - Configure an OS X system with lunaryorn::user_configuration::osx
# - On OS X, install Source Code Pro for the $user
# - Configure a Gnome system with lunaryorn::user_configuration::gnome
class lunaryorn::user_configuration(
  $user_name = $lunaryorn::params::user_name,
  $home_directory = $lunaryorn::params::home_directory,
  ) inherits lunaryorn::params {

  if $::id != $user_name {
    $exec_user = $user_name
  }

  if defined(Package['zsh']) {
    $shell_requires = [Package['zsh']]
  }
  else {
    $shell_requires = []
  }
  if $::id == 'root' {
    exec { "chsh -s /bin/zsh ${user_name}":
      path    => ['/usr/bin', '/usr/sbin', '/bin', '/sbin'],
      require => $shell_requires,
    }
  }
  else {
    notice('You need root privileges to change the shell')
  }

  # System-specific user configuration
  case $::operatingsystem {
    'Archlinux': {
      include lunaryorn::user_configuration::gnome
    }
    'Darwin': {
      include lunaryorn::user_configuration::osx

      # Install the awesome Source Code Pro font
      $sourcecodepro_url = 'http://sourceforge.net/projects/sourcecodepro.adobe/files/SourceCodePro_FontsOnly-1.017.zip/download'
      $sourcecodepro_archive = "${home_directory}/Downloads/SourceCodePro.zip"
      exec { 'Download Source Code Pro':
        command => "curl -L -s -o ${sourcecodepro_archive} ${sourcecodepro_url}",
        unless  => "test -f ${home_directory}/Library/Fonts/SourceCodePro-Regular.otf",
        creates => $sourcecodepro_archive,
        user    => $exec_user,
        path    => ['/usr/bin', '/bin'],
      }

      exec { 'Install Source Code Pro':
        command => "unzip -oj ${sourcecodepro_archive} '*.otf' -d ${home}/Library/Fonts",
        creates => "${home_directory}/Library/Fonts/SourceCodePro-Regular.otf",
        user    => $exec_user,
        path    => ['/usr/bin'],
        require => Exec['Download Source Code Pro'],
      }
    }
    default: {
      notice("No user configuration for ${::operatingsystem}")
    }
  }
}
