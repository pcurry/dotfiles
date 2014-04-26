# Class: lunaryorn::user::settings.
#
# This class sets my personal user settings.
#
# Actions:
# - Change my login shell to Zsh
# - Configure an OS X system with lunaryorn::user_configuration::osx
# - On OS X, install Source Code Pro for the $user
# - Configure a Gnome system with lunaryorn::user_configuration::gnome
class lunaryorn::user::settings {
  require lunaryorn

  if $::id != $::lunaryorn::user_name {
    $exec_user = $::lunaryorn::user_name
  }

  Exec {
    user => $exec_user,
  }

  $home = $::lunaryorn::home_directory

  if defined(Class['apps::zsh']) {
    $shell_requires = Class['apps::zsh']
  }

  if $::id == 'root' {
    exec { "chsh -s /bin/zsh ${::lunaryorn::user_name}":
      path    => ['/usr/bin', '/usr/sbin', '/bin', '/sbin'],
      user    => undef,
      require => $shell_requires,
    }
  }
  else {
    notice('You need root privileges to change the shell')
  }

  # System-specific user configuration
  if $::operatingsystem == 'Darwin' {
    include lunaryorn::user::settings::osx

    # Install the awesome Source Code Pro font
    $sourcecodepro_url = 'http://sourceforge.net/projects/sourcecodepro.adobe/files/SourceCodePro_FontsOnly-1.017.zip/download'
    $sourcecodepro_archive = "${home}/Downloads/SourceCodePro.zip"
    exec { 'Download Source Code Pro':
      command => "curl -L -s -o ${sourcecodepro_archive} ${sourcecodepro_url}",
      creates => "${home}/Library/Fonts/SourceCodePro-Regular.otf",
      path    => ['/usr/bin', '/bin'],
    }

    exec { 'Install Source Code Pro':
      command => "unzip -oj ${sourcecodepro_archive} '*.otf' -d ${home}/Library/Fonts",
      creates => "${home}/Library/Fonts/SourceCodePro-Regular.otf",
      path    => ['/usr/bin'],
      require => Exec['Download Source Code Pro'],
    }

    # Spelling dictionaries for Hunspell
    $english_url = 'http://sourceforge.net/projects/aoo-extensions/files/17102/3/dict-en.oxt/download'
    $english_archive = "/tmp/dict-en.zip"
    $english_dicts = ['en_AU.aff',
                      'en_AU.dic',
                      'en_CA.aff',
                      'en_CA.dic',
                      'en_GB.aff',
                      'en_GB.dic',
                      'en_US.aff',
                      'en_US.dic',
                      'en_ZA.aff',
                      'en_ZA.dic',]
    $english_files = join($english_dicts, ' ')

    exec { 'lunaryorn::user::settings::download_english_dicts':
      command => "curl -L -s -o ${english_archive} ${english_url}",
      creates => "${home}/Library/Spelling/en_GB.dic",
      path    => ['/usr/bin'],
    }

    exec { 'lunaryorn::user::settings::install_english_dicts':
      command => "unzip ${english_archive} ${english_files} -d ${home}/Library/Spelling",
      creates => "${home}/Library/Spelling/en_GB.dic",
      path    => ['/usr/bin'],
      require => Exec['lunaryorn::user::settings::download_english_dicts'],
    }

    # Symlinks for default dictionary, needed by Emacs
    file { "${home}/Library/Spelling/default.dic":
      ensure  => link,
      target  => 'en_GB.dic',
      require => Exec['lunaryorn::user::settings::install_english_dicts'],
    }
    file { "${home}/Library/Spelling/default.aff":
      ensure  => link,
      target  => 'en_GB.aff',
      require => Exec['lunaryorn::user::settings::install_english_dicts'],
    }

    $german_url = 'http://sourceforge.net/projects/aoo-extensions/files/1075/13/dict-de_de-frami_2013-12-06.oxt/download'
    $german_archive = "/tmp/dict-de_de-frami.zip"
    exec { 'lunaryorn::user::settings::download_german_dicts':
      command => "curl -L -s -o ${german_archive} ${german_url}",
      creates => "${home}/Library/Spelling/de_DE_frami.dic",
      path    => ['/usr/bin'],
    }

    exec { 'lunaryorn::user::settings::install_german_dicts':
      command => "unzip -j ${german_archive} de_DE_frami/de_DE_frami.aff de_DE_frami/de_DE_frami.dic -d ${home}/Library/Spelling",
      creates => "${home}/Library/Spelling/de_DE_frami.dic",
      path    => ['/usr/bin'],
      require => Exec['lunaryorn::user::settings::download_german_dicts'],
    }

    # Symbols for the standard locale name
    file { "${home}/Library/Spelling/de_DE.dic":
      ensure  => link,
      target  => 'de_DE.dic',
      require => Exec['lunaryorn::user::settings::install_german_dicts'],
    }
    file { "${home}/Library/Spelling/de_DE.aff":
      ensure  => link,
      target  => 'de_DE.aff',
      require => Exec['lunaryorn::user::settings::install_german_dicts'],
    }
  }
}
