# Class: lunaryorn::system
#
# This class installs and configures the system.
class lunaryorn::system {
  require lunaryorn

  if $::id != 'root' {
    fail('You must be root to install the system!')
  }


  case $::operatingsystem {
    'Darwin': {
      if $::macosx_productversion_major != '10.9' {
        fail('Get Mavericks, stupid!')
      }
      if ! $::osx_file_vault_active {
        fail('Hey man, put your files in the vault!')
      }

      class { 'homebrew':
        user => $::lunaryorn::user_name
      }

      # Install enable enable Homebrew for package management
      require homebrew::install
      Package {
        provider => homebrew,
      }

      # Disable boot sound
      exec { 'Disable beep on boot':
        command => '/usr/sbin/nvram SystemAudioVolume=" "',
      }
    }
    default: {
    }
  }

  # System services and tools
  include apps::zsh
  include apps::openssh

  # Applications
  include apps::dropbox
  include apps::google_chrome
  include apps::virtualbox
  include apps::inkscape

  # Dotfile tools
  package { 'stow': ensure => latest } # Symlink manager
  if $::operatingsystem != 'Darwin' {
    # On OS X, we install puppet as local Gem
    package { 'puppet': ensure => latest }
  }

  # Emacs
  include apps::emacs_snapshot
  # Tags and spell check for Emacs
  package { ['ctags', 'hunspell']: ensure => latest }
  if $::operatingsystem != 'Darwin' {
    # On OS X we install dictionaries per user
    package { ['hunspell-de', 'hunspell-en']: ensure => latest }
  }

  # Programming languages and environments
  include apps::python2
  include apps::ocaml
  include apps::ghc
  include apps::texlive

  # Developer tools
  include apps::git
  include apps::mercurial
  include apps::bazaar
  include apps::github_cli_tools
  package { 'the_silver_searcher': # Grep for source code
    ensure => latest
  }

  # Misc packages
  $misc_packages = ['pwgen',           # Password generator
                    'nmap',            # Port scanner for diagnostics
                    'youtube-dl',      # Youtube downloader
                    ]
  package { $misc_packages: ensure => latest }
}
