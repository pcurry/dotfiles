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
      # Linux

      # Setup package manager
      require linux::package_manager

      # System locales
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

      # Drivers
      include linux::drivers

      # System services and tools
      include linux::pc_speaker
      include linux::alsa
      include desktop::networkmanager
      package { ['wireless_tools', 'net-tools', 'htop']:
        ensure => latest
      }

      # Desktop services
      include desktop::cups
      include desktop::cups::hp
      include desktop::cups::systemconfig
      include desktop::avahi
      include desktop::pulseaudio
      include desktop::telepathy
      include desktop::gstreamer
      include desktop::gstreamer::legacy

      # Fonts
      include desktop::fonts    # Basic fonts and settings
      # Font settings
      include desktop::fonts::subpixel  # Enable RGB subpixel rendering
      include desktop::fonts::lcdfilter # Enable default LCD filter
      # Some extra fonts
      include desktop::fonts::droid
      include desktop::fonts::source_code_pro
      include desktop::fonts::anonymous_pro
      include desktop::fonts::inconsolata

      # Desktop environment
      include kde
      include kde::gtk
      include kde::appmenu
      include kde::networkmanager
      include kde::telepathy
      include kde::kdm
      include kde::k3b
      include kde::amarok
      include kde::dropbox
      include kde::tools
      include kde::calligra
      $kde_languages = [ 'de' ]
      kde::l10n { $kde_languages: }
      kde::calligra::l10n { $kde_languages: }
    }
  }

  # Host name and time zone
  $hostname = $::operatingsystem ? {
    'Darwin'    => 'lunaryorn-air',
    'Archlinux' => 'lunaryorn-arch',
    default     => fail("No hostname for ${::operatingsystem}!")
  }
  system::hostname { $hostname:
    before => Class['system::ntp'],
  }
  system::timezone { 'Europe/Berlin':
    before => Class['system::ntp'],
  }

  include system::ntp

  # System services and tools
  include apps::zsh
  include apps::sudo
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
  package { ['ctags', 'hunspell']: ensure => latest } # Tags for Emacs

  # Programming languages and environments
  include apps::python2
  include apps::ocaml
  include apps::clojure
  include apps::haskell
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
