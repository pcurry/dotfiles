# Class: lunaryorn::packages
#
# Install packages I use
#
# Parameters:
# - The $user_name (for OS X Homebrew)
#
# Actions:
# - On OS X, install Homebrew if possible
# - On Arch Linux, configure Pacman
# - On Linux, install Zsh, Openssh, Sudo, NetworkManager, Gnome and various
#   fonts
# - Install GNU Stow for this dotfile repo
# - Install popular VCS programs
# - Install GNU Texinfo to build Emacs documentation
# - Install Aspell as Emacs spell checker
# - Install Emacs snapshot
# - Install Google Chrome
# - Install Dropbox
# - Install Ocaml and its package manager Opam
# - Install Leinigen for Clojure development
# - Install Github command line tools hub and ghi
# - Install Ag, the silver searcher for fast code searchx
# - Install pwgen to generate passwords
# - Install nmap for network diagnosis
# - Install youtube-dl to download youtube videos
# - Install fasd for fast directory jumps in Zsh
class lunaryorn::packages(
  $homebrew_user = $lunaryorn::params::user_name
  ) inherits lunaryorn::params {

  if ($::operatingsystem == 'Darwin') or ($::id == 'root') {
    # We must be root to install packages on Linux

    if $::operatingsystem == 'Darwin' {
      # Setup Homebrew
      class { 'homebrew':
        user => $homebrew_user
      }

      # Enable our Homebrew provider
      Package {
        provider => homebrew
      }

      if $::id == 'root' {
        # Install Homebrew if possible
        require homebrew::install
      }
      else {
        notice('We are not root, and cannot install Homebrew')
      }
    }
    else {
      require linux::package_manager
    }

    # System drivers
    if $::operatingsystem == 'Archlinux' {
      package { 'dkms-8192cu':  # A much better driver for my Edimax WLAN device
        ensure => latest,
        before => Class['desktop::networkmanager']
      }

      package { 'ntfs-3g':      # NTFS driver for Linux
        ensure => latest
      }
    }

    # System services and tools
    include apps::zsh
    include apps::sudo
    include apps::openssh
    if $::operatingsystem != 'Darwin' {
      # Sound system on Linux
      include linux::alsa

      # Network management on Linux
      include desktop::networkmanager
      package { ['wireless_tools', 'net-tools']:
        ensure => latest
      }
    }

    # Desktop environment
    if $::operatingsystem != 'Darwin' {
      # Install a desktop environment on Linux
      include kde
      include kde::gtk
      include kde::appmenu
      include kde::networkmanager
      include kde::telepathy
      include kde::kdm
      include kde::tools
      include kde::k3b
      include kde::amarok
      include kde::dropbox
      kde::l10n { [ 'de', 'en_gb' ]: }
    }

    # Desktop services
    if $::operatingsystem != 'Darwin' {
      # Desktop services on Linux
      include desktop::telepathy
      include desktop::gstreamer
      include desktop::gstreamer::legacy
      include desktop::fonts
    }

    # Applications
    include apps::dropbox
    include apps::google_chrome
    include apps::virtualbox

    # Dotfile tools
    package { 'stow': ensure => latest } # Symlink manager
    if $::operatingsystem != 'Darwin' {
      # On OS X, we install puppet as local Gem
      package { 'puppet': ensure => latest }
    }

    # Emacs
    class {'apps::aspell':      # Emacs spell checker
      languages => ['en', 'de']
    }
    include apps::emacs_snapshot
    include apps::mu4e
    package { 'ctags': ensure => latest } # Tags for Emacs

    # Programming languages and environments
    include apps::python2
    include apps::ocaml
    include apps::clojure
    include apps::haskell_platform

    # Developer tools
    include apps::git
    include apps::mercurial
    include apps::bazaar
    include apps::github_cli_tools
    package { 'the_silver_searcher': # Grep for source code
      ensure => latest
    }

    # Misc packages
    $isync = $::operatingsystem ? {
      'Archlinux' => 'isync-git', # isync itself is outdated in AUR
      default     => 'isync'
    }

    $misc_packages = ['pwgen',           # Password generator
                      'nmap',            # Port scanner for diagnostics
                      'youtube-dl',      # Youtube downloader
                      $isync,            # IMAP mail sync
                      ]
    package { $misc_packages: ensure => latest }
  }
  else {
    notice('We need root privileges to install packages')
  }
}
