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

    case $::operatingsystem {
      'Darwin': {
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
      default: {
        require lunaryorn::packages::linux
      }
    }

    # For our dotfiles
    package { 'stow': ensure => latest }

    # VCSs
    $bazaar = $::operatingsystem ? {
      'Darwin' => 'bazaar',
      default  => 'bzr',
    }
    package { ['git', 'mercurial', $bazaar]: ensure => latest }

    # Emacs tag browsing
    package { 'ctags': ensure => latest }

    # Applications
    class {'apps::aspell':      # Emacs spell checker
      languages => ['en', 'de']
    }
    include apps::emacs_snapshot
    include apps::mu4e
    include apps::dropbox
    include apps::google_chrome

    include apps::python2
    include apps::ocaml
    include apps::clojure
    include apps::haskell_platform

    include apps::virtualbox

    # Developer tools:
    # hub: Github from CLI
    # ghi: Github issues from CLI
    # the_silver_searcher: Grep for Code!
    package { ['hub', 'the_silver_searcher']: ensure => latest }
    $ghi_requires = $::operatingsystem ? {
      'Archlinux' => Package['wget'], # AUR ghi needs wget to install
      default     => [],
    }
    package { 'ghi':
      require => $ghi_requires,
    }

    if $::operatingsystem == 'Archlinux' {
      package { 'wget': ensure => latest }
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
                      'fasd',            # Fast directory switching for Zsh
                      ]
    package { $misc_packages: ensure => latest }
  }
  else {
    notice('We need root privileges to install packages')
  }
}
