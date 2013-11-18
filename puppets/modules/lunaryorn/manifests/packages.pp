# Class: lunaryorn::packages
#
# Install packages I use
#
# Parameters:
# - The $user_name (for OS X Homebrew)
#
# Actions:
# - Install GNU Stow for this dotfile repo
class lunaryorn::packages(
  $homebrew_user = $lunaryorn::params::user_name
) inherits lunaryorn::params {

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
        include homebrew::install

        Class['homebrew::install'] -> Package<| |>
      }
      else {
        notice('We are not root, and cannot install Homebrew')
      }
    }
    'Archlinux': {
      file { '/etc/pacman.conf':
        source => 'puppet:///modules/lunaryorn/pacman.conf',
        owner  => 'root',
        group  => 'root',
        mode   => '0644',
        notify => Exec['pacman -Sy'],
      }

      # Refresh package lists if the configuration changed
      exec { 'pacman -Sy':
        path        => ['/usr/bin/'],
        refreshonly => true,
      }

      # Configure pacman before installing anything
      File['/etc/pacman.conf'] -> Package<| |>
    }
    default : {
    }
  }

  unless $::operatingsystem == 'Darwin' {
    # The following packages are pre-installed on OS X

    # My preferred shell
    package { 'zsh': ensure => latest }

    # OpenSSH for remote access
    package { 'openssh': ensure => latest }
    service { 'sshd.socket':
      ensure  => running,
      enable  => true,
      require => Package['openssh'],
    }

    # Sudo
    package { 'sudo': ensure => latest }

    # Network Manager
    package { 'networkmanager': ensure => latest }
    service { 'NetworkManager':
      ensure  => running,
      enable  => true,
      require => Package['networkmanager'],
    }

    # Gnome
    include gnome
    include gnome::gdm          # Enable and start Gnome

    # Additional utilities for Gnome
    $gnome_packages = [ 'brasero',     # Disk burner
                        'file-roller', # Archive tool
                        'seahorse',    # Manage keyring
                        'evolution',   # Organizer
                        'nautilus-sendto',
                        'gnome-weather',
                        'gnome-tweak-tool',
                        'gnome-packagekit',
                        'rhythmbox', # Audio player
                        'gst-plugins-ugly', # Media codecs
                        'gst-libav',
                        'libgpod', # iPod support
                        ]
    package { $gnome_packages: ensure => latest }

    package { 'network-manager-applet':
      require => Package['networkmanager']
    }

    # A good fontset for non-OS X systems
    $fonts = [ 'adobe-source-code-pro-fonts',
               'terminus-font',
               'ttf-dejavu',
               'ttf-liberation' ]
    package { $fonts : ensure => latest }
  }

  # For our dotfiles
  package { 'stow': ensure => latest }

  # Puppet for provisioning.  On OS X, we install it as user-local Gem via
  # bundler, so we don't have it here
  unless $::operatingsystem == 'Darwin' {
    package { 'puppet': ensure => latest}
  }

  # VCSs
  $bazaar = $::operatingsystem ? {
    'Darwin' => 'bazaar',
    default  => 'bzr',
  }
  package { ['git', 'mercurial', $bazaar]: ensure => latest }

  # Emacs documentation
  package { 'texinfo': ensure => latest }

  if $::operatingsystem == 'Darwin' {
    # On OS X, link our modern Texinfo to /usr/bin, for convenience
    exec { 'Link Texinfo from Homebrew':
      command     => "${::homebrew::prefix}/bin/brew link --force texinfo",
      user        => $::homebrew::realuser,
      environment => ["USER=${::homebrew::user}",
                      "HOME=/Users/${::homebrew::user}"],
      creates     => "${::homebrew::prefix}/bin/makeinfo",
      require     => [Class['homebrew'], Package['texinfo']],
    }
  }

  # Emacs spell checker
  case $::operatingsystem {
    'Darwin': {
      package { 'aspell':
        ensure          => latest,
        install_options => ['--with-lang-de', '--with-lang-en']
      }
    }
    default: {
      package { ['aspell', 'aspell-de', 'aspell-en']: ensure => latest }
    }
  }

  # Applications
  include apps::emacs_snapshot
  include apps::google_chrome
  include apps::dropbox

  # Ocaml
  $ocaml = $::operatingsystem ? {
    'Darwin' => 'objective-caml',
    default  => 'ocaml',
  }
  package { $ocaml:
    ensure => latest,
    alias  => 'ocaml',
  }
  package { 'opam':
    ensure  => latest,
    require => Package['ocaml']
  }

  # Clojure
  package { 'leiningen': ensure => latest}

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
  package { [ 'pwgen',           # Password generator
              'nmap',            # Port scanner for diagnostics
              'youtube-dl',      # Youtube downloader
              'fasd',            # Fast directory switching for Zsh
              ]:
    ensure => latest
  }
}
