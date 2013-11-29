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

        # Install current Pacman first
        package { 'pacman':
          ensure  => latest,
          require => [File['/etc/pacman.conf'], Exec['pacman -Sy']]
        }

        # Now generate a list of fast mirrors, with reflector
        package { 'reflector':
          ensure  => latest,
          require => Package['pacman'],
        }

        # We don't use "creates" here, because we want to regenerate the mirror
        # list each time before provisioning
        exec { 'reflector -c Germany -p https -n 10 -f 5 --sort rate --save /etc/pacman.d/mirrorlist':
          path    => ['/usr/bin/', '/bin/'],
          require => Package['reflector'],
          before  => File['/etc/pacman.d/mirrorlist'],
        }

        # Resource anchor for the mirrorlist
        file { '/etc/pacman.d/mirrorlist':
          ensure => present,
        }

        # Now install Yaourt, to automatically install packages from AUR
        package { 'yaourt':
          ensure  => latest,
          require => [Package['pacman'],
                      File['/etc/pacman.conf'],
                      File['/etc/pacman.d/mirrorlist']],
        }

        Package {
          require => Package['yaourt']
        }
      }
      default : {
      }
    }

    unless $::operatingsystem == 'Darwin' {
      # The following packages are pre-installed or not required on OS X

      # Access to NTFS file systems
      package { 'ntfs-3g': ensure => latest }

      # My preferred shell
      package { 'zsh': ensure => latest }

      # Audio tools
      package { ['alsa-utils', 'pavucontrol']: ensure => installed }
      # Save and restore mixer volume on boot
      service { ['alsa-restore', 'alsa-store']:
        enable  => true,
        require => Package['alsa-utils'],
      }

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
      package { 'wireless_tools': ensure => latest }
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
                          'nautilus-dropbox', # Dropbox integration for Nautilus
                          ]
      package { $gnome_packages: ensure => latest }

      package { 'network-manager-applet':
        require => Package['networkmanager']
      }

      # A good fontset for non-OS X systems
      $fonts = [# Essential standard fonts
                'ttf-dejavu',
                'ttf-liberation',
                # Monospace fonts
                'adobe-source-code-pro-fonts',
                'ttf-anonymous-pro',
                'ttf-inconsolata',
                'terminus-font',
                # Misc fonts
                'ttf-droid',
                'ttf-ubuntu-font-family',
                # Windows fonts
                'ttf-ms-fonts',
                'ttf-vista-fonts',
                # Taiwan, Chinese and Arabic fonts
                'ttf-baekmuk',
                'wqy-microhei',
                'wqy-zenhei',
                'ttf-arphic-ukai',
                'ttf-arphic-uming',
                'opendesktop-fonts',
                'ttf-hannom',
                ]
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

    # Emacs tag browsing
    package { 'ctags': ensure => latest }

    # Applications
    class {'apps::aspell':      # Emacs spell checker
      languages => ['en', 'de']
    }
    include apps::emacs_snapshot
    include apps::dropbox
    class { 'apps::firefox':
      language => 'de',
    }

    include apps::ocaml
    include apps::clojure

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
    $misc_packages = [ 'pwgen',           # Password generator
                       'nmap',            # Port scanner for diagnostics
                       'youtube-dl',      # Youtube downloader
                       'fasd',            # Fast directory switching for Zsh
                       ]
    package { $misc_packages: ensure => latest }
  }
  else {
    notice('We need root privileges to install packages')
  }
}
