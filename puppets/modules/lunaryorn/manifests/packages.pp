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
    default : {
    }
  }

  # For our dotfiles
  package { 'stow': ensure => latest }

  # Puppet for provisioning.  On OS X, we install it as user-local Gem via
  # bundler, so we don't have it here
  unless $::operatingsystem == 'Darwin' {
    package { 'puppet': ensure => latest}
  }

  # Zsh.  On OS X, it's preinstalled
  unless $::operatingsystem == 'Darwin' {
    package { 'zsh': ensure => latest }
  }
  # Fast directory switching for Zsh
  package { 'fasd': ensure  => latest }

  # VCSs
  package { ['git', 'mercurial', 'bazaar']: ensure => latest }

  # Emacs snapshot
  case $::operatingsystem {
    'Darwin': {
      package { 'emacs':
        ensure          => latest,
        # Install Emacs trunk, with Cocoa support, better colors, and GNU TLS
        # built-in
        install_options => ['--HEAD', '--cocoa', '--srgb', '--with-gnutls'],
        require         => Package['aspell'] # For flyspell
      }

      # Put Emacs into the applications folder
      file { '/Applications/Emacs.app':
        ensure  => link,
        target  => "${::homebrew::prefix}/Cellar/emacs/HEAD/Emacs.app",
        require => [Class['homebrew'], Package['emacs']],
      }
    }
    default: {
      warning("Don't know how to install Emacs snapshot on $::operatingsystem")
    }
  }

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

  # Google Chrome
  case $::operatingsystem {
    'Darwin': {
      package { 'google-chrome':
        ensure   => installed,
        provider => appdmg,
        source   => 'https://dl.google.com/chrome/mac/stable/GGRO/googlechrome.dmg',
      }
    }
    default: {
      warning("Don't know how to install Chrome on ${::operatingsystem}")
    }
  }

  # Dropbox
  case $::operatingsystem {
    'Darwin': {
      # Dropbox has a silly non-standard OS X installer
      warning('Please download and install Dropbox manually from https://www.dropbox.com/downloading?os=mac.')
    }
    default: {
      warning("Don't know how to install Dropbox on ${::operatingsystem}")
    }
  }

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
  package { ['hub', 'ghi', 'the_silver_searcher']: ensure => latest }

  # Misc packages
  package { [ 'pwgen',           # Password generator
              'nmap',            # Port scanner for diagnostics
              'youtube-dl',      # Youtube downloader
              ]:
    ensure => latest
  }
}
