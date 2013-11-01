# Setup an OS X system

node default {

  $user = 'swiesner'

  class { 'homebrew':
    user => $user
  }

  # We must have 10.9
  if $::macosx_productversion_major != '10.9' {
    fail('Get Mavericks, stupid!')
  }

  # We also want filefault
  if ! $::osx_file_vault_active {
    fail('Hey man, put your files in the vault!')
  }

  # If we can, let's install Homebrew
  if $::id == 'root' {
    include homebrew::install

    Class['homebrew::install'] -> Package<| |>
  }
  else {
    info('We are not root, and cannot install Homebrew')
  }

  # Switch my shell to Zsh
  if $::id == 'root' {
    exec { "/usr/bin/chsh -s /bin/zsh ${user}": }
  }
  else {
    info('We are not root, and cannot switch your shell')
  }

  # Install all our packages with Homebrew
  Package {
    provider => homebrew,
  }

  # Install our Homebrew packages
  package { [ 'git',             # VCS tools
              'mercurial',
              'bazaar',
              'texinfo',             # Build Emacs docs
              'leiningen',           # Clojure project management
              'objective-caml',      # Ocaml and its package manager
              'opam',
              'nmap',                # The port scanner for network debugging
              'pwgen',               # Password generation from command line
              'the_silver_searcher', # Code search reloaded
              'fasd',                # Fast directory switching for Zsh
              'hub',                 # Control Github from command line
              'ghi',                 # Browse Github issues from command line
              'youtube-dl',          # Youtube video downloader
              ]:
  }

  package { 'emacs':
    ensure          => latest,
    # Install Emacs trunk, with Cocoa support, better colors, and GNU TLS
    # built-in
    install_options => ['--HEAD', '--cocoa', '--srgb', '--with-gnutls'],
    require         => Package['aspell'] # For flyspell
  }

  # Put Emacs into the Applications folder
  file { '/Applications/Emacs.app':
    ensure  => link,
    target  => "${::homebrew::prefix}/Cellar/emacs/HEAD/Emacs.app",
    require => [Class['homebrew'], Package['emacs']],
  }

  # Spell checker for Emacs
  package { 'aspell':
    ensure          => latest,
    install_options => ['--with-lang-de', '--with-lang-en']
  }

  # Link Texinfo to /usr/bin, for convenience
  if $::operatingsystem == 'Darwin' {
    exec { 'roles::devel::emacs::link-texinfo':
      command     => "${::homebrew::prefix}/bin/brew link --force texinfo",
      user        => $::homebrew::realuser,
      environment => ["USER=${::homebrew::user}",
                      "HOME=/Users/${::homebrew::user}"],
      creates     => "${::homebrew::prefix}/bin/makeinfo",
      require     => [Class['homebrew'], Package['texinfo']],
    }
  }
}
