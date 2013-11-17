# Class: apps::emacs_snapshot
#
# Install an Emacs Bazaar snapshot
class apps::emacs_snapshot {
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
    'Archlinux': {
      package { 'emacs-bzr':
        ensure  => latest,
        require => Package['bzr'],
        alias   => 'emacs',
      }
    }
    default: {
      fail("Cannot install Emacs snapshot on ${::operatingsystem}")
    }
  }
}
