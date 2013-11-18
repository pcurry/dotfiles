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
      }

      # Put Emacs into the applications folder
      file { '/Applications/Emacs.app':
        ensure  => link,
        target  => "${::homebrew::prefix}/Cellar/emacs/HEAD/Emacs.app",
        require => [Class['homebrew'], Package['emacs']],
      }
    }
    'Archlinux': {
      # Versioning of snapshots is unstable, and causes Emacs Bzr to be
      # re-installed on every run with ensure => latest.  Hence, we just make
      # sure that Emacs is installed.  To update it, just remove emacs-bzr
      package { 'emacs-bzr':
        ensure  => present,
        require => Package['bzr'],
        alias   => 'emacs',
      }
    }
    default: {
      fail("Cannot install Emacs snapshot on ${::operatingsystem}")
    }
  }
}
