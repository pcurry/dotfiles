# Class: apps::emacs_snapshot
#
# This class installs a snapshot build of Emacs.
#
# Parameters:
#
# Actions:
# - Install a snapshot build of Emacs
class apps::emacs_snapshot {
  require apps::git

  case $::operatingsystem {
    'Darwin': {
      package { 'emacs':
        ensure          => latest,
        # Install Emacs trunk, with Cocoa support, and GNU TLS built-in
        install_options => ['--HEAD', '--use-git-head',
                            '--cocoa', '--with-gnutls'],
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
      package { 'emacs-git':
        ensure  => present,
        alias   => 'emacs',
      }
    }
    default: {
      fail("Cannot install Emacs snapshot on ${::operatingsystem}")
    }
  }
}
