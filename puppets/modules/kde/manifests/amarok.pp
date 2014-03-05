# Class: kde::amarok
#
# This class installs the Amarok music player.
#
# Actions:
# - Install optional dependencies for Amarok
# - Install Amarok
class kde::amarok {
  require kde

  $dependencies = [ 'libgpod',  # iPod support
                    'libmtp',   # MTP support
                    'clamz',    # Amazon music support
                    ]
  package { $dependencies:
    ensure => latest
  }

  package { 'amarok':
    ensure  => latest,
    require => Package[$dependencies],
  }
}
