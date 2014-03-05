# Class: kde::k3b
#
# This class installs the K3b CD/DVD authoring application.
#
# Actions:
# - Install optional dependencies for K3b
# - Install k3b
class kde::k3b {
  require kde

  $dependencies = [ 'dvd+rw-tools', # DVD burning support
                    'transcode',    # MPEG conversion support
                    'cdrdao',       # Disk-at-once support
                    'cdparanoia',   # CD ripping support
                    ]
  package { $dependencies:
    ensure => latest
  }

  package { 'k3b':
    ensure  => latest,
    require => Package[$dependencies],
  }
}
