# Class: desktop::cups::hp
#
# This class installs HP support for CUPS.
#
# Actions:
# - Install hplip for HP printer support
class desktop::cups::hp {

  include desktop::cups

  # Optional dependencies
  $dependencies = [ 'python2-dbus',   # DBUS support
                    'python2-notify', # Desktop notifications
                    'rpcbind',        # Network support
                    'python2-pyqt4',  # HP Toolbox
                    'python2-gobject2',
                    ]

  package { $dependencies:
    ensure => latest
  }

  # hplip
  package { 'hplip':
    ensure  => latest,
    require => [Package['cups'], Package[$dependencies]],
    # Before CUPS service is started, to avoid the need to restart the service
    before  => Service['cups']
  }

  # Binary driver blobs for hplip
  package { 'hplip-plugin':
    ensure  => latest,
    require => Package['hplip']
  }
}
