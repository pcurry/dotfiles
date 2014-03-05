# Class: kde
#
# This class installs KDE.
#
# Parameters:
#
# Actions:
# - Install the Phonon backend
# - Install KDE
class kde() {
  require x11
  require desktop::fonts
  require desktop::gstreamer::legacy

  # Install the Phonon backend
  $phonon_backend = 'phonon-gstreamer'
  package { $phonon_backend:
    ensure  => latest,
    require => Class['desktop::gstreamer::legacy'],
  }

  $kde_packages = [ 'kde-meta-kdebase',
                    'kde-meta-kdeartwork',
                    'kde-meta-kdegraphics',
                    'kde-meta-kdemultimedia',
                    'kde-meta-kdenetwork',
                    'kde-meta-kdeplasma-addons',
                    'kde-meta-kdeutils',
                    'kde-meta-kdetoys',
                    'kde-meta-kdegames',
                    'kde-meta-kdepim',
                    'kde-meta-kdeedu',
                    'kde-meta-kdeaccessibility',]
  package { $kde_packages:
    ensure  => latest,
    require => Package[$phonon_backend],
  }
}
