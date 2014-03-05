# Class: kde::dropbox
#
# This class installs Dropbox support for KDE.
#
class kde::dropbox {
  require kde
  require apps::dropbox

  package { 'dolphin-dropbox-plugin-hg':
    ensure => latest
  }
}
