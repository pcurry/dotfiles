# Class: apps::bazaar
#
# This class installs GNU Bazaar.
#
# Actions:
# - Install Bazaar
class apps::bazaar {

  if $::operatingsystem == 'Darwin' {
    Package {
      provider => homebrew
    }
  }

  $package = $::operatingsystem ? {
    'Darwin' => 'bazaar',
    default  => 'bzr',
  }

  package { $package:
    ensure   => latest,
  }
}
