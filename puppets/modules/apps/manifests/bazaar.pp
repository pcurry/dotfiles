# Class: apps::bazaar
#
# This class installs GNU Bazaar.
#
# Actions:
# - Install Bazaar
class apps::bazaar {

  $package = $::operatingsystem ? {
    'Darwin' => 'bazaar',
    default  => 'bzr',
  }

  package { $package:
    ensure   => latest,
  }
}
