# Class: apps::ocaml
#
# This class installs OCaml.
#
# Parameters:
#
# Actions:
# - Install Ocaml and OPAM.
class apps::ocaml {

  $package = $::operatingsystem ? {
    'Darwin' => 'objective-caml',
    default  => 'ocaml',
  }

  package { $package:
    ensure => latest,
    alias  => 'ocaml',
  }

  package { 'opam':
    ensure  => latest,
    require => Package[$package],
  }
}
