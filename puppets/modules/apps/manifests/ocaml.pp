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

  if $::operatingsystem != 'Darwin' {
    # On Linux, we need to install ocamlfind separately
    package { 'ocaml-findlib':
      ensure => latest,
    }
  }

  package { 'opam':
    ensure  => latest,
    require => Package[$package],
  }
}
