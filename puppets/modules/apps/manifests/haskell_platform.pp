# Class: apps::haskell
#
# This class installs Haskell.
#
# Parameters:
#
# Actions:
# - Install GHC
# - Install cabal-install
# - Install some additional libraries
class apps::haskell_platform {
  $common_packages = ['ghc', 'cabal-install']

  package { $common_packages: ensure => latest }

  if $::operatingsystem == 'Archlinux' {
    $additional_packages = ['haddock',
                            'happy',
                            'haskell-regex-posix',
                            'haskell-parsec',
                            'haskell-random',
                            ]
    package { $additional_packages: ensure => latest }
  }
}
