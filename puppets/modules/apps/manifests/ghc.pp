# Class: apps::ghc
#
# This class installs GHC and Cabal install.
#
# Actions:
# - Install GHC
# - Install cabal-install
class apps::ghc {
  if $::operatingsystem == 'Darwin' {
    # Homebrew's formulae for GHC and cabal-install are poorly maintained, so we
    # tell users to use the binary installer
    notice('GHC must be manually installed on OS x!')
    notice('Cabal install must be manually installed on OS X!')
  }
  else {
    $ghc_packages = [ 'ghc', 'cabal-install', # The basics
                      'happy', 'alex'         # And some utilities
                      ]
    package { $ghc_packages: ensure => latest }
  }
}
