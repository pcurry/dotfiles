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
    package { ['ghc', 'cabal-install']: ensure => latest }
  }
}
