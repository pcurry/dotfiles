# Class: apps::github_cli_tools
#
# This class installs various Github command line tools.
#
# Actions:
# - Install GHI, to access Github issues from the command line
# - Install Hub, for Github-enhanced Git commands
class apps::github_cli_tools {
  $hub = 'hub'
  $ghi = 'ghi'

  package { [$ghi, $hub]:
    ensure => latest
  }
}
