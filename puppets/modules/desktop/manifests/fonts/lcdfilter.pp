# Class: desktop::fonts::defaulter
#
# This class configures LCD filtering.
#
# Parameters:
# - The $filter to use.  Defaults to the default filter.  Set to false to
#   disable LCD filtering.
#
# Actions:
# - Enable the specified LCD $filter.
class desktop::fonts::lcdfilter($filter = 'default') {

  $filters = ['default', 'legacy', 'light']
  $prefix = '11-lcdfilter-'
  $settings = regsubst($filters, '^.*$', "${prefix}\\1")

  if !$filter {
    desktop::fonts::fontconfig{ $settings:
      ensure => disabled
    }
  }
  else {
    if !($filter in $filters) {
      fail("Unknown filter: ${filter}!")
    }

    $to_enable = "${prefix}${filter}"
    $to_disable = delete($filters, $to_enable)

    desktop::fonts::fontconfig { $to_enable:
      ensure => enabled
    }

    desktop::fonts::fontconfig { $to_disable:
      ensure => disabled
    }
  }
}
