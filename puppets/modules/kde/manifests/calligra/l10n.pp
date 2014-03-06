# Definition: kde::calligra::l10n
#
# This class enables Calligra localization packages.
#
# Parameters:
# - The $language to use (title parameter)
#
# Actions:
# - Install the Calligra l10n package for $language
#
# Requires:
#
# Sample Usage:
define kde::calligra::l10n($language = $title) {
  $package = "calligra-l10n-${language}"

  package { $package:
    ensure => latest
  }
}
