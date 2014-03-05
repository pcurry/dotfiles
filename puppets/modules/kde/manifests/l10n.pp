# Definition: kde::l10n
#
# This class enables KDE localization packages.
#
# Parameters:
# - The $language to use (title parameter)
#
# Actions:
# - Install the KDE l10n package for $language
#
# Requires:
#
# Sample Usage:
define kde::l10n($language = $title) {
  $package = "kde-l10n-${language}"

  package { $package:
    ensure => latest
  }
}
