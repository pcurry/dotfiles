# Class: desktop::fonts::source_code_pro
#
# This class installs the Adobe Source Code Pro fonts.
#
# Actions:
# - Install the Source Code Pro font from Adobe
class desktop::fonts::source_code_pro {
  package { 'adobe-source-code-pro-fonts':
    ensure => latest,
  }
}
