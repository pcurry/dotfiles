# Class: desktop::fonts::anonymous_pro
#
# This class installs the Anonymous Pro font.
#
# Actions:
# - Install the Anonymous Pro font
class desktop::fonts::anonymous_pro {
  package { 'ttf-anonymous-pro':
    ensure => latest,
  }
}
