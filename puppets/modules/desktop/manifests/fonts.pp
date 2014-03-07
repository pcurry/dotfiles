# Class: desktop::fonts
#
# Install fonts for Linux desktop systems.
#
# Actions:
# - Install various font packages
class desktop::fonts {

  # Install and enable standard fonts
  require desktop::fonts::dejavu
  require desktop::fonts::liberation
}
