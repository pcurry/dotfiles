# Class: apps::clojure
#
# This class installs Clojure.
#
# Parameters:
#
# Actions:
# - Install Leiningen.
class apps::clojure {
  package { 'leiningen': ensure => latest}
}
