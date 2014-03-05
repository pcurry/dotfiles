# Class: kde::kdm
#
# This class enables KDM.
#
# Parameters:
#
# Actions:
# - Enable the KDM service
class kde::kdm {
  service { 'kdm':
    enable  => true,
    require => [Class['x11'], Package['kde-meta-kdebase']],
  }
}
