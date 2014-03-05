# Class: desktop::gstreamer
#
# This class installs the current GStreamer.
#
# Parameters:
#
# Actions:
# - Install gstreamer
# - Install all gstreamer plugins
class desktop::gstreamer {
  package { 'gstreamer': ensure => latest }

  $plugins = ['gst-libav',
              'gst-plugins-base',
              'gst-plugins-good',
              'gst-plugins-bad',
              'gst-plugins-ugly',]
  package { $plugins:
    ensure  => latest,
    require => Package['gstreamer'],
  }
}
