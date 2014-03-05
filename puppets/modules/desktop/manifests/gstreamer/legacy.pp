# Class: desktop::gstreamer::legacy
#
# This class installs the legacy GStreamer.
#
# Parameters:
#
# Actions:
# - Install gstreamer
# - Install all gstreamer plugins
class desktop::gstreamer::legacy {
  package { 'gstreamer0.10': ensure => latest }

  $plugins = ['gstreamer0.10-ffmpeg',
              'gstreamer0.10-base-plugins',
              'gstreamer0.10-good-plugins',
              'gstreamer0.10-bad-plugins',
              'gstreamer0.10-ugly-plugins',]
  package { $plugins:
    ensure  => latest,
    require => Package['gstreamer0.10'],
  }
}
