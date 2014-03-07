# Class: desktop::pulseaudio
#
# This class installs and configures the PulseAudio sound server.
class desktop::pulseaudio {

  package { 'pulseaudio':
    ensure => latest,
  }

  # ALSA configuration for PulseAudio
  package { ['pulseaudio-alsa']:
    ensure  => latest,
    require => Package['pulseaudio']
  }

  # GUI frontends to control PulseAudio
  package { ['paprefs', 'pavucontrol']:
    ensure  => latest,
    require => Package['pulseaudio']
  }

  # Force PulseAudio for libraries
  file { '/etc/libao.conf':
    content => "default_driver=pulse\n",
    owner   => 'root',
    group   => 'root',
    mode    => '0644',
    require => Package['pulseaudio'],
  }
}
