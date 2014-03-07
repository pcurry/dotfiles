# Class: linux::pc_speaker
#
# This class disables the PC speaker on Linux.
#
# Actions:
# - Disable the PC Speaker module
# - Blacklist the PC speaker module
class linux::pc_speaker {

  $module = 'pcspkr'

  exec { 'linux::pc_speaker::unload':
    command => "modprobe -r ${module}",
    path    => ['/usr/bin', '/bin'],
  }

  file { '/etc/modprobe.d/blacklist-pcspkr':
    content => "blacklist ${module}\n",
    owner   => 'root',
    group   => 'root',
    mode    => '0644',
  }
}
