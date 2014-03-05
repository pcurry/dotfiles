# Definition: system::hostname
#
# Set the hostname of the system.
#
# Parameters:
# - The $hostname (title parameter)
define system::hostname($hostname = $title) {
  case $::operatingsystem {
    'Darwin': {
      osx::systemconfig { 'ComputerName':  value => $hostname }
      osx::systemconfig { 'HostName':      value => $hostname }
      osx::systemconfig { 'LocalHostName': value => hostname }
      osx::defaults { 'Set SMB host name':
        ensure => present,
        domain => '/Library/Preferences/SystemConfiguration/com.apple.smb.server',
        key    => 'NetBIOSName',
        type   => string,
        value  => $hostname,
        user   => 'root',
      }
    }
    default: {
      # Linux systems
      file { '/etc/hostname':
        content => $hostname,
        owner   => 'root',
        group   => 'root',
        mode    => '0644',
      }
    }
  }
}
