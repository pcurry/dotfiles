# Class: linux::locale
#
# This class sets up locales on Linux.
#
# Parameters:
# - The $system_language to use, as a single locale name
# - The $enabled_locales, as a list of locales to generate
class linux::locale(
  $system_language='en_US.utf8',
  $enabled_locales=['en_US.UTF-8 UTF-8']
  ) {

  if !is_string($system_language) {
    fail('system_language must a string!')
  }
  elsif !$system_language {
    fail('No system language specified!')
  }

  if !is_array($enabled_locales) {
    fail('enables_locales must be an array!')
  }
  elsif empty($enabled_locales) {
    fail('No locales enabled!')
  }

  File {
    owner => 'root',
    group => 'root',
    mode  => '0644',
  }

  $enabled_locales_str = join($enabled_locales, "\n")

  file { '/etc/locale.gen':
    content => "${enabled_locales_str}\n",
    notify  => Exec['linux::locale::generate']
  }

  exec { 'linux::locale::generate':
    command     => 'locale-gen',
    path        => ['/usr/bin', '/bin'],
    refreshonly => true,
  }

  file { '/etc/locale.conf':
    content => template('linux/locale/locale.conf'),
    require => Exec['linux::locale::generate'],
  }
}
