# Setup my systems
node default {

  if $::id == 'root' {
    include lunaryorn::system
  }
  else {
    notice('Need root privileges to install and configure the system.')
  }

  include lunaryorn::user::settings
}
