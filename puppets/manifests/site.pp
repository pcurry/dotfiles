# Setup my systems
node default {

  # General prerequisites
  case $::operatingsystem {
    'Darwin': {
      if $::macosx_productversion_major != '10.9' {
        fail('Get Mavericks, stupid!')
      }
      if ! $::osx_file_vault_active {
        fail('Hey man, put your files in the vault!')
      }
    }
    default : {
    }
  }

  include lunaryorn::system_configuration
  include lunaryorn::user_configuration
  include lunaryorn::packages
}
