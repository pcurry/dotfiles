class homebrew ($user = undef) {
  $prefix = '/usr/local'

  if $::id != $user {
    $realuser = $user
  }
}
