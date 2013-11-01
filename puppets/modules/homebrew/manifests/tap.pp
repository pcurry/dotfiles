define homebrew::tap($tap = $title) {

  Class['homebrew'] -> Exec["homebrew::tap::${title}"]

  $parts = split($tap, '/')
  $user = $parts[0]
  $repo = $parts[1]
  $directory = "${::homebrew::prefix}/Library/Taps/${user}-${repo}"

  exec { "homebrew::tap::${title}":
    command     => "${::homebrew::prefix}/bin/brew tap ${tap}",
    user        => $::homebrew::realuser,
    creates     => $directory,
    environment => ["USER=${::homebrew::user}",
                    "HOME=/Users/${::homebrew::user}"]
  }
}
