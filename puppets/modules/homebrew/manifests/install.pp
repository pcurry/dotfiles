class homebrew::install {

  # Directories from the Homebrew ::installation script
  $directories = [$homebrew::prefix,
                  "${::homebrew::prefix}/bin",
                  "${::homebrew::prefix}/etc",
                  "${::homebrew::prefix}/include",
                  "${::homebrew::prefix}/lib",
                  "${::homebrew::prefix}/lib/pkgconfig",
                  "${::homebrew::prefix}/Library",
                  "${::homebrew::prefix}/sbin",
                  "${::homebrew::prefix}/share",
                  "${::homebrew::prefix}/var",
                  "${::homebrew::prefix}/var/log",
                  "${::homebrew::prefix}/share/locale",
                  "${::homebrew::prefix}/share/man",
                  "${::homebrew::prefix}/share/man/man1",
                  "${::homebrew::prefix}/share/man/man2",
                  "${::homebrew::prefix}/share/man/man3",
                  "${::homebrew::prefix}/share/man/man4",
                  "${::homebrew::prefix}/share/man/man5",
                  "${::homebrew::prefix}/share/man/man6",
                  "${::homebrew::prefix}/share/man/man7",
                  "${::homebrew::prefix}/share/man/man8",
                  "${::homebrew::prefix}/share/man/de",
                  "${::homebrew::prefix}/share/man/de/man1",
                  "${::homebrew::prefix}/share/man/de/man2",
                  "${::homebrew::prefix}/share/man/de/man3",
                  "${::homebrew::prefix}/share/man/de/man4",
                  "${::homebrew::prefix}/share/man/de/man5",
                  "${::homebrew::prefix}/share/man/de/man6",
                  "${::homebrew::prefix}/share/man/de/man7",
                  "${::homebrew::prefix}/share/man/de/man8",
                  "${::homebrew::prefix}/share/info",
                  "${::homebrew::prefix}/share/doc",
                  "${::homebrew::prefix}/share/aclocal"]

  file { $directories:
    ensure => directory,
    owner  => $::homebrew::user,
    group  => 'admin',
    mode   => '0775',
  }

  Exec {
    path => ['/usr/local/bin', '/usr/bin'],
    user => $::homebrew::user,
    cwd  => $::homebrew::prefix,
  }

  exec { 'homebrew::init':
    command => 'git init -q',
    creates => "${::homebrew::prefix}/.git",
    require => File[$directories],
    notify  => Exec['homebrew::remote'],
  }

  exec { 'homebrew::remote':
    command     => 'git remote add origin https://github.com/mxcl/homebrew.git',
    refreshonly => true,
    unless      => 'git remote show origin',
    notify      => Exec['homebrew::fetch'],
  }

  exec { 'homebrew::fetch':
    command     => 'git fetch origin master:refs/remotes/origin/master -n',
    refreshonly => true,
    notify      => Exec['homebrew::reset'],
  }

  exec { 'homebrew::reset':
    command     => 'git reset --hard origin/master',
    refreshonly => true,
  }

}
