# Setup an OS X system

node default {

  $user = 'swiesner'
  $home = "/Users/${user}"
  $realuser = $::id ? {
    $user   => undef,
    default => $user,
  }

  $hostname = 'lunaryorn-air'

  class { 'homebrew':
    user => $user
  }

  # We must have 10.9
  if $::macosx_productversion_major != '10.9' {
    fail('Get Mavericks, stupid!')
  }

  # We also want filefault
  if ! $::osx_file_vault_active {
    fail('Hey man, put your files in the vault!')
  }

  # If we can, let's install Homebrew
  if $::id == 'root' {
    include homebrew::install

    Class['homebrew::install'] -> Package<| |>
  }
  else {
    notice('We are not root, and cannot install Homebrew')
  }

  # Switch my shell to Zsh
  if $::id == 'root' {
    exec { "/usr/bin/chsh -s /bin/zsh ${user}": }
  }
  else {
    notice('We are not root, and cannot switch your shell')
  }

  # Install all our packages with Homebrew
  Package {
    provider => homebrew,
  }

  # Install our Homebrew packages
  package { [ 'stow',                # To manage these dotfiles
              'git',                 # VCS tools
              'mercurial',
              'bazaar',
              'texinfo',             # Build Emacs docs
              'leiningen',           # Clojure project management
              'objective-caml',      # Ocaml and its package manager
              'opam',
              'nmap',                # The port scanner for network debugging
              'pwgen',               # Password generation from command line
              'the_silver_searcher', # Code search reloaded
              'fasd',                # Fast directory switching for Zsh
              'hub',                 # Control Github from command line
              'ghi',                 # Browse Github issues from command line
              'youtube-dl',          # Youtube video downloader
              ]:
  }

  package { 'emacs':
    ensure          => latest,
    # Install Emacs trunk, with Cocoa support, better colors, and GNU TLS
    # built-in
    install_options => ['--HEAD', '--cocoa', '--srgb', '--with-gnutls'],
    require         => Package['aspell'] # For flyspell
  }

  # Put Emacs into the Applications folder
  file { '/Applications/Emacs.app':
    ensure  => link,
    target  => "${::homebrew::prefix}/Cellar/emacs/HEAD/Emacs.app",
    require => [Class['homebrew'], Package['emacs']],
  }

  # Spell checker for Emacs
  package { 'aspell':
    ensure          => latest,
    install_options => ['--with-lang-de', '--with-lang-en']
  }

  # Link Texinfo to /usr/bin, for convenience
  if $::operatingsystem == 'Darwin' {
    exec { 'roles::devel::emacs::link-texinfo':
      command     => "${::homebrew::prefix}/bin/brew link --force texinfo",
      user        => $::homebrew::realuser,
      environment => ["USER=${::homebrew::user}",
                      "HOME=/Users/${::homebrew::user}"],
      creates     => "${::homebrew::prefix}/bin/makeinfo",
      require     => [Class['homebrew'], Package['texinfo']],
    }
  }

  # Install Source Code Pro font
  $sourcecodepro_url = 'http://sourceforge.net/projects/sourcecodepro.adobe/files/SourceCodePro_FontsOnly-1.017.zip/download'
  $sourcecodepro_archive = "${home}/Downloads/SourceCodePro.zip"
  exec { 'Download Source Code Pro':
    command => "curl -L -s -o ${sourcecodepro_archive} ${sourcecodepro_url}",
    unless  => "test -f ${home}/Library/Fonts/SourceCodePro-Regular.otf",
    creates => $sourcecodepro_archive,
    user    => $real_user,
    path    => ['/usr/bin', '/bin'],
  }

  exec { 'Install Source Code Pro':
    command => "unzip -oj ${sourcecodepro_archive} '*.otf' -d ${home}/Library/Fonts",
    creates => "${home}/Library/Fonts/SourceCodePro-Regular.otf",
    user    => $real_user,
    path    => ['/usr/bin'],
    require => Exec['Download Source Code Pro'],
  }

  Osx::Defaults {
    user => $user,
  }

  # TODO: Reset Launchpad?
  # find ~/Library/Application\ Support/Dock -name "*.db" -maxdepth 1 -delete

  # Change OS X settings

  # System settings
  # Disable the annoying beep on boot
  if $::id == 'root' {
    exec { 'Disable beep on boot':
      command => '/usr/sbin/nvram SystemAudioVolume=" "',
    }
  }
  else {
    notice('We are not root, and cannot disable the boot sound')
  }

  # Set hostname
  if $::id == 'root' {
    osx::systemconfig { 'ComputerName':
      value => $hostname
    }

    osx::systemconfig { 'HostName':
      value => $hostname
    }

    osx::systemconfig { 'LocalHostName':
      value => hostname
    }

    osx::defaults { 'Set SMB host name':
      ensure => present,
      domain => '/Library/Preferences/SystemConfiguration/com.apple.smb.server',
      key    => 'NetBIOSName',
      type   => string,
      value  => $hostname,
    }
  }
  else {
    notice('We are not root, and cannot change the host name')
  }

  # Locale
  # TODO: Select languages
  # defaults write NSGlobalDomain AppleLanguages -array "de" "en" "uk"
  osx::defaults { 'Use German locale settings':
    ensure => present,
    domain => 'NSGlobalDomain',
    key    => 'AppleLocale',
    type   => string,
    value  => 'de_DE',
  }

  osx::defaults { 'Use centimeters as measurement units' :
    ensure => present,
    domain => 'NSGlobalDomain',
    key    => 'AppleMeasurementUnits',
    type   => string,
    value  => 'Centimeters',
  }

  osx::defaults { 'Use metric units':
    ensure => present,
    domain => 'NSGlobalDomain',
    key    => 'AppleMetricUnits',
    type   => boolean,
    value  => true,
  }

  if $::id == 'root' {
    osx::timezone { 'Europe/Berlin': }
  }
  else {
    notice('We are not root, and cannot change the timezone')
  }

  # Security
  osx::defaults { 'Disable "Are you sure you want to open this application?"':
    ensure => present,
    domain => 'com.apple.LaunchServices',
    key    => 'LSQuarantine',
    type   => boolean,
    value  => false,
  }

  osx::defaults { 'Check for updates daily':
    ensure => present,
    domain => 'com.apple.SoftwareUpdate',
    key    => 'ScheduleFrequency',
    type   => integer,
    value  => 1,
  }

  # Battery applet
  osx::defaults { 'Disable battery percentage':
    ensure => present,
    domain => 'com.apple.menuextra.battery',
    key    => 'ShowPercent',
    type   => string,
    value  => 'NO',
  }

  osx::defaults { 'Enable battery time':
    ensure => present,
    domain => 'com.apple.menuextra.battery',
    key    => 'ShowTime',
    type   => string,
    value  => 'YES',
  }

  #
  osx::defaults { 'Disable shadow in screenshots':
    ensure => present,
    domain => 'com.apple.screencapture',
    key    => 'disable-shadow',
    type   => boolean,
    value  => true,
  }

  osx::defaults { 'Enable subpixel font rendering on non-Apple LCDs':
    ensure => present,
    domain => 'NSGlobalDomain',
    key    => 'AppleFontSmoothing',
    type   => integer,
    value  => 2,
  }

  # Finder
  osx::defaults { 'Search in the current folder by default':
    ensure => present,
    domain => 'com.apple.finder',
    key    => 'FXDefaultSearchScope',
    type   => string,
    value  => 'SCcf',
  }

  osx::defaults { 'Do not warn when changing the file extension':
    ensure => present,
    domain => 'com.apple.finder',
    key    => 'FXEnableExtensionChangeWarning',
    type   => boolean,
    value  => false,
  }

  osx::defaults { 'Do not warn before moving to Trash':
    ensure => present,
    domain => 'com.apple.finder',
    key    => 'WarnOnEmptyTrash',
    type   => boolean,
    value  => false,
  }

  # Terminal.app
  exec { 'Import Terminal.app color theme':
    # FIXME: Can we stop this from popping up a new terminal window?
    command => 'open -j -g "files/Solarized Light.terminal" && sleep 1',
    path    => ['/usr/bin', '/bin']
  }

  osx::defaults { 'Set default color theme':
    ensure => present,
    domain => 'com.apple.terminal',
    key    => 'Default Window Settings',
    type   => string,
    value  => 'Solarized Light',
  }

  osx::defaults { 'Set startup color theme':
    ensure => present,
    domain => 'com.apple.terminal',
    key    => 'Startup Window Settings',
    type   => string,
    value  => 'Solarized Light',
  }

  # Dock and Dashboard
  osx::defaults { 'Minimize windows to their application\'s icon':
    ensure => present,
    domain => 'com.apple.dock',
    key    => 'minimize-to-application',
    type   => boolean,
    value  => true,
  }

  osx::defaults { 'Show indicator lights for open applications':
    ensure => present,
    domain => 'com.apple.dock',
    key    => 'show-process-indicators',
    type   => boolean,
    value  => true,
  }

  osx::defaults { 'Move Dock to left side':
    ensure => present,
    domain => 'com.apple.dock',
    key    => 'orientation',
    type   => string,
    value  => 'left',
  }

  osx::defaults { 'Disable the Dashboard':
    ensure => present,
    domain => 'com.apple.dashboard',
    key    => 'mcx-disabled',
    type   => boolean,
    value  => true,
  }

  osx::defaults { 'Do not show the Dashboard as a Space':
    ensure => present,
    domain => 'com.apple.dock',
    key    => 'dashboard-in-overlay',
    type   => boolean,
    value  => true,
  }

  osx::defaults { 'Do not automatically hide the Dock':
    ensure => present,
    domain => 'com.apple.dock',
    key    => 'autohide',
    type   => boolean,
    value  => false,
  }

  osx::defaults { 'Make the icons of hidden applications translucent':
    ensure => present,
    domain => 'com.apple.dock',
    key    => 'showhidden',
    type   => boolean,
    value  => true,
  }

  # Twitter
  osx::defaults { 'Show Twitter.app window when clicking the menu icon':
    ensure => present,
    domain => 'com.twitter.twitter-mac',
    key    => 'MenuItemBehavior',
    type   => integer,
    value  => 1
  }

  osx::defaults { 'Allow closing the "new tweet" window by pressing `Esc`':
    ensure => present,
    domain => 'com.twitter.twitter-mac',
    key    => 'ESCClosesComposeWindow',
    type   => boolean,
    value  => true
  }

  osx::defaults { 'Show full names rather than Twitter handles':
    ensure => present,
    domain => 'com.twitter.twitter-mac',
    key    => 'ShowFullNames',
    type   => boolean,
    value  => true
  }

  osx::defaults { 'Hide Twitter.app in the background':
    ensure => present,
    domain => 'com.twitter.twitter-mac',
    key    => 'HideInBackground',
    type   => boolean,
    value  => true
  }
}
