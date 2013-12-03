# Class: lunaryorn::user_configuration::osx
#
# This class sets my personal preferences on OS X
#
# Parameters:
# - The $user whose defaults to change
#
# Actions:
# - Change to German locale
# - Disable quarantine configuration for downloaded applications
# - Enable daily update checks
# - Show remaining battery time instead of percentage in the battery applet
# - Disable shadows in screenshots
# - Enable subpixel rendering for all LCDs
# - Change the default search directory of Finder to the current directory
# - Disable warnings about changing the file extension or moving files to Trash
# - Import the Solarized Light and Zenburn themes for Terminal.app
# - Switch Terminal.app to Solarized Light color theme
# - Move the Dock to the left site
# - Enable indicators lights for open applications
# - Disable the dashboard and remove it from spaces
# - Make applications minimize to their icons
# - Make icons of hidden applications translucent
# - Disable automatic hiding of the Dock
# - Automatically hide the Twitter.app window
# - Make Twitter.app open the window when clicking the menu icon
# - Make Twitter.app show full names rather than handles
# - Disable Ping in iTunes
class lunaryorn::user_configuration::osx(
  $user = $lunaryorn::params::user_name
  ) inherits lunaryorn::params {

  if $user != $::id {
    $exec_user = $user
  }

  Osx::Defaults {
    user => $user
  }

  # TODO: Reset Launchpad?
  # find ~/Library/Application\ Support/Dock -name "*.db" -maxdepth 1 -delete

  # Local settings
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

  # Desktop settings
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
  # FIXME: Can we stop this from popping up a new terminal window?
  exec { 'Import Solarized Light theme':
    command => 'open -j -g "files/Solarized Light.terminal" && sleep 1',
    unless  => 'defaults read com.apple.Terminal "Window Settings" | grep "Solarized Light"',
    path    => ['/usr/bin', '/bin'],
    user    => $exec_user,
  }

  exec { 'Import Zenburn theme':
    command => 'open -j -g "files/Zenburn.terminal" && sleep 1',
    unless  => 'defaults read com.apple.Terminal "Window Settings" | grep "Zenburn"',
    path    => ['/usr/bin', '/bin'],
    user    => $exec_user,
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

  # iTunes
  osx::defaults { 'Disable iTunes Ping side bar':
    ensure => present,
    domain => 'com.apple.iTunes',
    key    => 'disablePingSidebar',
    type   => boolean,
    value  => true,
  }

  osx::defaults { 'Disable iTunes Ping':
    ensure => present,
    domain => 'com.apple.iTunes',
    key    => 'disablePing',
    type   => boolean,
    value  => true,
  }
}
