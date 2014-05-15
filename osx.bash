#!/bin/bash
# Copyright (c) 2014 Sebastian Wiesner <lunaryorn@gmail.com>

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

set -C -e -u -v

if [[ "$(fdesetup isactive)" != 'true' ]]; then
  echo "PUT YOUR DISK IN FILEVAULT!"
fi

# Install Homebrew if absent
if ! brew --version > /dev/null; then
  ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
fi

# Setup an OS X system.
brew bundle "$(dirname $0)"

# Setup my shell
my_shell="$(dscl . -read /Users/swiesner UserShell)"
if [[ $my_shell != 'UserShell: /bin/zsh' ]]; then
  chsh -s /bin/zsh
fi

# Time zone
sudo systemsetup -settimezone "Europe/Berlin"

# Shut up boot sound
sudo nvram SystemAudioVolume=" "

# The hostname
my_hostname='lunaryorn-air'
if [[ "$(hostname)" != "$my_hostname" ]]; then
  sudo scutil --set ComputerName "${my_hostname}"
  sudo scutil --set HostName "${my_hostname}"
  sudo scutil --set LocalHostName "${my_hostname}"
  sudo defaults write \
       /Library/Preferences/SystemConfiguration/com.apple.smb.server \
       NetBIOSName -string "${my_hostname}"
fi

# Locale settings
defaults write NSGlobalDmain AppleLocale -string 'de_DE'
defaults write NSGlobalDmain AppleMeasurementUnits -string 'Centimeters'
defaults write NSGlobalDmain AppleMetricUnits -bool true

# Disable security questions
defaults write com.apple.LaunchServices LSQuarantine -bool false

# Battery information
defaults write com.apple.menuextra.battery ShowPercent -string 'NO'
defaults write com.apple.menuextra.battery ShowTime -string 'YES'

# Expanded dialogs by default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Don't create .DS_Store files on network volumes
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Show control characters
defaults write NSGlobalDomain NSTextShowsControlCharacters -bool true

# No shadow in screenshots
defaults write com.apple.screencapture disable-shadow -bool true

# Subpixel rendering in non-Apple LCDs
defaults write NSGlobalDomain AppleFontSmoothing -int 2

# Search in current folder by default
defaults write com.apple.finder FXDefaultSearchScope -string 'SCcf'

# Do not warn about changing extensions
defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

# Do not warn when emptying trash
defaults write com.apple.finder WarnOnEmptyTrash -bool false

# Show icons for hard drives, servers, and removable media on the desktop
defaults write com.apple.finder ShowExternalHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowHardDrivesOnDesktop -bool true
defaults write com.apple.finder ShowMountedServersOnDesktop -bool true
defaults write com.apple.finder ShowRemovableMediaOnDesktop -bool true
# Minimize windows to their apps
defaults write com.apple.dock minimize-to-application -bool true

# Show indicators for open applications
defaults write com.apple.dock show-process-indicators -bool true

# Dock to bottom
defaults write com.apple.dock orientation -string bottom

# Don't hide the dock automatically
defaults write com.apple.dock autohide -bool false

# Make hidden icons transparent
defaults write com.apple.dock showhidden -bool true

# Don't rearrange spaces on most recent usage
defaults write com.apple.dock mru-spaces -bool false

# No Dashboard please
defaults write com.apple.dashboard mcx-disabled -bool true

# Don't show Dashboard as a Space
defaults write com.apple.dock dashboard-in-overlay -bool true

# Show Twitter.app window when clicking the icon
defaults write com.twitter.twitter-mac MenuItemBehavior -int 1

# Allow closing the "new tweet" window by pressing `Esc`
defaults write com.twitter.twitter-mac ESCClosesComposeWindow -bool true

# Show full names rather than handles
defaults write com.twitter.twitter-mac ShowFullNames -bool true

# Keep Twitter visible
defaults write com.twitter.twitter-mac HideInBackground -bool false

# Disable iTunes Ping
defaults write com.apple.iTunes disablePingSidebar -bool true
defaults write com.apple.iTunes disablePing -bool true

# Show the ~/Library folder
chflags nohidden ~/Library

# Import Solarized Colorscheme
if ! defaults read com.apple.Terminal 'Window Settings' | grep -q Solarized;
then
  open -j -g "$(dirname $0)/Solarized Light.terminal" && sleep 1
fi

# Set color scheme for Terminal.app
for setting in 'Default Window Settings' 'Startup Window Settings'; do
  defaults write com.apple.terminal "$setting" -string 'Solarized Light'
done
