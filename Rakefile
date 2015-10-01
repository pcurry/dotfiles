# Copyright (c) 2015  Sebastian Wiesner <swiesner@lunaryorn.com>

# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:

# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

namespace :install do
  desc 'Install Homebrew packages from Brewfile'
  task :brew do
    sh 'brew', 'tap', 'Homebrew/bundle'
    sh 'brew', 'bundle'
  end

  desc 'Install local Python packages from Requirements'
  task :pip do
    sh 'pip', 'install', '--upgrade', '--user', '--requirement', 'Requirements'
  end

  desc 'Install all programs and tools'
  task all: [:brew, :pip]
end

namespace :dotfiles do
  DOTFILE_PACKAGES = FileList['*/.stow-local-ignore']
                     .sub('/.stow-local-ignore', '')

  def stow(package)
    sh 'stow', --target, ENV['HOME'], '-R', package
  end

  desc 'Install dotfile packages'
  task :install do |_, args|
    packages = args.extras.empty? ? DOTFILE_PACKAGES : args.extras
    packages.each do |pkg|
      stow(pkg)
    end
  end

  desc 'List all available dotfile packages'
  task :list do
    DOTFILE_PACKAGES.each do |pkg|
      puts pkg
    end
  end
end

namespace :osx_defaults do |ns|
  def type_arg(v)
    if v.is_a?(String)
      '-string'
    elsif v.is_a?(TrueClass) || v.is_a?(FalseClass)
      '-bool'
    elsif v.is_a?(Integer)
      '-int'
    elsif v.is_a?(Float)
      '-float'
    else
      nil
    end
  end

  # Global settings
  defaults = {
    # Global settings
    'NSGlobalDomain' => {
      # Locale and units
      'AppleLocale' => 'de_DE',
      'AppleMeasurementUnits' => 'Centimeters',
      'AppleMetricUnits' => true,
      # Expand save and print dialogs by default
      'NSNavPanelExpandedStateForSaveMode' => true,
      'PMPrintingExpandedStateForPrint' => true,
      'NSNavPanelExpandedStateForSaveMode2' => true,
      'PMPrintingExpandedStateForPrint2' => true,
      # Don't save to iCloud by default
      'NSDocumentSaveNewDocumentsToCloud' => false,
      # Show ASCII control characters in standard text views
      'NSTextShowsControlCharacters' => true,
    },
    'com.apple.LaunchServices' => {
      'LSQuarantine' => false,
    },
    # Dock & Mission Control
    'com.apple.dock' => {
      # Show indicator dots for open apps
      'show-process-indicators' => true,
      # Don't minimize windows to their apps
      'minimize-to-application' => false,
      # Dock at bottom, and keep visible
      'orientation' => 'bottom',
      'autohide' => false,
      # Don't reorder spaces by most recent use
      'mru-spaces' => false,
    },
    # Dashboard
    'com.apple.dashboard' => {
      'dashboard-enabled-state' => 1,
    },
    # Menubar
    'com.apple.menuextra.battery' => {
      'ShowPercent' => 'NO',
    },
    # Screenshots
    'com.apple.screencapture' => {
      # I like shadows
      'disable-shadow' => false,
    },
    # Finder
    'com.apple.finder' => {
      # Search in current folder by default
      'FXDefaultSearchScope' => 'SCcf',
      # Don't warn about changing file extensions
      'FXEnableExtensionChangeWarning' => false,
      # Show servers and mounted media on desktop, but not internal HDDs
      'ShowExternalHardDrivesOnDesktop' => true,
      'ShowMountedServersOnDesktop' => true,
      'ShowRemovableMediaOnDesktop' => true,
      'ShowHardDrivesOnDesktop' => false,
      # Dont' warn when emptying trash
      'WarnOnEmptyTrash' => false
    }
  }

  defaults_tasks = defaults.flat_map do |domain, settings|
    settings.map do |name, value|
      task_name = "#{domain}:#{name}"
      task task_name do
        t = type_arg(value) || fail("Type #{value.class} not allowed")
        sh('defaults', 'write', domain, name, t, value.to_s)
      end
      task_name
    end
  end.to_a

  desc 'Set all OSX defaults'
  task all: defaults_tasks
end

namespace :conf do
  task :show_library do
    sh 'chflags', 'nohidden', "#{ENV['HOME']}/Library"
  end

  desc 'Set user configuration'
  task user: ['osx_defaults:all', :show_library]
end
