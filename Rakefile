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
  TYPES = {
    int: '-int',
    string: '-string',
    bool: '-bool'
  }

  def osx_default(domain, name, type, value)
    task_name = "#{domain}:#{name}"
    task task_name do
      sh('defaults', 'write', domain, name, TYPES[type], value.to_s)
    end
  end

  # Locale and units
  osx_default 'NSGlobalDomain', 'AppleLocale', :string, 'de_DE'
  osx_default 'NSGlobalDomain', 'AppleMeasurementUnits', :string, 'Centimeters'
  osx_default 'NSGlobalDomain', 'AppleMetricUnits', :bool, true

  # No quarantaine
  osx_default 'com.apple.LaunchServices', 'LSQuarantine', :bool, false
  osx_default 'com.apple.menuextra.battery', 'ShowPercent', :string, 'NO'

  # Dialogs: Enable extended save and print panels
  panels = %w(NSNavPanelExpandedStateForSaveMode
              NSNavPanelExpandedStateForSaveMode2
              PMPrintingExpandedStateForPrint
              PMPrintingExpandedStateForPrint2)
  panels.each { |name| osx_default 'NSGlobalDomain', name, :bool, true }

  desc 'Set all OSX defaults'
  task all: ns.tasks.reject { |t| t.name == 'osx_defaults:all' }
end

namespace :conf do
  task :show_library do
    sh 'chflags', 'nohidden', "#{ENV['HOME']}/Library"
  end

  task defaults: ['osx_defaults:all', :show_library]

  desc 'Set user configuration'
  task user: [:defaults]
end
