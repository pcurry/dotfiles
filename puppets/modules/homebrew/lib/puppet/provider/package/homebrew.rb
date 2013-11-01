require 'puppet/provider/package'
require 'pathname'
require 'etc'

Puppet::Type.type(:package)
  .provide(:homebrew, { parent: Puppet::Provider::Package }) do

  desc 'Package management using HomeBrew on OS X'

  has_feature :install_options

  confine({ operatingsystem: :darwin })

  optional_commands({ brew: '/usr/local/bin/brew' })

  def self.homebrew_user
    prefix = Pathname.new(brew('--prefix').strip)
    uid = prefix.stat.uid
    fail Puppet::Error, 'Refusing to run Homebrew as root' if uid == 0
    Etc.getpwuid(uid)
  rescue Errno::ENOENT
    # The Homebrew prefix does not exist
    fail Puppet::Error, 'Homebrew not installed'
  end

  def self.homebrew(*args)
    user = homebrew_user
    options = {
      custom_environment: {
        'HOME' => user.dir,
        'USER' => user.name,
        # Please keep our valuable info files!
        'HOMEBREW_KEEP_INFO_' => 'true',
        # Keep build output simple
        'HOMEBREW_NO_EMOJI' => 'true',
      },
      failonfail: true,
    }

    options[:uid] = user.uid if user.uid != Process.uid

    execute([command(:brew), *args], options)
  end

  def self.instances
    homebrew('list', '--versions')
      .lines
      .map { |line| new(name_version_split(line)) }
  rescue Puppet::Error
    # Homebrew is not yet installed, hence there are no packages
    Puppet.warning 'Homebrew not installed'
    []
  end

  # Install packages, known as formulas, using brew.
  def install
    options = resource[:install_options]
    cmd = ['install', '--verbose', *options, resource[:name]]
    output = self.class.homebrew(*cmd)

    # Fail hard if there is no formula available.
    if output =~ /Error: No available formula/
      raise Puppet::ExecutionFailure, "Could not find package #{resource[:name]}"
    end
  end

  def uninstall
    self.class.homebrew('uninstall', resource[:name])
  end

  def update
    install
  end

  def query
    output = self.class.homebrew('list', '--versions', resource[:name])
    if output
      self.class.name_version_split(output)
    end
  end

  def latest
    query[:ensure]
  end

  private
  def self.name_version_split(line)
    /^(?<name>\S+)\s+(?<version>.+)/.match(line.strip) do |m|
      return {
        name: m[:name],
        ensure: m[:version],
        provider: :homebrew,
      }
    end
  end

end
