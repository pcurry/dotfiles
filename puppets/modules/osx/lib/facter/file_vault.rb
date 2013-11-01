Facter.add(:osx_file_vault_active) do
  confine :operatingsystem => 'Darwin'
  setcode do
    Facter::Util::Resolution.exec('fdesetup isactive') == 'true'
  end
end
