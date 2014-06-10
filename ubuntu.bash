#!/bin/bash
# Copyright (c) 2014 Sebastian Wiesner <swiesner@lunaryorn.com>

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

# Safety options for this script:
# -C: Do not clobber files on redirection
# -e: Exit on command errors
# -u: Error on unset variables
set -C -e -u -v

username='swiesner'
ubuntu_codename="$(lsb_release -c -s)"
downloads="${HOME}/Downloads"
stowdir='/usr/local/stow'

get () {
  curl -#fSL "$@"
}

get-and-extract () {  local url="$1"
  local filename="${url##*/}"
  local target="${downloads}/${filename}"
  get -o "${target}" "${url}"
  tar xf "${target}" -C "${downloads}"
}

add-ppa () {
  local name="${1}"
  local sources_file="${name/\//-}-${ubuntu_codename}.list"
  if [[ ! -f "/etc/apt/sources.list.d/${sources_file}" ]]; then
    sudo add-apt-repository -y "ppa:${name}"
  else
    return 1
  fi
}

package-installed-version () {
  dpkg-query -f '${Version}' -W "$1" 2> /dev/null
}

package-is-installed () {
  package-installed-version "$1" > /dev/null
}

package-install () {
  sudo apt-get install -y "${@}"
}

autotools-install-to-stow () {
  local directory="$1"
  local stowpkg="$2"
  pushd "${directory}"
  ./configure --prefix=/usr/local/
  sudo make prefix="${stowdir}/${stowpkg}" install
  popd
}

package-install-from-url () {
  local url="$1"
  local filename="${url##*/}"
  local target="${downloads}/${filename}"
  get -o "${target}" "${url}"
  sudo dpkg -i "${target}"
}

stow-install () {
  sudo stow -d "${stowdir}" "${1}"
}

cabal-install-binary-to-stow () {
  local package="$(cabal install --dry-run "$1" | tail -n 1)"
  if [[ ! -d "${stowdir}/${package}" ]]; then
    cabal get -d "${downloads}" "${package}"
    pushd "${downloads}/${package}"
    cabal sandbox init
    cabal install --only-dependencies
    cabal configure --prefix="${stowdir}/${package}"
    cabal build
    sudo cabal copy
    popd
  fi
  stow-install "${package}"
}

if [[ $ubuntu_codename != 'trusty' ]]; then
  echo "Current OS not supported!"
  exit 1
fi

if [[ "$(id -un)" != $username ]]; then
  echo "You must run this script as $username!"
  exit 1
fi

mkdir -p "${downloads}"

# Replace the broken WLAN driver
if lsmod | grep -q rtl8192cu; then
  # The prerequisites
  sudo apt-get install git build-essential linux-headers-generic dkms
  # Fast-clone the source code
  git clone --depth 1 'https://github.com/dz0ny/rt8192cu.git' "${downloads}/rt8192cu"
  # Remove and blacklist the old driver
  sudo modprobe -r rtl8192cu
  echo 'blacklist rtl8192cu' | sudo tee '/etc/modprobe.d/blacklist-rtl8192cu.conf'
  # Install the new one
  sudo make -C "${downloads}/rt8192cu" dkms

  # Tell the user to connect to the network, and to restart the script
  echo <<EOF
Please establish a network connection and restart this script!
EOF
  exit 1
fi

# Add the required repos and update the package list
if add-ppa 'ubuntu-elisp/ppa' ||
   add-ppa 'hansjorg/rust' ||
   add-ppa 'chris-lea/node.js'; then
  sudo apt-get update -y
fi

packages=(
  # Basic tools
  'curl' 'nmap' 'xsel'
  'openssh-server'
  'linux-headers-generic' 'dkms'       # Kernel module support
  'stow'                               # For dotfile management
  'zsh' 'zsh-doc' 'autojump'           # My shell
  'emacs-snapshot' 'emacs-snapshot-el' # My editor
  # Developer tools
  'silversearcher-ag'
  # Interpreters and programming languages
  'build-essential'             # The basic stuff (Make + compilers)
  'git' 'mercurial' 'subversion' # VCS
  'ruby2.0' 'ruby2.0-dev'       # Ruby
  'python-pip'                  # Python 2
  'nodejs'                      # NodeJS
  'ocaml' 'opam'                # OCaml
  'alex' 'happy'                # Haskell tools
  'pandoc'                      # *The* markdown processor
  'virtualbox-qt'               # Virtualbox
  # Multimedia support
  'vlc'                                # VLC for video playback
  'adobe-flashplugin'                  # Flash
  'gstreamer1.0-plugins-bad'
  'gstreamer1.0-plugins-ugly'
  'gstreamer1.0-libav'
  # Desktop tools
  'keepass2'                           # Password manager
  # Languages
  'language-pack-de'
  'language-pack-gnome-de'
  'libreoffice-help-de'
  'libreoffice-l10n-de'
  'hunspell-de-de-frami'
  'hunspell-en-us'
)

package-install "${packages[@]}"

# Install extra packages
if ! package-is-installed libdvdcss2; then
  sudo sh '/usr/share/doc/libdvdread4/install-css.sh' # For DVD playback
fi

# Dropbox
if ! package-is-installed dropbox; then
  package-install-from-url "https://www.dropbox.com/download?dl=packages/ubuntu/dropbox_1.6.0_amd64.deb"
fi

# Vagrant
vagrant_version='1.6.2'
if [[ "$(package-installed-version vagrant)" != "1:${vagrant_version}" ]]; then
  package-install-from-url "https://dl.bintray.com/mitchellh/vagrant/vagrant_${vagrant_version}_x86_64.deb"
fi

# GHC
ghc_version='7.8.2'
ghc="ghc-${ghc_version}"
if [[ ! -d '/usr/local/stow/ghc-7.8.2' ]]; then
  get-and-extract "https://www.haskell.org/ghc/dist/${ghc_version}/${ghc}-x86_64-unknown-linux-deb7.tar.xz"
  autotools-install-to-stow "${downloads}/${ghc}" "${ghc}"
fi

if [[ ! -f '/usr/local/bin/ghc' ]]; then
  stow-install "${ghc}"
fi

# Cabal (the latest binary)
if [[ ! -f '/usr/local/bin/cabal' ]]; then
  cabal_version='1.20.0.1'
  cabal="cabal-install-${cabal_version}"
  get-and-extract "http://www.haskell.org/cabal/release/${cabal}/cabal-x86_64-unknown-linux.tar.gz"
  sudo install -D -o root -g root -m 0755 "${downloads}/cabal" \
       "${stowdir}/${cabal}/bin/cabal"
  stow-install "${cabal}"
fi

# Update the Cabal package list
if [[ ! -f "${HOME}/.cabal/packages/hackage.haskell.org/00-index.tar" ]]; then
  cabal update
fi

cabal_binaries=(
  hlint
  shellcheck
  marmalade-upload
)

for pkg in "${cabal_binaries[@]}"; do
  cabal-install-binary-to-stow "$pkg"
done

eggs=(
  virtualenv virtualenvwrapper
  pygments ipython
)
pip install -U --user "${eggs[@]}"

gems=(
  'hub' 'ghi'                   # Github tools
  'travis' 'travis-lint'
  'puppet' 'puppet-lint'
  'rubocop'
)
gem2.0 install --user-install --no-ri --no-rdoc "${gems[@]}"
