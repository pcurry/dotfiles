# Class: apps::xorg
#
# Install Xorg
class apps::xorg {
  # The server
  package { ['xorg-server']: ensure => latest }

  # Input drivers
  $input_driver_packages = ['xf86-input-evdev',
                            'xf86-input-joystick',
                            'xf86-input-keyboard',
                            'xf86-input-mouse',
                            'xf86-input-synaptics',
                            'xf86-input-vmmouse',
                            'xf86-input-void',]
  package { $input_driver_packages: ensure => latest }

  # Video drivers
  $video_driver_packages = ['xf86-video-ark',
                            'xf86-video-ast',
                            'xf86-video-ati',
                            'xf86-video-cirrus',
                            'xf86-video-dummy',
                            'xf86-video-fbdev',
                            'xf86-video-glint',
                            'xf86-video-i128',
                            'xf86-video-intel',
                            'xf86-video-mach64',
                            'xf86-video-mga',
                            'xf86-video-modesetting',
                            'xf86-video-neomagic',
                            'xf86-video-nouveau',
                            'xf86-video-nv',
                            'xf86-video-openchrome',
                            'xf86-video-r128',
                            'xf86-video-savage',
                            'xf86-video-siliconmotion',
                            'xf86-video-sis',
                            'xf86-video-tdfx',
                            'xf86-video-trident',
                            'xf86-video-v4l',
                            'xf86-video-vesa',
                            'xf86-video-vmware',
                            'xf86-video-voodoo',]
  package { $video_driver_packages: ensure => latest }

  # Apps
  $app_packages = [ 'xorg-bdftopcf',
                    'xorg-iceauth',
                    'xorg-luit',
                    'xorg-mkfontdir',
                    'xorg-mkfontscale',
                    'xorg-sessreg',
                    'xorg-setxkbmap',
                    'xorg-smproxy',
                    'xorg-x11perf',
                    'xorg-xauth',
                    'xorg-xbacklight',
                    'xorg-xcmsdb',
                    'xorg-xcursorgen',
                    'xorg-xdpyinfo',
                    'xorg-xdriinfo',
                    'xorg-xev',
                    'xorg-xgamma',
                    'xorg-xhost',
                    'xorg-xinput',
                    'xorg-xkbcomp',
                    'xorg-xkbevd',
                    'xorg-xkbutils',
                    'xorg-xkill',
                    'xorg-xlsatoms',
                    'xorg-xlsclients',
                    'xorg-xmodmap',
                    'xorg-xpr',
                    'xorg-xprop',
                    'xorg-xrandr',
                    'xorg-xrdb',
                    'xorg-xrefresh',
                    'xorg-xset',
                    'xorg-xsetroot',
                    'xorg-xvinfo',
                    'xorg-xwd',
                    'xorg-xwininfo',
                    'xorg-xwud',]
  package { $app_packages: ensure => latest }
}
