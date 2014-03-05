# Class: desktop::fonts
#
# Install fonts for Linux desktop systems.
#
# Actions:
# - Install various font packages
class desktop::fonts {

  # A good fontset for non-OS X systems
  $fonts = [# Essential standard fonts
            'ttf-dejavu',
            'ttf-liberation',
            # Monospace fonts
            'adobe-source-code-pro-fonts',
            'ttf-anonymous-pro',
            'ttf-inconsolata',
            'terminus-font',
            # Misc fonts
            'ttf-droid',
            'ttf-ubuntu-font-family',
            # Windows fonts
            'ttf-ms-fonts',
            'ttf-vista-fonts',
            # Taiwan, Chinese and Arabic fonts
            'ttf-baekmuk',
            'wqy-microhei',
            'wqy-zenhei',
            'ttf-arphic-ukai',
            'ttf-arphic-uming',
            'opendesktop-fonts',
            'ttf-hannom',
            ]

  package { $fonts : ensure => latest }
}
