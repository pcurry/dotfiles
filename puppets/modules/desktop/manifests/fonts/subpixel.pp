# Class: desktop::fonts::subpixel
#
# This class configures subpixel rendering.
#
# Actions:
# - Setup subpixel rendering
class desktop::fonts::subpixel($rgba = true) {

  $rgba_settings = ['10-no-sub-pixel',
                    '10-sub-pixel-bgr',
                    '10-sub-pixel-rgb',
                    '10-sub-pixel-vbgr',
                    '10-sub-pixel-vrgb',
                    ]

  $to_enable = $rgba ? {
    false => '10-no-sub-pixel',
    true  => '10-sub-pixel-rgb',
    /^(bgr|rgb|vbgr|vrgb)$/ => "10-sub-pixel-${rgba}",
    default => fail("Unknown rgba value: ${rgba}!")
  }

  desktop::fonts::fontconfig { $to_enable:
    ensure => enabled,
  }

  $to_disable = delete($rgba_settings, $to_enable)

  desktop::fonts::fontconfig { $to_disable:
    ensure => disabled,
  }
}
