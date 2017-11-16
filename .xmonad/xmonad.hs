{-# LANGUAGE NamedFieldPuns #-}
import XMonad
import XMonad.Layout.NoBorders
import XMonad.Actions.GridSelect
import XMonad.Util.EZConfig
import XMonad.Hooks.DynamicLog

main :: IO ()
main = xmonad =<< statusBar "xmobar" pp hideStat conf

conf = defaultConfig
    { terminal    = "urxvt"
    , modMask     = mod4Mask
    , borderWidth = 1
    , normalBorderColor = "#333333"
    , focusedBorderColor = "#999999"
    , layoutHook = smartBorders $ layoutHook defaultConfig
    }
    `additionalKeysP`
    [ ("M-g", goToSelected defaultGSConfig)
    , ("M-x", spawn "xscreensaver-command -lock")
    , ("M-s", spawnSelected defaultGSConfig [ "firefox"
                                            , "gimp"
                                            , "xscreensaver-demo"
                                            , "pavucontrol"
                                            , "spotify --force-device-scale-factor=2.0"
                                            , "dwarf-fortress"
                                            , "darktable"
                                            , "chromium --profile-directory=Default --app-id=bikioccmkafdpakkkcpdbppfkghcmihk"
                                            , "evince"
                                            ])
    , ("M-<Print>", spawn "scrot '/home/derek/screenshots/%Y-%m-%d_%T_$wx$h_scrot.png'")
    , ("M-m", spawn "xdotool mousemove 1600 900")
    , ("M-]", spawn "/home/derek/.bin/brightness_up")
    , ("M-[", spawn "/home/derek/.bin/brightness_down")
    , ("M-f", spawn "/usr/sbin/alsactl restore 0 -f ~/.alsaconfig-fix-static")
    ]

pp = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

hideStat XConfig { modMask } = (modMask, xK_a)
