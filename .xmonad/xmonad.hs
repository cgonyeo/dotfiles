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
    , ("M-s", spawnSelected defaultGSConfig ["chromium", "evince", "openttd", "gimp", "xscreensaver-demo", "pavucontrol", "wpa_gui"])
    , ("M-<Print>", spawn "scrot '/home/derek/screenshots/%Y-%m-%d_%T_$wx$h_scrot.png'")
    , ("M-<F1>", spawn "xrandr --output LVDS1 --rotate normal")
    , ("M-<F2>", spawn "xrandr --output LVDS1 --rotate left")
    , ("M-<F3>", spawn "xrandr --output LVDS1 --rotate inverted")
    , ("M-<F4>", spawn "xrandr --output LVDS1 --rotate right")
    ]

pp = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }

hideStat XConfig { modMask } = (modMask, xK_a)
