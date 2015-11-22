import XMonad
import XMonad.Layout.NoBorders
import XMonad.Actions.GridSelect
import XMonad.Util.EZConfig

main :: IO ()
main = do
  xmonad $ defaultConfig
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
    , ("M-s", spawnSelected defaultGSConfig ["chromium", "evince", "openttd", "gimp", "xscreensaver-demo"])
    , ("M-x", spawn "xscreensaver-command -lock")
    , ("M-<Up>", spawn "light -A 10")
    , ("M-<Down>", spawn "light -U 10")
    , ("M-<Print>", spawn "scrot '/home/derek/screenshots/%Y-%m-%d_%T_$wx$h_scrot.png'")
    , ("M-<F1>", spawn "xrandr --output LVDS1 --rotate normal")
    , ("M-<F2>", spawn "xrandr --output LVDS1 --rotate left")
    , ("M-<F3>", spawn "xrandr --output LVDS1 --rotate inverted")
    , ("M-<F4>", spawn "xrandr --output LVDS1 --rotate right")
    ]
