import XMonad
import XMonad.Layout.NoBorders

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
