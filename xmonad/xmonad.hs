import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog(xmobar)
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    spawnPipe "compton -b"
    spawnPipe "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 10 --transparent true --tint 0x191970 --height 26"
    spawnPipe "hsetroot -solid '#000077'"
    xmonad =<< xmobar desktopConfig
        { 
            terminal = "termite",
            modMask = mod4Mask,
            borderWidth = 2,
            layoutHook = avoidStruts $ 
                         smartSpacingWithEdge 5 $ 
                         Tall 1 (3/100) (1/2) ||| 
                         Full,
            manageHook = manageDocks <+> manageHook defaultConfig
            } 
            -- `additionalKeys` [
                -- ((mod4Mask .|. shiftMask, xK_g), sendMessage $ ToggleGaps)
            -- ]


