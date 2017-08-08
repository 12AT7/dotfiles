import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Tabbed
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers

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
                         Mirror (ThreeColMid 1 (3/100) (1/2)) ||| Full,
            -- manageHook = manageDocks <+> manageHook defaultConfig
            manageHook = isDialog --> doF W.shiftMaster <+> doF W.focusDown,
            logHook = dynamicLogWithPP $ xmobarPP
        } 


