import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
-- import XMonad.Layout.Gaps
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

main = do
    xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs" 
    xmonad $ desktopConfig
        { 
            terminal = "termite",
            modMask = mod4Mask,
            borderWidth = 2,
            layoutHook = avoidStruts $ 
                         smartSpacingWithEdge 5 $ 
                         Tall 1 (3/100) (1/2) ||| Full,
            manageHook = manageHook defaultConfig <+> manageDocks 
            } 
            `additionalKeys` [
                -- ((mod4Mask .|. shiftMask, xK_g), sendMessage $ ToggleGaps)
            ]


