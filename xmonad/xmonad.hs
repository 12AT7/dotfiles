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
import System.Taffybar.Hooks.PagerHints

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig
        { 
            manageHook = manageDocks <+> manageHook defaultConfig,
            layoutHook = avoidStruts $ 
                         smartSpacingWithEdge 5 $ 
                         -- Mirror (ThreeColMid 1 (3/100) (1/2)) ||| Full,
                         ThreeColMid 1 (3/100) (1/2) ||| Full,
	    logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc,
			  ppTitle = xmobarColor "green" "" . shorten 50
			},
	    handleEventHook = mconcat [
		docksEventHook,
		handleEventHook defaultConfig ],
            terminal = "termite",
            modMask = mod4Mask,
            borderWidth = 2
           } `additionalKeys`
	   [
		((0, xK_Print), spawn "shutter")
	   ]


