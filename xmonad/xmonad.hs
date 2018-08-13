import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Layout
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.PerScreen

myLayout = 
    avoidStruts $ 
    smartSpacingWithEdge 4 $
    (ifWider 1900 horizontalLayout verticalLayout) ||| Full
  where
    horizontalLayout = Tall 1 (3/100) (1/2)
    verticalLayout = ThreeColMid 1 (3/100) (1/2)

myManageHook = composeAll [
               manageDocks,
               className =? "Shutter" --> doFloat,
               className =? "Gimp" --> doFloat
               ] 

myStartupHook = spawnOnce "albert"

main = do
    xmproc <- spawnPipe "/home/johng/.cabal/bin/xmobar"
    xmonad $ defaultConfig
        { 
            modMask = mod4Mask,
            terminal = "termite",
	    startupHook = myStartupHook,
            manageHook = myManageHook,
            layoutHook = myLayout,
            logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc,
			  ppTitle = xmobarColor "green" "" . shorten 50
			},
	    handleEventHook = mconcat [
		docksEventHook,
		handleEventHook defaultConfig ],
            borderWidth = 2
           } `additionalKeys` [ 
               ((mod4Mask, xK_b), spawn "chromium"),
               ((0, xK_Print), spawn "shutter")
             ]


