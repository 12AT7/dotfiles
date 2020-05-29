import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.SpawnOnce
import XMonad.Layout
import XMonad.Layout.Spacing
import XMonad.Layout.ThreeColumns
import XMonad.Actions.WorkspaceNames
import XMonad.Util.EZConfig(additionalKeys)
import System.IO
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Layout.PerScreen
import Data.Ratio
import XMonad.StackSet
import Text.Regex.Posix

myLayout =
    avoidStruts $
    smartSpacingWithEdge 4 $
    (ifWider 1900 horizontalLayout verticalLayout) ||| Full
  where
    horizontalLayout = Tall 1 (3/100) (1/2)
    verticalLayout = Mirror (ThreeColMid 1 (3/100) (1/2))

mayavi_root = "Mayavi2 - The 3D data visualizer"
isModal :: Query Bool
isModal = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_MODAL"

myManageHook = composeAll [
               manageDocks,
               className =? "Gimp" --> doFloat,
	       className =? "SWT" --> doRectFloat (RationalRect (1%4) (1%4) (1%2) (1%2)),
	       title =? "FDM Visualizer" --> doShift "3",
	       isModal --> doFloat
               ]

myStartupHook = do
	setWMName "LG3D" -- Java hack

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ desktopConfig
        {
            modMask = mod4Mask,
            terminal = "termite",
	    startupHook = myStartupHook,
            manageHook = myManageHook <+> manageHook desktopConfig,
            layoutHook = myLayout ||| layoutHook desktopConfig,
            logHook = dynamicLogWithPP xmobarPP
			{ ppOutput = hPutStrLn xmproc,
			  ppTitle = xmobarColor "green" "" . shorten 50
			},
	    handleEventHook = mconcat [
		docksEventHook,
		handleEventHook defaultConfig ],
            borderWidth = 2
           } `additionalKeys` [
               ((mod4Mask, xK_c), spawn "chromium"),
               ((mod4Mask, xK_z), spawn "slock"),
               ((mod4Mask, xK_s), spawn "slack"),
               ((mod4Mask, xK_w), renameWorkspace def),
               ((0, xK_Print), spawn "shutter")
             ]
