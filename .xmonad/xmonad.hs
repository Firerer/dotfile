{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures -Wno-unused-imports #-}

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.SpawnOnce ( spawnOnce )
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Hooks.ManageHelpers ( doFullFloat, isFullscreen )
import XMonad.Hooks.ManageDocks( avoidStruts, docks, manageDocks, Direction2D(D, L, R, U) )
import XMonad.Layout.NoBorders
--import Xmonad.Layout.ThreeColumns
import XMonad.Layout.Fullscreen (fullscreenManageHook, fullscreenSupport)
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )
import XMonad.Layout.Gaps(gaps, setGaps, GapMessage(DecGap, ToggleGaps, IncGap))
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioNext)

import Control.Monad ( join, when )
import Data.Monoid ()
import Data.List
import Data.Maybe (maybeToList)
import qualified Data.Map        as M
import System.Exit

myTerminal      = "alacritty"
myFocusFollowsMouse = True
myClickJustFocuses = False
myBorderWidth   = 2
myModMask       = mod4Mask
myWorkspaces   = ["Code" , "Web" , "Term" , "Game", "5","6","7","8","9"]
-- myWorkspaces    = ["1:\63083", "2:\63288", "3:\63306", "4:\61723", "5:\63107", "6:\63601", "7:\63391", "8:\61713", "9:\61884"]
myNormalBorderColor  = "#3b4050"
myFocusedBorderColor = "#bc96da"
myGaps = [(L,0), (R,0), (U,30), (D,0)]
myWindowGaps = 2
myStartupHook = do
   addEWMHFullscreen
   spawnOnce "lxsession &" -- conflicts with polybar ewmh module
   -- spawnOnce "picom &"
   spawnOnce "dunst &"
   spawnOnce "fcitx5 &"
   spawnOnce "feh --bg-fill ~/Pictures/wallpapers/alone-girl-artwork-l9.jpg"
   spawnOnce "emacs --daemon &"
   spawnOnce "polybar example &"

------------------------------------------------------------------------
-- Window rules:
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    (
    [ resource  =? "Dialog"         --> doFloat
    , resource  =? "pavucontrol"         --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen --> doFullFloat
    ]
    ++ [fmap (c `isInfixOf`) className --> doFloat | c <- floatClassInfixes ]
    ++ [fmap (c `isInfixOf`) resource --> doFloat | c <- floatResourceInfixes ]
    ++ [fmap (c ==) className --> doShift (myWorkspaces !! (i-1)) | (c, i) <- shfitClassInfixes ]
    )
    where
      floatClassInfixes = ["hiped", "MPlayer", "Gimp", "pavucontrol"]
      floatResourceInfixes = ["Dialog", "control"]
      -- TODO: some shifts are not working
      shfitClassInfixes = [("zoom", 3),
                           ("Steam", 4), ("Lutris", 4), ("battle.net.exe", 4),
                           ("Discord", 5), ("VirtualBox", 6), ("Spotify", 9)]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
maimcopy :: MonadIO m => m () -- Don't question it
maimcopy = spawn "maim -s | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot"
maimsave = spawn "maim -s ~/Pictures/ScreenShots/$(date +%%Y-%m-%d_H:%M:%S).png && notify-send \"Screenshot\" \"Saved to file\" -i flameshot"
rofiLauncher :: MonadIO m => m ()
rofiLauncher = spawn "rofi -show combi"
-- rofiLauncher = spawn "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"candy-icons\" "
restartXmonad = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

-- To see key masks :
-- `$ xev` or look up`Graphics.X11.Types.KeyMask`
-- M means a frequent keybinding
-- M-S means reset/moving
-- M-C means control
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
    [
    -- Quit xmonad
    ((modm .|. shiftMask, xK_q), io exitSuccess)
    -- Restart xmonad
    , ((modm, xK_q), restartXmonad)
    -- close focused window
    , ((modm , xK_d), kill)
    ------------
    --- APPS
    ------------
    -- open apps
    , ((modm, xK_o), rofiLauncher)
    , ((modm .|. shiftMask, xK_o), spawn "~/.bin/rofi_quicklinks.sh")
    , ((modm , xK_o), spawn "~/.bin/rofi_open_pdf.sh")
    , ((modm, xK_e), spawn "emacsclient --eval \"(emacs-everywhere)\"")
    , ((modm, xK_s), spawn "google-chrome-stable open.spotify.com --new-window")
    -- launch a terminal
    , ((modm , xK_Return), spawn $ XMonad.terminal conf)
    -- lock screen
    , ((0, xK_Pause), spawn "betterlockscreen -l")
    -- Screenshot
    , ((0,                    xK_Print), maimcopy)
    , ((modm,                 xK_Print), maimsave)
    -- clipboard
    -- , ((modm .|. shiftMask, xK_a     ), clipboardy)
    -----------
    --- GAPS
    -----------
    , ((modm, xK_f), toggleFull)
    , ((modm .|. controlMask, xK_g), sendMessage ToggleGaps)               -- fullscreen, some app support F11 as well
    , ((modm .|. shiftMask, xK_g), sendMessage $ setGaps myGaps) -- reset the GapSpec
    -- , ((modm .|. controlMask, xK_Up), sendMessage $ IncGap 10 L)              -- increment the left-hand gap
    -- , ((modm .|. shiftMask, xK_Up     ), sendMessage $ DecGap 10 L)           -- decrement the left-hand gap
    -- , ((modm .|. controlMask, xK_Up), sendMessage $ IncGap 10 U)              -- increment the top gap
    -- , ((modm .|. shiftMask, xK_Up), sendMessage $ DecGap 10 U)           -- decrement the top gap
    -- , ((modm .|. controlMask, xK_u), sendMessage $ IncGap 10 D)              -- increment the bottom gap
    -- , ((modm .|. shiftMask, xK_u     ), sendMessage $ DecGap 10 D)           -- decrement the bottom gap
    -- , ((modm .|. controlMask, xK_i), sendMessage $ IncGap 10 R)              -- increment the right-hand gap
    -- , ((modm .|. shiftMask, xK_i     ), sendMessage $ DecGap 10 R)           -- decrement the right-hand gap
    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)
    -----------
    --- WINDOWS
    -----------
    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    -- Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)
    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )
    -- Reset focus to the master window
    -- , ((modm .|. shiftMask, xK_m     ), windows W.focusMaster  )
    -- Reset the focused window and the master window
    , ((modm .|. shiftMask, xK_m), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp    )
    -- Shrink the master area
    , ((modm .|. controlMask, xK_h), sendMessage Shrink)
    -- Expand the master area
    , ((modm .|. controlMask, xK_l), sendMessage Expand)
    -- Reset window back into tiling
    , ((modm .|. shiftMask, xK_t), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm .|. controlMask, xK_comma), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm .|. controlMask, xK_period), sendMessage (IncMasterN (-1)))
    ------------
    --- MORE KEYS
    ------------
    -- Audio keys
    , ((0,                    xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0,                    xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0,                    xF86XK_AudioNext), spawn "playerctl next")
    , ((0,                    xF86XK_AudioRaiseVolume), spawn "amixer set Master 5%+ unmute")
    , ((0,                    xF86XK_AudioLowerVolume), spawn "amixer set Master 5%- unmute")
    , ((0,                    xF86XK_AudioMute), spawn "pactl set-sink-mute 1 toggle")
    -- Brightness keys
    , ((0,                    xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%")
    , ((0,                    xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%")
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((mod2 .|. modm, num), windows $ func workspace)
        | (workspace, num) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (func, mod2) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings XConfig {XMonad.modMask = modm} = M.fromList
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), \w -> focus w >> windows W.shiftMaster)
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
    ]

------------------------------------------------------------------------
-- Layouts:
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
myLayout = gaps myGaps
            $ spacingRaw False (Border myWindowGaps 0 myWindowGaps 0) True (Border 0 myWindowGaps 0 myWindowGaps) True
            $ smartBorders
            $ avoidStruts(tiled ||| Mirror tiled ||| Full)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty


------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-----------------------------------------------------------------------
-- Run xmonad with the settings you specify. No need to modify this.
--
main = xmonad . fullscreenSupport . docks $ ewmh def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
      -- hooks, layouts
        manageHook = myManageHook, 
        layoutHook = myLayout,
        logHook            = myLogHook,
        handleEventHook    = myEventHook,
        startupHook        = myStartupHook
    }

-- the sxiv app (and maybe others) believes that fullscreen is not supported,
-- so this fixes that.
-- see: https://mail.haskell.org/pipermail/xmonad/2017-March/015224.html
-- and: https://github.com/xmonad/xmonad-contrib/pull/109
addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

toggleFull = withFocused (\windowId -> do
    { floats <- gets (W.floating . windowset);
        if windowId `M.member` floats
        then withFocused $ windows . W.sink
        else withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1) })


--mkDbusClient :: IO D.Client
--mkDbusClient = do
--  dbus <- D.connectSession
--  D.requestName dbus (D.busName_ "org.xmonad.log") opts
--  return dbus
-- where
--  opts = [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]
--
---- Emit a DBus signal on log updates
--dbusOutput :: D.Client -> String -> IO ()
--dbusOutput dbus str =
--  let opath  = D.objectPath_ "/org/xmonad/Log"
--      iname  = D.interfaceName_ "org.xmonad.Log"
--      mname  = D.memberName_ "Update"
--      signal = (D.signal opath iname mname)
--      body   = [D.toVariant $ UTF8.decodeString str]
--  in  D.emit dbus $ signal { D.signalBody = body }
--
--polybarHook :: D.Client -> PP
--polybarHook dbus =
--  let wrapper c s | s /= "NSP" = wrap ("%{F" <> c <> "} ") " %{F-}" s
--                  | otherwise  = mempty
--      blue   = "#2E9AFE"
--      gray   = "#7F7F7F"
--      orange = "#ea4300"
--      purple = "#9058c7"
--      red    = "#722222"
--  in  def { ppOutput          = dbusOutput dbus
--          , ppCurrent         = wrapper blue
--          , ppVisible         = wrapper gray
--          , ppUrgent          = wrapper orange
--          , ppHidden          = wrapper gray
--          , ppHiddenNoWindows = wrapper red
--          , ppTitle           = shorten 100 . wrapper purple
--          }
