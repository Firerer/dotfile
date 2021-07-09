{-# OPTIONS_GHC -Wno-missing-signatures #-}

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.SpawnOnce ( spawnOnce )
import XMonad.Hooks.EwmhDesktops ( ewmh )
import XMonad.Hooks.ManageHelpers ( doFullFloat, isFullscreen )
import XMonad.Hooks.ManageDocks( avoidStruts, docks, manageDocks, Direction2D(D, L, R, U) )
import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenManageHook, fullscreenSupport)
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )
import XMonad.Layout.Gaps(gaps, setGaps, GapMessage(DecGap, ToggleGaps, IncGap))
import Graphics.X11.ExtraTypes.XF86 (xF86XK_AudioLowerVolume, xF86XK_AudioRaiseVolume, xF86XK_AudioMute, xF86XK_MonBrightnessDown, xF86XK_MonBrightnessUp, xF86XK_AudioPlay, xF86XK_AudioPrev, xF86XK_AudioNext)

import Control.Monad ( join, when )
import Data.Monoid ()
import Data.Maybe (maybeToList)
import qualified Data.Map        as M
import System.Exit

myTerminal      = "alacritty"
myFocusFollowsMouse = True
myClickJustFocuses = False
myBorderWidth   = 2
myModMask       = mod4Mask
myWorkspaces   = ["Code" , "Web" , "Term" , "Game", "5","6","7","8","9"]
--myWorkspaces    = ["\63083", "\63288", "\63306", "\61723", "\63107", "\63601", "\63391", "\61713", "\61884"]
myNormalBorderColor  = "#3b4050"
myFocusedBorderColor = "#bc96da"
myGaps = [(L,0), (R,0), (U,30), (D,0)]
myWindowGaps = 5
myStartupHook = do
  spawnOnce "lxsession &"
  spawnOnce "nitrogen --restore &"
  spawnOnce "picom --experimental-backends"
  spawnOnce "greenclip daemon" -- clipboard
  spawnOnce "dunst"
  spawn "exec ~/bin/lock.sh"
  --spawn "xsetroot -cursor_name left_ptr"
  spawnOnce "polybar example &"
  spawnOnce "emacs --deamon &"


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
clipboardy :: MonadIO m => m () -- Don't question it 
clipboardy = spawn "rofi -modi \"\63053 :greenclip print\" -show \"\63053 \" -run-command '{cmd}' -theme ~/.config/rofi/launcher/style.rasi"
maimcopy = spawn "maim -s | xclip -selection clipboard -t image/png && notify-send \"Screenshot\" \"Copied to Clipboard\" -i flameshot"
maimsave = spawn "maim -s ~/Desktop/$(date +%Y-%m-%d_%H-%M-%S).png && notify-send \"Screenshot\" \"Saved to Desktop\" -i flameshot"
rofi_launcher = spawn "rofi -no-lazy-grab -show drun -modi run,drun,window -theme $HOME/.config/rofi/launcher/style -drun-icon-theme \"candy-icons\" "
restart_xmonad = spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

-- To see key masks :
-- `$ xev` or look up`Graphics.X11.Types.KeyMask`
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
    -- lock screen
    -- (fn , pause) is sleep, system default keybinding
    , ((0, xK_Pause), spawn "betterlockscreen -l")
    -- open apps
    , ((modm, xK_o), rofi_launcher)
    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)
    -- Screenshot
    , ((0,                    xK_Print), maimcopy)
    , ((modm,                 xK_Print), maimsave)
    -- clipboard
    , ((modm .|. shiftMask, xK_a     ), clipboardy)

    -- GAPS!!!
    , ((modm .|. controlMask, xK_g), sendMessage $ ToggleGaps)               -- fullscreen, some app support F11 as well
    , ((modm .|. shiftMask, xK_g), sendMessage $ setGaps myGaps) -- reset the GapSpec
    
    , ((modm .|. controlMask, xK_Up), sendMessage $ IncGap 10 L)              -- increment the left-hand gap
    , ((modm .|. shiftMask, xK_Up     ), sendMessage $ DecGap 10 L)           -- decrement the left-hand gap
    
    , ((modm .|. controlMask, xK_Up), sendMessage $ IncGap 10 U)              -- increment the top gap
    , ((modm .|. shiftMask, xK_Up), sendMessage $ DecGap 10 U)           -- decrement the top gap

    , ((modm .|. controlMask, xK_u), sendMessage $ IncGap 10 D)              -- increment the bottom gap
    , ((modm .|. shiftMask, xK_u     ), sendMessage $ DecGap 10 D)           -- decrement the bottom gap

    , ((modm .|. controlMask, xK_i), sendMessage $ IncGap 10 R)              -- increment the right-hand gap
    , ((modm .|. shiftMask, xK_i     ), sendMessage $ DecGap 10 R)           -- decrement the right-hand gap

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k), windows W.swapUp    )

    -- Shrink the master area
    , ((modm, xK_h), sendMessage Shrink)

    -- Expand the master area
    , ((modm, xK_l), sendMessage Expand)

    -- Push window back into tiling
    , ((modm, xK_t), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm, xK_comma), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm, xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess) )

    -- Restart xmonad
    , ((modm              , xK_q     ), restart_xmonad)

    -- Audio keys
    , ((0,                    xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0,                    xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0,                    xF86XK_AudioNext), spawn "playerctl next")
    , ((0,                    xF86XK_AudioRaiseVolume), spawn "pactl set-sink-volume 0 +5%")
    , ((0,                    xF86XK_AudioLowerVolume), spawn "pactl set-sink-volume 0 -5%")
    , ((0,                    xF86XK_AudioMute), spawn "pactl set-sink-mute 0 toggle")

    -- Brightness keys
    , ((0,                    xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%")
    , ((0,                    xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%")
    ]
    ++

    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

------------------------------------------------------------------------
-- Layouts:
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
myLayout = avoidStruts(tiled ||| Mirror tiled ||| Full)
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
-- Window rules:
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen --> doFullFloat
                                 ]

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
main = xmonad $ fullscreenSupport $ docks $ ewmh defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
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
        layoutHook = gaps myGaps
            $ spacingRaw False (Border myWindowGaps 0 myWindowGaps 0) True (Border 0 myWindowGaps 0 myWindowGaps) True
          --  $ spacingRaw True (Border 10 10 10 10) True (Border 10 10 10 10) True
            $ smartBorders $ myLayout,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook >> addEWMHFullscreen
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
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
    wms <- getAtom "_NET_WM_STATE"
    wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    mapM_ addNETSupported [wms, wfs]

-- TODO: start from xdm, copyed from @randomthoughts
-- ln -s ~/.xmonad/bin/xsession ~/.xsession
-- # Logout, login from lightdm/xdm/kdm/gdm
