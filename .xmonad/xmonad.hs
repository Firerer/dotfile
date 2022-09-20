{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures -Wno-unused-imports #-}

import Control.Monad ( join, when )
import Data.List
import Data.Maybe (maybeToList)
import Data.Monoid
import Graphics.X11.ExtraTypes.XF86
import System.Exit
import XMonad
import XMonad.Prompt
import qualified Data.Map        as M
import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves
import XMonad.Actions.Search
import XMonad.Actions.WithAll

import XMonad.Util.EZConfig
import XMonad.Util.SpawnOnce ( spawnOnce )

import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers ( doFullFloat, isFullscreen )
import XMonad.Hooks.WindowSwallowing

import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps(gaps, setGaps, GapMessage(DecGap, ToggleGaps, IncGap))
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )


myWorkspaces = ["1:\63083", "2:\63288", "3:\63306", "4:\61723", "5:\63107", "6:\63601", "7:\63391", "8:\61713", "9:\61884"]
--myWorkspaces   = ["Code" , "Web" , "Term" , "Game", "5","6","7","8","9"]
myTerminal = "alacritty"
myGaps = [(L,0), (R,0), (U,0), (D,0)]
myWindowGaps = 2

main = xmonad . fullscreenSupportBorder . docks . ewmhFullscreen . ewmh $ def {
        terminal           = myTerminal,
        focusFollowsMouse  = True,
        clickJustFocuses   = False,
        borderWidth        = 2,
        modMask            = mod4Mask,
        workspaces         = myWorkspaces,
        normalBorderColor  = "#3b4050",
        focusedBorderColor = "#bc96da",
        -- key bindings
        keys               = myKeys,
        -- hooks, layouts
        manageHook         = myManageHook, 
        layoutHook         = myLayout,
        logHook            = dynamicLog,
        handleEventHook    = swallowEventHook (className =? "Alacritty" <||> className =? "XTerm") (return True),
        startupHook        = myStartupHook
    }
    `additionalKeysP`
    myEZkeys

myEZkeys :: [(String, X ())]
myEZkeys = 
    [ ("M-q r", spawn "xmonad --recompile && xmonad --restart")
    , ("M-q S-r", spawn "reboot")
    , ("M-q q", io exitSuccess)
    , ("M-q d", killAll)
    , ("M-q s", spawn "systemctl suspend")
    , ("M-q S-s", spawn "shutdown")
    , ("M-q l", spawn "hslock")

    , ("M-d", kill1)
    , ("M-o", spawn "rofi -show combi")
    , ("M-p l", spawn "~/.bin/rofi_quicklinks.sh")
    , ("M-p p", spawn "~/.bin/rofi_open_pdf.sh")
    , ("M-p f", spawn "firefox")
    , ("M-p e", spawn "emacsclient emacs")
    , ("M-p s", spawn "firefox --new-window open.spotify.com ")
    , ("M-y", spawn "~/.bin/rofi_clipboard.sh") -- yank
    , ("M-b", spawn "polybar-msg cmd toggle") -- toggle bar
    -- TODO use neovim
    -- , ((modm, xK_e), spawn "emacsclient --eval \"(emacs-everywhere)\"") 
    -- launch a terminal
    , ("M-<Return>", spawn myTerminal)
    -- lock screen
    , ("<Pause>", spawn "hslock")
    , ("M-C-g", sendMessage ToggleGaps)
    -- , ((modm .|. shiftMask, xK_g), sendMessage $ setGaps myGaps) -- reset the GapSpec
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    , ("M-f", toggleFull)
    , ("M-t", withFocused toggleFloat)

    , ("M-; h", sendMessage Shrink)
    , ("M-; l", sendMessage Expand)

    , ("M-h", prevWS)
    , ("M-l", nextWS)
    , ("M-<Tab>", toggleWS)

    , ("M-n", sendMessage NextLayout)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-S-m", promote)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-C-j", rotAllDown)
    , ("M-C-k", rotAllUp)

    , ("<Print>", spawn "~/.bin/maim_clip.sh")
    , ("S-<Print>", spawn "~/.bin/maim_save.sh")

    , ("<XF86AudioPlay>", spawn "mocp --play")
    , ("<XF86AudioPrev>", spawn "mocp --previous")
    , ("<XF86AudioNext>", spawn "mocp --next")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute")
    ]
    -- mod-[1..],       Switch to workspace N
    -- mod-shift-[1..], Move client to workspace N
    ++ [ (m ++ "M-" ++ [k], windows $ f i) 
       | (i, k) <- zip myWorkspaces "123456789"
       , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]]
    ++ [("M-s " ++ k, promptSearch def f) | (k,f) <- searchList ]
    ++ [("M-S-s " ++ k, selectSearch f) | (k,f) <- searchList ]

searchList = [ ("g", google)
             , ("h", hoogle)
             , ("w", wikipedia)
             ]
------------------------------------------------------------------------
-- Layouts:
myLayout = gaps myGaps
            $ spacingRaw False (Border myWindowGaps 0 myWindowGaps 0) True (Border 0 myWindowGaps 0 myWindowGaps) True
            $ smartBorders
            $ avoidStruts 
            $ tiled ||| monocle ||| float ||| Grid
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall 1 (2/100) (1/2)
     float = simplestFloat
     monocle = noBorders Full

myStartupHook = do
   setWMName "LG3D"
   -- spawnOnce "lxsession &" -- conflicts with polybar ewmh module
   spawnOnce "nm-applet &"
   spawnOnce "fcitx5 &"
   -- spawnOnce "picom &"
   spawnOnce "dunst &"
   spawnOnce "greenclip daemon &"
   spawnOnce "feh --bg-fill ~/Pictures/wallpapers &"
   spawnOnce "emacs --daemon &"
   spawnOnce "polybar main &"

------------------------------------------------------------------------
-- Window rules:
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
myManageHook = fullscreenManageHook <+> manageDocks <+> composeAll
    ([resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , isFullscreen --> doFullFloat
    ]
    ++ [fmap (c `isInfixOf`) className --> doFloat | c <- floatClassInfixes ]
    ++ [fmap (c `isInfixOf`) resource --> doFloat | c <- floatResourceInfixes ]
    ++ [fmap (c ==) className --> doShift (myWorkspaces !! (i-1)) | (c, i) <- shfitClassInfixes ]
    )
    where
      floatClassInfixes = [ "xmessage", "MPlayer", "Gimp", "pavucontrol"]
      floatResourceInfixes = ["Dialog", "control", "pavucontrol"]
      -- TODO: some shifts are not working
      shfitClassInfixes = [("zoom", 3),
                           ("Steam", 4), ("Lutris", 4), ("battle.net.exe", 4),
                           ("Discord", 5), ("VirtualBox", 6), ("Spotify", 9)]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
-- To see key masks :
-- `xev` or look up`Graphics.X11.Types.KeyMask`
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
    -- Quit xmonad
    [ ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
    , ((0, xF86XK_AudioPrev), spawn "playerctl previous")
    , ((0, xF86XK_AudioNext), spawn "playerctl next")
    , ((0, xF86XK_MonBrightnessUp), spawn "brightnessctl s +10%")
    , ((0, xF86XK_MonBrightnessDown), spawn "brightnessctl s 10-%")
    ]++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

toggleFull = withFocused (\windowId -> do
    { floats <- gets (W.floating . windowset);
        if windowId `M.member` floats
        then withFocused $ windows . W.sink
        else withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1) })

toggleFloat w = windows (\s -> if M.member w (W.floating s)
                            then W.sink w s
                            else (W.float w (W.RationalRect (1/3) (1/4) (1/2) (4/5)) s))
-- the sxiv app (and maybe others) believes that fullscreen is not supported,
-- so this fixes that.
-- see: https://mail.haskell.org/pipermail/xmonad/2017-March/015224.html
-- and: https://github.com/xmonad/xmonad-contrib/pull/109
-- addNETSupported :: Atom -> X ()
-- addNETSupported x   = withDisplay $ \dpy -> do
--     r               <- asks theRoot
--     a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
--     a               <- getAtom "ATOM"
--     liftIO $ do
--        sup <- join . maybeToList <$> getWindowProperty32 dpy a_NET_SUPPORTED r
--        when (fromIntegral x `notElem` sup) $
--          changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

-- addEWMHFullscreen :: X ()
-- addEWMHFullscreen   = do
    -- wms <- getAtom "_NET_WM_STATE"
    -- wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
    -- mapM_ addNETSupported [wms, wfs]
