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
import XMonad.Actions.Minimize
import XMonad.Layout.Minimize
import qualified XMonad.Layout.BoringWindows as BW

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
myGaps = [(L,0), (R,0), (U,0), (D,0)]
myWindowGaps = 2

main = xmonad . fullscreenSupportBorder . docks . ewmhFullscreen . ewmh $ def {
        terminal           = "alacritty",
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
    , ("M-q S-s", spawn "shutdown now")
    -- , ("M-q l", spawn "hslock")

    , ("M-d", kill1)
    , ("M-o", spawn "rofi -show combi")
    , ("M-p l", spawn "~/.bin/rofi_quicklinks.sh")
    , ("M-p p", spawn "~/.bin/rofi_open_pdf.sh")
    , ("M-p f", spawn "firefox")
    , ("M-p S-f", spawn "firefox --private-window")
    , ("M-p d", spawn "discord")
    , ("M-p m", spawn "thunderbird")
    , ("M-p n", spawn "logseq")
    , ("M-p s", spawn "spotify-launcher")
    , ("M-p c", spawn "chat-gpt")
    , ("M-y", spawn "~/.bin/rofi_clipboard.sh") -- yank
    , ("M-b", spawn "polybar-msg cmd toggle") -- toggle bar

    -- launch a terminal
    , ("M-<Return>", spawn myTerminal)
    , ("M-C-g", sendMessage ToggleGaps)
    -- , ((modm .|. shiftMask, xK_g), sendMessage $ setGaps myGaps) -- reset the GapSpec
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    , ("M-f", toggleFull)
    , ("M-t", withFocused toggleFloat)
    , ("M-m", withFocused minimizeWindow)
    , ("M-S-m", withLastMinimized maximizeWindowAndFocus)

    , ("M-; h", sendMessage Shrink)
    , ("M-; l", sendMessage Expand)

    , ("M-h", prevWS)
    , ("M-l", nextWS)
    , ("M-<Tab>", toggleWS)

    , ("M-n", sendMessage NextLayout)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    -- , ("M-S-m", promote)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-C-j", rotAllDown)
    , ("M-C-k", rotAllUp)

    , ("<Print>", spawn "~/.bin/maim_clip.sh")
    , ("S-<Print>", spawn "~/.bin/maim_save.sh")

    , ("<XF86AudioPlay>", spawn "playerctl play-pause")
    , ("<XF86AudioPrev>", spawn "playerctl previous")
    , ("<XF86AudioNext>", spawn "playerctl next")
    , ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
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
            $ minimize
            $ BW.boringWindows
            $ tiled ||| monocle ||| float ||| Grid
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall 1 (2/100) (1/2)
     float = simplestFloat
     monocle = Full

myStartupHook = do
    setWMName "LG3D"
    -- spawnOnce "lxsession &" -- conflicts with polybar ewmh module
    -- spawnOnce "nm-applet &"
    -- spawnOnce "fcitx5 &"
    -- spawnOnce "picom &"
    -- spawnOnce "dunst &"
    -- spawnOnce "greenclip daemon &"
    -- spawnOnce "feh --bg-fill ~/Pictures/Wallpapers &"
    -- spawnOnce "emacs --daemon &"
    -- spawnOnce "polybar main &"

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
