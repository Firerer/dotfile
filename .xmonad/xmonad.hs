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
import XMonad.Util.SpawnOnce (spawnOnce)

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition (insertPosition, Focus(Newer), Position(Below, End))
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doFullFloat, isFullscreen, doCenterFloat)
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WindowSwallowing

import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps (gaps, GapMessage(DecGap, ToggleGaps, IncGap))
import XMonad.Layout.Grid
-- import XMonad.Layout.ResizableTile (ResizableTall, MirrorResize(MirrorShrink, MirrorExpand))
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.SimplestFloat
import XMonad.Layout.WindowNavigation (windowNavigation, Navigate(Go, Swap), Direction2D(L, R, U, D))
import XMonad.Layout.Spacing ( spacingRaw, Border(Border) )


-- myWorkspaces = ["1:\63083", "2:\63288", "3:\63306", "4:\61723", "5:\63107", "6:\63601", "7:\63391", "8:\61713", "9:\61885", "10:\61884"]
myWorkspaces   = ["1" , "2" , "3" , "4", "5", "6", "7", "8", "9"]
myTerminal = "alacritty"

main = xmonad . fullscreenSupportBorder . docks . ewmhFullscreen . ewmh $ def {
        terminal           = myTerminal,
        focusFollowsMouse  = True,
        clickJustFocuses   = False,
        borderWidth        = 2,
        modMask            = mod4Mask,
        workspaces         = myWorkspaces,
        normalBorderColor  = "#000000",
        focusedBorderColor = "#bc96da",
        -- key bindings
        keys               = myKeys,
        -- hooks, layouts
        manageHook         = myManageHook,
        layoutHook         = myLayout,
        logHook            = dynamicLog,
        --handleEventHook    = swallowEventHook (className =? "Alacritty" <||> className =? "XTerm") (return True),
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

    , ("M-<Return>", spawn myTerminal)
    , ("M-o", spawn "rofi -show combi")
    , ("M-p S-f", spawn "firefox --private-window")
    , ("M-p j", spawn "~/.bin/journal.sh")
    -- , ("M-p j", spawn "$TERM --title take_journal -e $EDITOR ~/Documents/logseq/journals/$(date +%Y_%m_%d).md")
    , ("M-p d", spawn "discord")
    , ("M-p f", spawn "firefox")
    , ("M-p g", spawn "google-chrome-stable")
    , ("M-p l", spawn "~/.bin/rofi_quicklinks.sh")
    , ("M-p m", spawn "thunderbird")
    , ("M-p n", spawn "flatpak run com.logseq.Logseq")
    , ("M-p p", spawn "~/.bin/rofi_open_pdf.sh")
    -- , ("M-p s", spawn "spotify-launcher")
    --, ("M-y", spawn "~/.bin/rofi_clipboard.sh") -- yank, use fcitx5's clipboard instead
    , ("M-b", spawn "polybar-msg cmd toggle") -- toggle bar

    , ("M-C-g", sendMessage ToggleGaps)
    , ("M-d", kill1)
    , ("M-f", toggleFull)
    , ("M-t", withFocused toggleFloat)
    , ("M-m", withFocused minimizeWindow)
    , ("M-S-m", withLastMinimized maximizeWindowAndFocus)

    , ("M-C-h", prevWS)
    , ("M-C-l", nextWS)
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    , ("M-S-h", sendMessage MirrorShrink)
    , ("M-S-l", sendMessage MirrorExpand)
    , ("M-<Tab>", toggleWS)

    , ("M-n", sendMessage NextLayout)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)

    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-C-j", rotAllDown)
    , ("M-C-k", rotAllUp)

    , ("M-<Left>", sendMessage $ Go L)
    , ("M-<Right>", sendMessage $ Go R)
    , ("M-<Up>", sendMessage $ Go U)
    , ("M-<Down>", sendMessage $ Go D)
    , ("M-C-<Left>", sendMessage $ Swap L)
    , ("M-C-<Right>", sendMessage $ Swap R)
    , ("M-C-<Up>", sendMessage $ Swap U)
    , ("M-C-<Down>", sendMessage $ Swap D)


    , ("<Print>", spawn "~/.bin/maim_clip.sh")
    , ("C-<Print>", spawn "~/.bin/maim_ocr_clip.sh")
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
    ++ [ ("M-" ++ mod ++ [index], windows $ action workspace)
       | (workspace, index) <- zip myWorkspaces "1234567890"
       , (action, mod) <- [(W.greedyView, ""), (W.shift, "S-")]]

------------------------------------------------------------------------
-- Layouts:
myWindowGaps = [(L,0), (R,0), (U,0), (D,0)]
myGapSize = 2
myLayout = gaps myWindowGaps
            $ spacingRaw False (Border myGapSize 0 myGapSize 0) True (Border 0 myGapSize 0 myGapSize) True
            $ smartBorders
            $ avoidStruts
            $ minimize
            $ BW.boringWindows
            $ windowNavigation
            $ tiled ||| Full ||| float ||| Grid
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = ResizableTall 1 (2/100) (1/2) []
     float = simplestFloat

myStartupHook = do
    setWMName "LG3D"

------------------------------------------------------------------------
-- Window rules:
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
myManageHook = insertPosition Below Newer
    <+> fullscreenManageHook
    <+> manageDocks
    <+> composeAll
    ([resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , title     =? "take_journal" --> doCenterFloat
    , title =? "Friends List" --> doFloat
    , isFullscreen --> doFullFloat
    ]
    -- ++ [fmap (c ==) title --> doFloat | c <- floatTitleInfixes ]
    ++ [fmap (c `isInfixOf`) className --> doFloat | c <- floatClassInfixes ]
    ++ [fmap (c `isInfixOf`) resource --> doFloat | c <- floatResourceInfixes ]
    ++ [fmap (c ==) className --> doShift (myWorkspaces !! (i-1)) | (c, i) <- shfitClassInfixes ]
    )
    where
      -- floatTitleInfixes = [ "take_journal" ]
      floatClassInfixes = [ "xmessage", "MPlayer", "Gimp", "zenity" ]
      floatResourceInfixes = ["Dialog", "control", "pavucontrol"]
      shfitClassInfixes = [ --("zoom", 3),
          ("Steam", 4), ("Lutris", 4), ("battle.net.exe", 4)
          , ("Discord", 5), ("wechat", 5), ("Wine", 5)
          , ("VirtualBox", 6), ("Spotify", 10)
          ]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
-- To see key masks :
-- `xev` or look up`Graphics.X11.Types.KeyMask`
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
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
