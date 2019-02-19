-- Libraries {{{
import Control.Monad
import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Scratchpad

import XMonad.Layout
import XMonad.Layout.Gaps
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Square
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.TwoPane

import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import qualified XMonad.StackSet as W
-- }}}

-- Main module {{{
main = do
  wsPanel     <- spawnPipe wsBar
  statsPanel  <- spawnPipe statsBar
  xmonad
    $ docks
    $ withUrgencyHook NoUrgencyHook
    $ defaultConfig
      -- Simple stuff
      { modMask             = modm
      , terminal            = term
      , focusFollowsMouse   = mouseFocus
      , borderWidth         = bdrSize
      , normalBorderColor   = bdrNormal
      , focusedBorderColor  = bdrFocus
      , workspaces          = workspaces'

      -- Lets hook up
      , handleEventHook     = eventHook
      , logHook             = logHook' wsPanel
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , startupHook         = startupHook'
      } `additionalKeys` keyboard

---- Simple stuff
modm          = mod4Mask
term          = "terminator"
mouseFocus    = False
workspaces'   = myWorkspaces
keyboard      = myKeys
browser       = "firefox"

----- Appearance
bdrSize       = 0
bdrNormal     = fgColor
bdrFocus      = bgColor
font          = "Misc Termsyn.Icons:size=13"
monitorSize   = 1980
monitor n     = show(round(monitorSize * n))
monitor' n    = round(monitorSize * n)

----- WHAT COLOR?
bgColor       = "#B79288"
fgColor       = "#F2EBEA"
layoutColor   = "#AA3355"
wsBgColor     = "#f1f3f4"
wsFgColor     = bgColor
barBgColor    = bgColor
barFgColor    = fgColor
hintColor     = layoutColor

---- Panel
leftBarSize   = monitor 0.7
rightBarSize  = monitor 0.3
leftBarPos    = "0"
rightBarPos   = leftBarSize
barHeight     = "26"

wsBar      =
  "dzen2 -dock -ta l      \
  \ -bg '" ++ barBgColor  ++ "' \
  \ -fg '" ++ barFgColor  ++ "' \
  \ -w  '" ++ leftBarSize ++ "' \
  \ -h  '" ++ barHeight   ++ "' \
  \ -x  '" ++ leftBarPos  ++ "' \
  \ -fn '" ++ font        ++ "' "

statsPanel      =
  "dzen2 -dock -ta r      \
  \ -bg '" ++ barBgColor   ++ "'\
  \ -fg '" ++ barFgColor   ++ "'\
  \ -w  '" ++ rightBarSize ++ "'\
  \ -h  '" ++ barHeight    ++ "'\
  \ -x  '" ++ rightBarPos  ++ "'\
  \ -fn '" ++ font         ++ "'"

statusConfig  = "conky -c ~/.xmonad/status.conf"
statsBar      = statusConfig ++ " | " ++ statsPanel

---- Hooks
eventHook     = fullscreenEventHook
layoutHook'   = myLayoutHook
logHook'      = myLogHook
manageHook'   = myManageHook <+> manageScratchPad
startupHook'  = myStartupHook
-- }}}

-- Log Hook {{{
myLogHook h =
  dynamicLogWithPP $
  dzenPP
    { ppOutput  = hPutStrLn h
    , ppCurrent = dzenColor (fg) (bg) . pad
    , ppVisible = pad
    , ppHidden  = pad . noScratchPad
    , ppUrgent  = dzenColor (bg) (hint) . pad
    , ppSep     = ""
    , ppOrder   = \(ws:l:t:_) -> [l, ws]
    , ppLayout  = dzenColor (bg) (layoutBg) . pad . pad .
        ( \t -> case t of
          "Tall" -> "þ"
          "Mirror Tall" -> "ü"
          "Full" -> "ÿ"
          _ -> t
        )
    }
  where
    bg = wsBgColor
    fg = wsFgColor
    hint = hintColor
    layoutBg = layoutColor
    noScratchPad ws = if ws == "NSP" then "" else ws
-- }}}

-- Workspaces {{{
myWorkspaces = ws $ ["TERM", "INET", "DEV", "ENT", "PLAY", "PROD"]
  where
    ws l =
      [ "^ca(1,xdotool key super+" ++ show n ++ ")  " ++ ws ++ "  ^ca()"
      | (i, ws) <- zip [1 ..] l
      , let n = i
      ]
-- }}}

-- Layout Hook {{{
myLayoutHook =
  avoidStruts
  $ mkToggle (NOBORDERS ?? FULL ?? EOT)
  $ onWorkspace (w !! 0) termLayout
  $ onWorkspace (w !! 1) webLayout
  $ standardLayout
  where
    w = workspaces'
    termLayout =
      gaps [(L,50), (U,50), (R,50), (D,50)] $
      standardLayout
    webLayout = Full ||| Tall (1) (3/100) (1/2)
    standardLayout =
      smartBorders $
      renamed [CutWordsLeft 1] $
      smartSpacingWithEdge 8 $ layoutHook defaultConfig
-- }}}

-- Manage Hook {{{
myManageHook =
    composeAll . concat $
    [ [ className =? c --> doShift (w !! 1) | c <- inetApp ]
    , [ className =? c --> doShift (w !! 2) | c <- devApp ]
    , [ className =? c --> doShift (w !! 3) | c <- entApp ]
    , [ className =? c --> doShift (w !! 4) | c <- playApp ]
    , [ className =? c --> doShift (w !! 5) | c <- prodApp ]
    , [ className =? c --> doFloat          | c <- floatingApp ]
    , [ className =? c --> doIgnore         | c <- ignoreApp ]
    , [ isDialog       --> doCenterFloat ]
    , [ isRole         --> doCenterFloat ]
    , [ manageDocks ]
    , [ manageHook def ]
    ]
    where
      w = workspaces'
      isRole = stringProperty "WM_WINDOW_ROLE" =? "pop-up"
      inetApp = ["Chromium", "Firefox"]
      devApp =
        [ "SecureCRT", "GNS3", "VirtualBox Manager"
        , "VirtualBox Machine", "jetbrains-studio"
        , "Code", "oni"
        ]
      entApp = ["MPlayer", "smplayer", "mpv", "Gimp"]
      playApp = ["player", "Genymotion Player"]
      prodApp = ["MuPDF"]
      floatingApp = ["SecureCRT", "TeamViewer", "Xmessage"]
      ignoreApp = ["desktop", "desktop_window", "stalonetray", "trayer"]
  -- }}}

-- Scratchpad {{{
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.3
    w = 1
    t = 1 - h
    l = 1 - w
-- }}}

-- Startup Hook {{{
myStartupHook = do
  spawnOnce "feh --bg-fill $HOME/.xmonad/background.png"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "compton --config /dev/null -bGC \
            \ --focus-exclude \"class_g = 'Dmenu'\" \
            \ --inactive-dim 0.2 "
  spawnOnce "sleep 1.0 && setxkbmap -option"
  spawnOnce "sleep 1.0 && setxkbmap -layout us,ru -variant -option grp:alt_shift_toggle,grp_led:scroll"
  spawnOnce "xclip"
  spawnOnce "terminator"
  spawnOnce "firefox"
-- }}}


-- Keymapping {{{
-- /usr/include/X11/keysymdef.h
myKeys =
  [ ((m, xK_b), spawn browser)
  , ((m, xK_p), spawn dmenu)
  , ((m, xK_s), sendMessage ToggleStruts)
  , ((m, xK_BackSpace), focusUrgent)
  , ((m, xK_equal), toggleWS)
  , ((m, xK_grave), toggleWS)
  , ((m, xK_minus), scratchPad)
  , ((m, xK_f), sendMessage $ Toggle FULL)
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 5%-")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 5%+")
  , ((0, xF86XK_MonBrightnessDown), spawn "light -U 10")
  , ((0, xF86XK_MonBrightnessUp), spawn "light -A 10")
  , ((0, xK_Print), spawn "gnome-screenshot")
  , ((s, xK_Print), spawn "maim | xclip -selection clipboard -t image/png")
  ]
  where
    m = modm
    s = shiftMask
    c = controlMask
    dmenu =
      "dmenu_run -i \
      \ -fn '" ++ fn ++ "' \
      \ -nf '" ++ fgColor ++ "' \
      \ -sf '" ++ fgColor ++ "' \
      \ -nb '" ++ bgColor ++ "' \
      \ -sb '" ++ layoutColor ++ "'"
      where
        fn = "Misc Termsyn.Icons:size=18"
    scratchPad = scratchpadSpawnActionTerminal term
-- }}}
