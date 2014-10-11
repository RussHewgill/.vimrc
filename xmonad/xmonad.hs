-- vim: set foldlevel=0 :
{- Notes {{{
 -
 - Most of this was done while learning haskell
 - goal of config was more or less to behave like i3
 - used some other programs (dzen2, conky) for statusbar
 - music stuff for dzen just uses dbus to talk with clementine
 - {{{ }}} is for vim folding, makes config ~30 lines
 -
}}}-}

-- Imports {{{

import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.FloatKeys
import XMonad.Actions.Navigation2D
import XMonad.Actions.PhysicalScreens
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place
import XMonad.Layout.Grid
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.WindowProperties
import XMonad.Util.WorkspaceCompare
import qualified XMonad.StackSet as W

import System.IO
import System.Exit
import Data.List
import Data.Ratio
import Control.Applicative
import Control.Monad
import Data.Maybe

-- }}}

-- Main {{{
main = do
    spawn "~/bin/wp"
    spawn "unclutter -grab"
    spawn "wmname LG3D"

    ldzenproc <- spawnPipe mydzenl
    spawn mydzenclockl
    
    -- only spawn second set of dzen bars when 2+ monitors are connected
    numscreens <- countScreens
    if numscreens == (1 :: Integer)
        then xmonad $ runbars laptopconf ldzenproc Nothing
        else do
        rdzenproc <- spawnPipe mydzenr
        spawn mydzenclockr
        xmonad $ ewmh $ runbars dualheadconf ldzenproc (Just rdzenproc)

-- }}}

-- Main Config {{{

conf = withNavigation2DConfig defaultNavigation2DConfig 
        $ defaultConfig
        {
          manageHook  = mymanagehook <+> namedScratchpadManageHook scratchpads -- <+> manageScratchPad 
                        <+> manageHook defaultConfig <+> manageDocks 
        , layoutHook  = mylayoutHook
        , terminal    = "roxterm"
        , modMask     = mod4Mask
        , borderWidth = 0
        , workspaces  = wss
        , focusFollowsMouse = True 
        , handleEventHook = handleEventHook defaultConfig <+> fullscreenEventHook
        } 

dualheadconf = conf `additionalKeysP` mykeysdualhead `removeKeysP` notKeysP

laptopconf = conf `additionalKeysP` mykeysonehead `removeKeysP` notKeysP

-- }}}

-- PPlogger {{{

--Runs 2 statusbars for 2+ monitors, 1 otherwise
runbars :: XConfig l -> Handle -> Maybe Handle -> XConfig l
runbars x left Nothing = x { logHook = myPPlog left (1 :: Integer) }
runbars x left (Just right) = x { logHook= myPPlog left (0 :: Integer) >> myPPlog right (1 :: Integer) }

-- produce a log for a handle and screen
myPPlog handle _ = dynamicLogWithPP . namedScratchpadFilterOutWorkspacePP $ dzenPP
                { ppOutput          = hPutStrLn handle
                , ppCurrent         = dzenColor fg cwsbg . clickable . pad . deTagWS
                , ppTitle           = dzenColor fg bg . wrap "^ca(3, xdotool key super+m)\
                                \^ca(1, xdotool key super+n) " "^ca()^ca()" . dzenEscape . shorten 30
                , ppVisible         = dzenColor fg vwsbg . clickable . pad . deTagWS
                , ppHidden          = dzenColor fg hwsbg . clickable . pad . deTagWS
                , ppHiddenNoWindows = dzenColor emptywsfg hwsbg . clickable . showNamedWs . deTagWS
                , ppSep             = ""
                , ppWsSep           = "^fg(" ++ wssepbg ++ ")^r(2x30)"
                , ppUrgent          = dzenColor fg "red"
                , ppLayout          = wrap ("^fg(" ++ wssepbg ++ ")^r(2x30)^fg() ") (" ^fg(" ++ wssepbg ++ ")^r(2x30)") . layoutifier
                }
        where
            -- add dzen clickable tags to each workspace
            clickable s = wrap ("^ca(1, xdotool key super+" 
                            ++ filter (\x->x `elem` ['1'..'9']) (deTagWS s) 
                            ++ ")") "^ca()" s
            --hide unnamed workspaces until they have windows
            showNamedWs ws = if any (`elem` ws) ['a'..'z']
                                then pad ws else ""
            -- remove per moniter tags from workspace
            deTagWS ws  = if '_' `elem` ws then takeWhile (not . (=='_')) ws else ws
            layoutifier x = case x of
                                "Grid"          -> "NG"
                                "ReflectX Grid" -> "RG"
                                "Tall"          -> "NT"
                                "Mirror Tall"   -> "MT"
                                "ReflectX Tall" -> "RT"
                                _ -> "WAT"

-- }}}

-- Themes {{{
-- light
{-bg        = "#D8D8D8"-}
{-fg        = "#000000"-}
{-cwsbg     = "#0088CC"-}
{-cwsfg     = "171616"-}
{-vwsbg     = "#A3BeCC"-}
{-wssepbg   = "#A8A8A8"-}
{-emptywsbg = "#c4c4c4"-}

-- i3wm clone
bg        = "#171616"
fg        = "#dddddd"
cwsbg     = "#0088CC"
cwsfg     = "#ffffff"
hwsbg     = "#333333"
vwsbg     = "#333333"
wssepbg   = "#5c5c5c"
emptywsbg = "#333333"
emptywsfg = "#888888"
-- }}}

-- Managehook {{{

mymanagehook = composeAll . concat $
    [ [className =? "Firefox"  <&&> resource =? "Navigator" --> doShift (head wss) ]
    , [className =? "Firefox"       --> doF W.swapMaster ]
    , [role =? "pentagvim"            --> doShift (head wss) <+> rect ]
    , [className =? "Firefox" <&&> resource /=? "Navigator" --> doFloat]
    , [className =? "Clementine"    --> doShift (wss!!2) ]
    --TODO: only move if no other gvim is running
    {-, [className =? "Gvim"          --> doShift (wss!!1) ]-}
    , [className =? "Gvim"          --> doF W.swapMaster ]
    , [className =? "Gmrun"         --> doSideFloat CW   ]
    , [className =? "Yakuake"         --> doFloat          ]
    , [className =? "Xmessage"      --> doFloat          ]
    , [className =? "Gsimplecal"    --> placeHook (withGaps (0,0,30,0) $ underMouse (0,0)) ]
    , [isFullscreen                 --> doFullFloat      ]
    , [isFullscreen                 --> (doF W.focusDown <+> doFullFloat)]
    , [fmap (not . isInfixOf c) title --> doF avoidMaster | c <- hiPriority ]
    , [isDialog                     --> (doFloat <+> doF W.focusUp) ]
    {-, [(fmap (isInfixOf c) title <&&> (qnot isDialog)) --> doF avoidMaster | c <- hiPriority ]-}
    , [className =? "Gimp"          --> doShift (wss!!4) ] 
    , [fmap (isInfixOf c) title --> doF W.focusUp | c <- pwds ]
    ] 
    {-<+>-}
    {-composeOne [  ]-}
    where 
        -- These windows go to master pane when started
        hiPriority = ["Firefox", "Gvim", "Hexchat"]
        pwds = [ "Enter password", "Administrator privileges", "Select a Partition or Device" ]
        role = stringProperty "WM_WINDOW_ROLE"
        rect = customFloating $ W.RationalRect 0 0.3 0.3 0.3


-- pure function for use with doF
-- causes new windows to be inserted below master
avoidMaster :: W.StackSet i l a s sd -> W.StackSet i l a s sd
avoidMaster = W.modify' $ \c -> case c of
    W.Stack t [] (r:rs) ->  W.Stack t [r] rs
    _ -> c

-- }}}

-- Layouthook {{{

mylayoutHook =  smartBorders
                $ avoidStruts
                $ onWorkspace (wss!!1) vimlayout 
                $ onWorkspace (head wss) firefoxlayout 
                  mainlayout

mainlayout = Mirror tiled ||| reflectHoriz columns ||| columns
    where
        tiled = Tall { tallNMaster = nmaster, tallRatio=ratio, tallRatioIncrement=delta }
        nmaster = 1
        delta = 3%100
        ratio = 1%2

firefoxlayout = reflectHoriz ff ||| Full
    where
        ff = Tall  { tallNMaster = 1, tallRatio=4%5, tallRatioIncrement=3%100 }

vimlayout = Mirror Tall { tallNMaster = 1, tallRatio=3%5, tallRatioIncrement=3%100 } 
                ||| Grid
                ||| reflectHoriz Grid
                ||| Tall { tallNMaster = 1, tallRatio=1%2, tallRatioIncrement=3%100 }

columns = Tall { tallNMaster = 1, tallRatio=1%2, tallRatioIncrement=3%100 }

-- }}}

-- workspaces {{{

wss :: [WorkspaceId]
wss = ["1:Web_1", "2:Text_2", "3:Tunes_0", "4:Other_0", "5:Other_0", "6_0", "7_0", "8_0", "9_0", "NSP_0"]

scratchpadws :: WorkspaceId
scratchpadws = "NSP_0"

--filter workspaces by tag (_n)
--will crash on empty workspace name
mywssl = filter ((=='1') . last) wss
mywssr = filter ((=='2') . last) wss
mywsso = filter ((=='0') . last) wss

-- }}}

-- dzen/conky {{{

-- most of these settings are specific to my monitor setup
-- 1920x1080 laptop left, larger 1680x1050 right
-- each monitor has 2 dzens, left for pplog and right for clock/date/music
-- important:   -p,     don't timeout
--              -e ''   disable close on right click
--              -y -1   bottom of screen
mydzenclockl = "conky | dzen2 -xs 1 -ta r -x 800 " ++ dzenfont ++ dzenOpts
mydzenclockr = "conky | dzen2 -xs 2 -ta r -x 800 " ++ dzenfont ++ dzenOpts

mydzenl   = "dzen2 -xs 1 -w 800 -ta l " ++ dzenfont ++ dzenOpts
mydzenr   = "dzen2 -xs 2 -w 800 -ta l " ++ dzenfont ++ dzenOpts

dzenOpts  = "-p -e '' -h 30 -y -1 -fg '" ++ fg ++ "' -bg '" ++ bg ++ "'"
dzenfont = " -fn '-*-dejavu sans-*-*-*-*-*-160-*-*-*-*-*-*' "

-- }}}

-- Scratchpad {{{

scratchpads = [
    NS "floatterm" (roxterm ++ "floatterm") ( title =? "floatterm") rect
  , NS "cmus" (xterm ++ "cmus -e ~/bin/cmus.sh") ( title =? "cmus") tunes
  , NS "notepad" "exec gvim --servername notepad -f --role notepad ~/Documents/notepad" ( role =? "notepad") rect
  ]
  where
    tunes = customFloating $ W.RationalRect 0 0 1 0.5
    rect = customFloating $ W.RationalRect 0 0 1 0.4
    roxterm = "roxterm --separate --profile=scratchpad -T "
    xterm ="xterm -xrm \"xterm*allowTitleOps: false\" -T " 
    role = stringProperty "WM_WINDOW_ROLE"

scratchpadBinds = [
         ("M-c" , namedScratchpadAction scratchpads "cmus")
        , ("M-z" , namedScratchpadAction scratchpads "notepad")
        , ("`" , namedScratchpadAction scratchpads "floatterm")
        , ("M-[", withFocused (keysResizeWindow (0,60) (1,0)))
        , ("M-]", withFocused (keysResizeWindow (0,-60) (1,0)))
        ]

-- }}}

-- Misc Functions and Vars {{{

myrestart = "xmonad --restart"

-- cycle through WSs with windows, excluding scratchpad
cycleHiddenNonEmptyNS :: Direction1D -> X ()
cycleHiddenNonEmptyNS d = findWorkspace filtsort d HiddenNonEmptyWS 1 >>= windows . W.view
    where
        filtsort = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex

-- either go to a workspace, or toggle if it's already selected
goOrToggle :: WorkspaceId -> X ()
goOrToggle w = do
    cur <- gets (W.currentTag . windowset)
    if cur == w
        then toggleWS' [scratchpadws]
        else windows . W.view $ w

toggleOrViewNoSP = toggleOrDoSkip [scratchpadws] W.greedyView

-- spawn a program if it's window isn't running
singlespawn prog command = allWithProperty (ClassName prog) >>=
                \x -> when (null x) $ spawn command

-- if current WS is empty, spawn command, otherwise goto next empty
nextEmpty :: String -> X ()
nextEmpty command = do
    curws <- gets (W.peek . windowset)
    if isNothing curws
        then spawn command
        else moveTo Next EmptyWS >> spawn command

-- there's probably a builtin for this
repeatX :: X () -> Int -> X ()
repeatX f 0 = f
repeatX f n = f >> repeatX f (n-1)

-- }}}

-- keybinds {{{

-- keylist {{{

--messy way to see if a key is available to use for something
isbound x = if ("M-" ++ [x]) `elem` availablebinds then "Unbound" else "Bound"

availablebinds = filter (`notElem` currentbinds) allbinds
currentbinds  = nub (map fst mykeysdualhead) ++ defaultbinds
allbinds = (++) <$> mods <*> keyslist

keyslist = map (:[]) $ ['1'..'9'] ++ ['a'..'z']
mods     = ["M-", "M-S-", "M-C-"]

-- Others {{{

defaultbinds = ["M-S-<Return>", "M-<Space>", "M-S-<Space>", "M-p", "M-S-p", "M-S-c", "M-n", "Mod-<Tab>", "M-S-<Tab>", "M-j", "M-k", "M-m", "M-<Return>", "M-S-j", "M-S-k", "M-h", "M-l", "M-t", "M-comma", "M-period", "M-S-q", "M-q", "M-w", "M-e", "M-r"] ++ map (("M-"++) . (:[])) ['1'..'9']

others = ["<Backspace>", "<Tab>", "<Return>", "<Pause>", "<Scroll_lock>", "<Sys_Req>", "<Print>", "<Escape>, <Esc>", "<Delete>", "<Home>", "<Left>, <L>", "<Up>, <U>", "<Right>, <R>", "<Down>, <D>", "<Page_Up>", "<Page_Down>", "<End>", "<Insert>", "<Break>", "<Space>", "<F1>-<F24>", "<KP_Space>", "<KP_Tab>", "<KP_Enter>", "<KP_F1>", "<KP_F2>", "<KP_F3>", "<KP_F4>", "<KP_Home>", "<KP_Left>", "<KP_Up>", "<KP_Right>", "<KP_Down>", "<KP_Prior>", "<KP_Page_Up>", "<KP_Next>", "<KP_Page_Down>", "<KP_End>", "<KP_Begin>", "<KP_Insert>", "<KP_Delete>", "<KP_Equal>", "<KP_Multiply>", "<KP_Add>", "<KP_Separator>", "<KP_Subtract>", "<KP_Decimal>", "<KP_Divide>", "<KP_0>-<KP_9>"]

-- }}}

-- }}}

-- Dual+ monitor {{{

mykeysdualhead = mykeysP ++ [
          ("M-i" , nextScreen)
        ]
        ++
        [ (otherModMasks ++ "M-" ++ key, action tag)
            | (tag, key) <- zip mywssl $ map (drop 1 . dropWhile (/='_')) mywssl
            , (otherModMasks, action) <- [  ("", screenswitch 0)
                                            , ("S-", windows . W.shift ) ]]
        ++
        [ (otherModMasks ++ "M-" ++ key, action tag)
            | (tag, key) <- zip mywssr $ map (drop 1 . dropWhile (/='_')) mywssr
            , (otherModMasks, action) <- [  ("", screenswitch 1)
                                            , ("S-", windows . W.shift ) ]]
        ++  -- Disable Greedy Switching
        [ (otherModMasks ++ "M-" ++ [key], action tag)
            | (tag, key) <- zip mywsso $ concatMap show [(length mywssl + length mywssr)+1..9]
            , (otherModMasks, action) <- [  ("", toggleOrViewNoSP )
                                            , ("S-", windows . W.shift ) ]]
        where
            screenswitch s ws = viewScreen s >> (windows . W.view $ ws )

-- }}}

-- Single monitor {{{

mykeysonehead = mykeysP ++
            [ (otherModMasks ++ "M-" ++ [key], action tag)
            | (tag, key) <- zip wss ['1'..'9']
            , (otherModMasks, action) <- [  ("", toggleOrViewNoSP )
                                            , ("S-", windows . W.shift ) ]]

-- }}}

-- standard keys {{{


--required for dzen clickables: M-n,M-m,M-S-p, and M-# keys above
mykeysP =
        [
            -- Volume
          ("M-v v" , spawn "amixer -q set Master 2%- unmute")
        , ("M-v f" , spawn "amixer -q set Master 2%+ unmute")
        , ("<XF86AudioLowerVolume>" , spawn "amixer -q set Master 2%- unmute")
        , ("<XF86AudioRaiseVolume>" , spawn "amixer -q set Master 2%+ unmute")
        , ("<XF86AudioMute>" , spawn "amixer set Master toggle")

            -- Tunes
        , ("<XF86AudioPlay>" , spawn "cmus-remote -u")
        , ("<XF86AudioStop>" , spawn "cmus-remote -s")
        , ("<XF86AudioPrev>" , spawn "cmus-remote -r")
        , ("<XF86AudioNext>" , spawn "cmus-remote -n")
        , ("M-o" , spawn "cmus-remote -u")
        , ("M-v b" , spawn "cmus-remote -n")
        , ("M-v z" , spawn "cmus-remote -r")

            -- Launch Apps
        , ("M-d" , spawn "gmrun")
        {-, ("M-d" , (inputPrompt defaultXPConfig "wat" ?+ spawn) )-}
        , ("M-t" , spawn "roxterm --profile=Default")
            --TODO: stop from launching more windows
        {-, ("M-g" , singlespawn "Firefox" "firefox")-}
        , ("M-g" , spawn "firefox-beta-bin" )
        , ("M-e" , spawn "xterm -e ranger" )
        , ("M-s" , spawn gvimcmd )

            -- Window Management
        , ("M-h" , windowGo L False)
        , ("M-j" , windowGo D False)
        , ("M-k" , windowGo U False)
        , ("M-l" , windowGo R False)
        , ("M-S-h" , windowSwap L False)
        , ("M-S-j" , windowSwap D False)
        , ("M-S-k" , windowSwap U False)
        , ("M-S-l" , windowSwap R False)

            --TODO: maybe ,. regular resize, C-,. finegrain?
        , ("M-," , sendMessage Shrink)
        , ("M-." , sendMessage Expand)
        , ("M-S-," , repeatX (sendMessage Shrink) 5)
        , ("M-S-." , repeatX (sendMessage Expand) 5)
        , ("M-m" , cycleHiddenNonEmptyNS Next)
        , ("M-n" , cycleHiddenNonEmptyNS Prev)
        , ("M-b" , toggleWS )
        , ("M-f" , withFocused $ windows . W.sink)
        , ("M1-<F4>" , kill)
        {-, ("M-p" , windows W.shiftMaster )-}

            -- lockscreen just picks a random wallpaper from a dual monitor
            -- folder and starts i3lock
        , ("M-<Pause>" , spawn "~/.lockscreen.sh && systemctl suspend")
        , ("M-<Delete>"   , spawn "~/.lockscreen.sh")
        , ("M-S-q" , spawn $ "pkill dzen2; sleep 1; " ++ myrestart )
        , ("M-S-<Delete>" , io exitSuccess)
        {-, ("<F8>" , spawn "~/bin/autoclick.sh" )-}
        ] ++
        scratchpadBinds
        where
          gvimcmd = "gvim --servername GVIM --remote-send '<Esc>:tabnew<CR>'; if [[ $? == 1 ]]; then gvim --servername GVIM; fi"

notKeysP = 
    [
    "M-p"
    ]

-- }}}

-- }}}

-- testing {{{



-- }}}



