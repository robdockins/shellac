{-
 -
 -  Copyright 2005-2007, Robert Dockins.
 -
 -}

module System.Console.Shell.ConsoleHandler
( withControlCHandler
) where

import qualified Control.Exception as Ex

#ifdef BUILD_WINDOWS

-- Windows build, use the GHC console
-- handler module
import qualified GHC.ConsoleHandler as CH

handleCtrlC :: IO () -> CH.Handler
handleCtrlC hdl = CH.Catch $ \ev ->
   case ev of
     CH.ControlC -> hdl
     _           -> return ()


withControlCHandler :: IO () -> IO a -> IO a
withControlCHandler hdl m =
  Ex.bracket
      (CH.installHandler (handleCtrlC hdl))
      (\oldh -> CH.installHandler oldh)
      (\_ -> m)

#else

-- not Windows, assume POSIX
import qualified System.Posix.Signals as PS

withControlCHandler :: IO () -> IO a -> IO a
withControlCHandler hdl m =
  Ex.bracket
      (PS.installHandler PS.keyboardSignal (PS.Catch hdl) Nothing)
      (\oldh -> PS.installHandler PS.keyboardSignal oldh Nothing)
      (\_ -> m)

#endif
