module System.Console.Shell.ConsoleHandler
( withControlCHandler
) where

import qualified Control.Exception as Ex

#ifndef mingw32_HOST_OS

import qualified System.Posix.Signals as PS

withControlCHandler :: IO () -> IO a -> IO a
withControlCHandler hdl m =
  Ex.bracket
      (PS.installHandler PS.keyboardSignal (PS.Catch hdl) Nothing)
      (\oldh -> PS.installHandler PS.keyboardSignal oldh Nothing)
      (\_ -> m)

#else

import qualified GHC.ConsoleHandler as CH


handleCtrlC :: IO () -> CH.Handler
handleCtrlC m = CH.Catch $ \ev ->
   case ev of
     CH.ControlC -> m
      _          -> return ()


withControlCHandler :: IO () -> IO a -> IO a
withControlCHandler hdl m =
  Ex.bracket
      (CH.installHandler (handleCtrlC hdl))
      (\oldh -> CH.installlHandler oldh)
      (\_ -> m)


#endif
