{-
 -
 -  Copyright 2005-2006, Robert Dockins.
 -
 -}

module System.Console.Shell.ShellMonad (
, Sh
, runSh
, shellPut
, shellPutStr, shellPutStrLn
, shellPutInfo, shellPutInfoLn
, shellPutErr, shellPutErrLn
, getShellSt, putShellSt
, modifyShellSt
, shellSpecial
) where

import Control.Monad.Reader
import Control.Monad.State

import System.Console.Shell.Backend
import System.Console.Shell.Types

runSh :: st -> ShellMonadInfo -> Sh st () -> IO (CommandResult st)
runSh st info = (flip runReaderT) info . (flip execStateT) (st,Nothing) . unSh

-- | Prints a regular output string
shellPut :: BackendOutput -> Sh st ()
shellPut out = Sh (lift ask >>= \f -> liftIO (f out))

-- | Prints a regular output string with a line terminator
shellPutStr :: String -> Sh st ()
shellPutStr = shellPut . RegularOutput

-- | Prints an informational output string
shellPutInfo :: String -> Sh st ()
shellPutInfo = shellPut . InfoOutput

-- | Prints an error output string
shellPutErr :: String -> Sh st ()
shellPutErr = shellPut . ErrorOutput

-- | Prints regular output with a line terminator
shellPutStrLn :: String -> Sh st ()
shellPutStrLn = shellPutStr . (++"\n")

-- | Prints an informational output string with a line terminator
shellPutInfoLn :: String -> Sh st ()
shellPutInfoLn = shellPutInfo . (++"\n")

-- | Prints and error output string with a line terminator
shellPutErrLn :: String -> Sh st ()
shellPutErrLn = shellPutErr . (++"\n")

-- | Get the current shell state
getShellSt :: Sh st st
getShellSt = Sh (get >>= return . fst)

-- | Set the shell state
putShellSt :: st -> Sh st ()
putShellSt st = Sh (get >>= \ (_,spec) -> put (st,spec))

-- | Apply the given funtion to the shell state
modifyShellSt :: (st -> st) -> Sh st ()
modifyShellSt f = getShellSt >>= putShellSt . f

-- | Schedule a shell \"special\" action.  Only the last call to
--   this function will affect the shell's behavior! It modifies
--   a bit of state that is overwritten on each call.
shellSpecial :: ShellSpecial st -> Sh st ()
shellSpecial spec = Sh (get >>= \ (st,_) -> put (st,Just spec))
