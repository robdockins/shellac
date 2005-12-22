{-
 - 
 -  Copyright 2005, Robert Dockins.
 -  
 -}

module System.Console.Shell.Backend.Basic
( basicBackend
) where

import System.IO

import System.Console.Shell.Backend

basicBackend :: ShellBackend () ()
basicBackend = ShBackend
  { initBackend                      = return ()
  , flushOutput                      = \_ -> hFlush stdout
  , getInput                         = \_ -> basicGetInput
  , addHistory                       = \_ _ -> return ()
  , setWordBreakChars                = \_ _ -> return ()
  , getWordBreakChars                = \_ -> return " \t\n\r\v`~!@#$%^&*()=[]{};\\\'\",<>"
  , setAttemptedCompletionFunction   = \_ _ -> return ()
  , setDefaultCompletionFunction     = \_ _ -> return ()
  , completeFilename                 = \_ _ -> return []
  , completeUsername                 = \_ _ -> return []
  , getHistoryState                  = \_ -> return ()
  , setHistoryState                  = \_ _ -> return ()
  , readHistoryState                 = \_ _ -> return ()
  , writeHistoryState                = \_ _ _ -> return ()
  }

basicGetInput :: String -> IO (Maybe String)
basicGetInput prompt = do
   hPutStr stdout prompt
   hFlush stdout
   x <- hGetLine stdin
   return (Just x)
