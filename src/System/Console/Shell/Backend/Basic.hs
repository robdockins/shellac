{-
 - 
 -  Copyright 2005, Robert Dockins.
 -  
 -}

module System.Console.Shell.Backend.Basic
( basicBackend
) where

import System.IO
import qualified Control.Exception as Ex

import System.Console.Shell.Backend

basicBackend :: ShellBackend () ()
basicBackend = ShBackend
  { initBackend                      = return ()
  , flushOutput                      = \_ -> hFlush stdout
  , getSingleChar                    = \_ -> basicGetSingleChar
  , getInput                         = \_ -> basicGetInput
  , addHistory                       = \_ _ -> return ()
  , setWordBreakChars                = \_ _ -> return ()
  , getWordBreakChars                = \_ -> return " \t\n\r\v`~!@#$%^&*()=[]{};\\\'\",<>"
  , setAttemptedCompletionFunction   = \_ _ -> return ()
  , setDefaultCompletionFunction     = \_ _ -> return ()
  , completeFilename                 = \_ _ -> return []
  , completeUsername                 = \_ _ -> return []
  , clearHistoryState                = \_ -> return ()
  , getMaxHistoryEntries             = \_ -> return 0
  , setMaxHistoryEntries             = \_ _ -> return ()
  , getHistoryState                  = \_ -> return ()
  , setHistoryState                  = \_ _ -> return ()
  , readHistory                      = \_ _ -> return ()
  , writeHistory                     = \_ _ -> return ()
  , freeHistoryState                 = \_ _ -> return ()
  }

basicGetSingleChar :: String -> IO (Maybe Char)
basicGetSingleChar prompt = do
   hPutStr stdout prompt
   hFlush stdout
   Ex.bracket (hGetBuffering stdin) (hSetBuffering stdin) $ \_ -> do
      hSetBuffering stdin NoBuffering
      c <- hGetChar stdin
      hPutStrLn stdout ""
      return (Just c)

basicGetInput :: String -> IO (Maybe String)
basicGetInput prompt = do
   hPutStr stdout prompt
   hFlush stdout
   x <- hGetLine stdin
   return (Just x)
