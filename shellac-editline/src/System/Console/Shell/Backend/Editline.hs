{-
 - 
 -  Copyright 2005-2008, Robert Dockins.
 -  
 -}

{- | This module implements a Shellac backend based on the editline library.
     
     Both the code for this binding and the editline library itself are
     licenced under BSD-style licences.
-}


module System.Console.Shell.Backend.Editline
( editlineBackend
) where

import System.IO            ( stdin, stdout, stderr, hFlush, hPutStr, hPutStrLn, hGetChar
                            , hSetBuffering, hGetBuffering
                            , BufferMode(..)
                            )
import System.IO.Error      ( mkIOError, userErrorType )

import qualified Control.Exception as Ex
import System.Console.Editline
import qualified System.Console.Editline.Readline as EL

import System.Console.Shell.Backend 

editlineBackend :: ShellBackend ()
editlineBackend = ShBackend
  { initBackend                      = return ()
  , shutdownBackend                  = \_ -> return ()
  , outputString                     = \_ -> editlineOutput
  , flushOutput                      = \_ -> hFlush stdout
  , getInput                         = \_ -> EL.readline
  , getSingleChar                    = \_ -> editlineGetSingleChar
  , addHistory                       = \_ -> EL.addHistory
  , getWordBreakChars                = \_ -> EL.getBasicWordBreakCharacters
  , setWordBreakChars                = \_ -> EL.setBasicWordBreakCharacters
  , onCancel                         = \_ -> hPutStrLn stdout "canceled..."
  , setAttemptedCompletionFunction   = \_ -> editlineCompletionFunction
  , setDefaultCompletionFunction     = \_ -> EL.setCompletionEntryFunction
  , completeFilename                 = \_ -> EL.filenameCompletionFunction
  , completeUsername                 = \_ -> EL.usernameCompletionFunction
  , clearHistoryState                = \_ -> EL.clearHistory
  , setMaxHistoryEntries             = \_ -> EL.stifleHistory
  , getMaxHistoryEntries             = \_ -> EL.historyMaxEntries
  , readHistory                      = \_ -> editlineReadHistory
  , writeHistory                     = \_ -> editlineWriteHistory
  }


editlineCompletionFunction :: CompletionFunction -> IO ()
editlineCompletionFunction f = EL.setAttemptedCompletionFunction (Just complete)

 where complete word begin end = do
          buffer <- EL.getLineBuffer
          let before = take begin buffer
          let after  = drop end buffer

          f (before,word,after)


editlineGetSingleChar :: String -> IO (Maybe Char)
editlineGetSingleChar prompt = do
   hPutStr stdout prompt
   hFlush stdout
   Ex.bracket (hGetBuffering stdin) (hSetBuffering stdin) $ \_ -> do
      hSetBuffering stdin NoBuffering
      c <- hGetChar stdin
      hPutStrLn stdout ""
      return (Just c)

editlineOutput :: BackendOutput -> IO ()
editlineOutput (RegularOutput str)  = hPutStr stdout str
editlineOutput (InfoOutput str)     = hPutStr stdout str
editlineOutput (ErrorOutput str)    = hPutStr stderr str


editlineReadHistory :: FilePath -> IO ()
editlineReadHistory p = do
   x <- EL.readHistory p
   if x then return ()
    else ioError $ mkIOError
            userErrorType
            "System.Console.Shell.Backend.Editline.editlineReadHistory"
	    Nothing
            (Just p)

editlineWriteHistory :: FilePath -> IO ()
editlineWriteHistory p = do
   x <- EL.writeHistory p
   if x then return ()
    else ioError $ mkIOError
            userErrorType
            "System.Console.Shell.Backend.Editline.editlineWriteHistory"
	    Nothing
            (Just p)
