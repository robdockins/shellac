module System.Console.Shell where

import Maybe (isJust)
import Data.Char (toLower, isDigit, isSpace)
import Data.List ( (\\), isPrefixOf, find )
import Control.Monad (when, MonadPlus(..) )
import Control.Monad.Error ()
import Control.Monad.Trans
import Control.Exception (bracket)
import Control.Concurrent (ThreadId, threadDelay, killThread, forkIO)
import Control.Concurrent.MVar
import System.Posix.Signals (Handler (..), installHandler, keyboardSignal)
import Numeric (readDec, readHex, readFloat)
import qualified System.Console.Readline as RL

import System.Console.Regex

data CommandStyle
   = OnlyCommands
   | ColonCommands

type CommandResult st = Either ShellSpecial st
type EvaluationFunction st = String -> st -> IO st

data ShellSpecial
  = ShellExit
  | ShellHelp (Maybe String)

data CommandParseResult st
  = CompleteParse (st -> IO (CommandResult st))
  | IncompleteParse (Maybe (st -> String -> IO [String]))

type CommandParser st = String -> [CommandParseResult st]
type ShellCommand st = ShellDescription st -> (String,CommandParser st,String,String)

data ShellDescription st
   = ShDesc
   { shellCommands      :: [ShellCommand st]
   , commandStyle       :: CommandStyle
   , evaluateFunc       :: EvaluationFunction st
   , wordBreakChars     :: [Char]
   , prompt             :: String
   , defaultCompletions :: Maybe (st -> String -> IO [String])
   }

initialShellDescription :: IO (ShellDescription st)
initialShellDescription =
  do let wbc = " \t\n\r\v`~!@#$%^&*()=[]{};\\\'\",<>"
     return ShDesc
       { shellCommands      = []
       , commandStyle       = ColonCommands
       , evaluateFunc       = \_ st -> return st
       , wordBreakChars     = wbc
       , prompt             = "> "
       , defaultCompletions = Just (\_ _ -> return [])
       }

mkShellDescription :: [ShellCommand st] -> EvaluationFunction st -> IO (ShellDescription st)
mkShellDescription cmds func =
   do desc <- initialShellDescription
      return desc
             { shellCommands = cmds
             , evaluateFunc  = func
             }

data InternalShellState st
   = InternalShellState
     { evalMVar         :: MVar (Maybe st)
     , evalThreadMVar   :: MVar ThreadId
     , cancelHandler    :: Handler
     }

runShell :: ShellDescription st -> st -> IO st
runShell desc init = bracket setupShell exitShell (\iss -> shellLoop desc iss init)

  where setupShell  =
         do evalM <- newEmptyMVar
            thM   <- newEmptyMVar
            return InternalShellState
                   { evalMVar = evalM
                   , evalThreadMVar = thM
                   , cancelHandler = Catch (handleINT evalM thM)
                   }

        exitShell iss = return ()

        handleINT evalM thM  =
           do tid <- tryTakeMVar thM
              case tid of
                 Nothing   -> error "could not take thread id mvar! bad race condition!"
                 Just tid' -> do killThread tid'
                                 tryPutMVar evalM Nothing
                                 return ()

completionFunction :: ShellDescription st -> st -> String -> Int -> Int -> IO (Maybe (String,[String]))
completionFunction desc st word begin end = do
   buffer <- RL.getLineBuffer
   let before = take begin buffer
   let after  = drop end buffer

   if all isSpace before
     then completeCommands desc before after word
     else
       case runRegex (commandsRegex desc) before of
             [((_,cmdParser,_,_),before')] ->
                let parses  = cmdParser before'
                    parses' = concatMap (\x -> case x of IncompleteParse (Just z) -> [z]; _ -> []) parses
                in case parses' of
                   compl:_ -> do
                       strings <- compl st word
                       case strings of
                          [] -> return Nothing
                          xs -> return $ Just (maximalPrefix xs,xs)
                   _ -> return Nothing
             _ -> return Nothing


completeCommands :: ShellDescription st -> String -> String -> String -> IO (Maybe (String,[String]))
completeCommands desc before after word =
    case matchingNames of
       [] -> return $ Nothing
       xs -> return $ Just (maximalPrefix xs,xs)

  where matchingNames = filter (word `isPrefixOf`) cmdNames
        cmdNames      = map (\ (n,_,_,_) -> (maybeColon desc)++n) (getShellCommands desc)

maybeColon :: ShellDescription st -> String
maybeColon desc = case commandStyle desc of ColonCommands -> ":"; OnlyCommands -> ""

getShellCommands desc = map ($ desc) (shellCommands desc)

maximalPrefix :: [String] -> String
maximalPrefix [] = []
maximalPrefix (x:xs) = f x xs
  where f p [] = p
        f p (x:xs) = f (fst $ unzip $ takeWhile (\x -> fst x == snd x) $ zip p x) xs

shellLoop :: ShellDescription st -> InternalShellState st -> st -> IO st
shellLoop desc iss init = loop init
 where
   loop st =
     do RL.setAttemptedCompletionFunction (Just (completionFunction desc st))
        case defaultCompletions desc of
           Nothing -> RL.setCompletionEntryFunction $ Nothing
           Just f  -> RL.setCompletionEntryFunction $ Just (f st)
        RL.setBasicWordBreakCharacters (wordBreakChars desc)
        inp <- RL.readline (prompt desc)
        case inp of
           Nothing   -> return st
           Just inp' -> handleInput inp' st

   handleInput inp st = do
     when (isJust (find (not . isSpace) inp)) (RL.addHistory inp)
     let inp' = inp++" " -- hack, makes commands unambiguous
     case runRegex (commandsRegex desc) inp' of
       (x,inp''):_ -> executeCommand x inp'' st
       []          -> evaluateInput inp st

   executeCommand (cmdName,cmdParser,_,_) inp st =
      let parses  = cmdParser inp
          parses' = concatMap (\x -> case x of CompleteParse z -> [z]; _ -> []) parses
      in case parses' of
          f:_ -> do
              r <- f st
              case r of
                  Left spec -> handleSpecial st spec
                  Right st' -> loop st'
          _   -> putStrLn (showCmdHelp desc cmdName) >> loop st

   handleSpecial st ShellExit              = return st
   handleSpecial st (ShellHelp Nothing)    = putStrLn (showShellHelp desc)   >> loop st
   handleSpecial st (ShellHelp (Just cmd)) = putStrLn (showCmdHelp desc cmd) >> loop st

   evaluateInput inp st =
     let m = evalMVar iss
         t = evalThreadMVar iss
         h = cancelHandler iss
         e = evaluateFunc desc
     in do tid <- forkIO (e inp st >>= putMVar m . Just)
           putMVar t tid
           result <- bracket
              (installHandler keyboardSignal h Nothing)
              (\oldh -> installHandler keyboardSignal oldh Nothing)
              (\_ -> do
                  result <- takeMVar m
                  tryTakeMVar t
                  return result)

           case result of
             Nothing  -> putStrLn "cancled..." >> loop st
             Just st' -> loop st'

showShellHelp :: ShellDescription st -> String
showShellHelp desc = concat [ concat [syn,"\t\t",msg,"\n"] | (_,_,syn,msg) <- getShellCommands desc ]

showCmdHelp :: ShellDescription st -> String -> String
showCmdHelp desc cmd =
  case cmds of
     [(n,_,syn,msg)] -> concat [syn,"\n",msg]
     _               -> concat ["bad command name: '",cmd,"'"]

 where cmds = filter (\ (n,_,_,_) -> n == cmd) (getShellCommands desc)

exitCommand :: String -> ShellCommand st
exitCommand name desc = ( name
                        , \_ -> [CompleteParse (\_ -> return (Left ShellExit))]
                        , concat [maybeColon desc,name]
                        , "Exit the shell"
                        )

helpCommand :: String -> ShellCommand st
helpCommand name desc = ( name
                        , \_ -> [CompleteParse (\_ -> return (Left (ShellHelp Nothing)))]
                        , concat [maybeColon desc,name]
                        , "Display the shell command help"
                        )

newtype FullCommand st   = FullCommand (st -> IO (CommandResult st))
newtype StateCommand st  = StateCommand (st -> IO st)
newtype SimpleCommand st = SimpleCommand (IO ())
newtype File = File String
newtype Username = Username String
newtype Completable compl = Completable String

class Completion compl st | compl -> st where
  complete :: compl -> (st -> String -> IO [String])
  completableLabel :: compl -> String

cmd :: CommandFunction f st => String -> f -> String -> ShellCommand st
cmd name f helpMsg desc =
      (name
      ,parseCommand (wordBreakChars desc) f
      ,concat [maybeColon desc,name,commandSyntax f]
      ,helpMsg
      )

class CommandFunction f st | f -> st where
  parseCommand  :: String -> f -> CommandParser st
  commandSyntax :: f -> String

instance CommandFunction (FullCommand st) st where
  parseCommand wbc (FullCommand f) str = 
         do (x,[]) <- runRegex (maybeSpaceBefore (Epsilon (CompleteParse f))) str
            return x

  commandSyntax _ = ""

instance CommandFunction (StateCommand st) st where
  parseCommand wbc (StateCommand f) str = 
         do (x,[]) <- runRegex (maybeSpaceAfter (Epsilon (CompleteParse (\st -> f st >>= return . Right)))) str
            return x

  commandSyntax _ = ""

instance CommandFunction (SimpleCommand st) st where
  parseCommand wbc (SimpleCommand f) str = 
         do (x,[]) <- runRegex (maybeSpaceAfter (Epsilon (CompleteParse (\st -> f >> return (Right st))))) str
            return x

  commandSyntax _ = ""

instance CommandFunction r st
      => CommandFunction (Int -> r) st where
  parseCommand = doParseCommand Nothing intRegex id
  commandSyntax f = concat [" ",show intRegex,commandSyntax (f undefined)]

instance CommandFunction r st
      => CommandFunction (Integer -> r) st where
  parseCommand = doParseCommand Nothing intRegex id
  commandSyntax f = concat [" ",show intRegex,commandSyntax (f undefined)]

instance CommandFunction r st
      => CommandFunction (Float -> r) st where
  parseCommand = doParseCommand Nothing floatRegex id
  commandSyntax f = concat [" ",show floatRegex,commandSyntax (f undefined)]

instance CommandFunction r st
      => CommandFunction (Double -> r) st where
  parseCommand = doParseCommand Nothing floatRegex id
  commandSyntax f = concat [" ",show floatRegex,commandSyntax (f undefined)]

instance CommandFunction r st
      => CommandFunction (String -> r) st where
  parseCommand wbc = doParseCommand Nothing (wordRegex wbc) id wbc
  commandSyntax f = concat [" ",show (wordRegex ""),commandSyntax (f undefined)]

instance CommandFunction r st
      => CommandFunction (File -> r) st where
  parseCommand wbc = doParseCommand
                        (Just (\st -> RL.filenameCompletionFunction)) (wordRegex wbc) File wbc
  commandSyntax f = concat [" <file>",commandSyntax (f undefined)]

instance CommandFunction r st
      => CommandFunction (Username -> r) st where
  parseCommand wbc = doParseCommand
                        (Just (\st -> RL.usernameCompletionFunction))
                        (wordRegex wbc)
                        Username
                        wbc
  commandSyntax f = concat [" <username>",commandSyntax (f undefined)]

instance (CommandFunction r st,Completion compl st)
      => CommandFunction (Completable compl -> r) st where
  parseCommand wbc = doParseCommand
                        (Just (complete (undefined::compl)))
                        (wordRegex wbc)
                        Completable
                        wbc
  commandSyntax f = concat [" ",completableLabel (undefined::compl),commandSyntax (f undefined)]

doParseCommand compl re proj wbc f []  = return (IncompleteParse compl)
doParseCommand compl re proj wbc f str =
  let xs = runRegex (maybeSpaceBefore (maybeSpaceAfter re)) str
  in case xs of
        [] -> return (IncompleteParse compl)
        _  -> do (x,str') <- xs; parseCommand wbc (f (proj x)) str'

commandsRegex :: ShellDescription st -> Regex Char (String,CommandParser st,String,String)
commandsRegex desc =
   case commandStyle desc of
      ColonCommands -> colonCommandsRegex (getShellCommands desc)
      OnlyCommands  -> onlyCommandsRegex  (getShellCommands desc)

onlyCommandsRegex :: [(String,CommandParser st,String,String)] -> Regex Char (String,CommandParser st,String,String)
onlyCommandsRegex xs =
    Concat (\_ x -> x) maybeSpaceRegex $
    Concat (\x _ -> x) (anyOfRegex (map (\ (x,y,z,w) -> (x,(x,y,z,w))) xs)) $
                       spaceRegex

colonCommandsRegex :: [(String,CommandParser st,String,String)] -> Regex Char (String,CommandParser st,String,String)
colonCommandsRegex xs =
    Concat (\_ x -> x) maybeSpaceRegex $
    Concat (\_ x -> x) (strTerminal ':') $
    Concat (\x _ -> x) (anyOfRegex (map (\ (x,y,z,w) -> (x,(x,y,z,w))) xs)) $
                       spaceRegex
