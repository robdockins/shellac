{-
 - 
 -  Copyright 2005, Robert Dockins.
 -  
 -}

{- | This module implements a framework for creating read-eval-print style
     command shells.  Shells are created by declarativly defining evaluation
     functions and \"shell commands\".  Input is read using the standard Haskell
     readline bindings, and the shell framework handles history and word completion
     features.

     The basic idea is:
       1) Create a list of shell commands and an evaluation function
       2) Create a shell description
       3) Set up the initial shell state
       4) Run the shell
-}

module System.Console.Shell (

-- * Shell Descriptions
  ShellDescription (..)
, initialShellDescription
, mkShellDescription

-- * Executing Shells
, runShell

-- * Creating Shell Commands
-- ** High-level Interface
, exitCommand
, helpCommand
, cmd
, CommandFunction (..)
, FullCommand (..)
, StateCommand (..)
, SimpleCommand (..)
, File (..)
, Username (..)
, Completable (..)
, Completion (..)

-- ** Low-Level Interface
, ShellCommand

-- * Subshells
, Subshell
, simpleSubshell
, runSubshell

-- * Printing Help Messages
, showShellHelp
, showCmdHelp

-- * Type Synonyms and Auxiliary Types
, CommandStyle (..)
, CommandResult
, EvaluationFunction
, ShellSpecial (..)
, CommandParseResult (..)
, CommandParser
) where

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


-- | Datatype describing the style of shell commands.  This 
--   determines how shell input is parsed.
data CommandStyle
   = OnlyCommands   -- ^ Indicates that all input is to be interpreted as shell commands; no
                    --   input will be passed to the evaluation function.
   | ColonCommands  -- ^ Indicates that commands are prefaced with a colon ':' character.

-- | The type of results from shell commands.  They are either
--   a \"special\" action for the shell framework to execute, or
--   a modified shell state.
type CommandResult st = Either ShellSpecial st

-- | The type of an evaluation function for a shell.  The function
--   takes the input string and the current shell state, and returns
--   a possibly modified shell state.
type EvaluationFunction st = String -> st -> IO st

-- | Special commands for the shell framework.
data ShellSpecial
  = ShellExit                  -- ^ Causes the shell to exit
  | ShellHelp (Maybe String)   -- ^ Causes the shell to print an informative message.
                               --   If a command name is specified, only information about
                               --   that command will be displayed.

-- | The result of parsing a command.
data CommandParseResult st
  = CompleteParse (st -> IO (CommandResult st)) 
          -- ^ A complete parse.  A command function is returned.
  | IncompleteParse (Maybe (st -> String -> IO [String]))
          -- ^ An incomplete parse.  A word completion function may be returned.

-- | The type of a command parser.
type CommandParser st = String -> [CommandParseResult st]

-- | The type of a shell command.  The shell description is passed in, and the
--   tuple consists of
--     (command name,command parser,command syntax,help message)
type ShellCommand st = ShellDescription st -> (String,CommandParser st,String,String)

-- | The type of subshells.  The tuple consists of
--    1) A function to generate the initial subshell state from the outer shell state
--    2) A function to generate the outer shell state from the final subshell state
--    3) A function to generate the shell description from the inital subshell state
type Subshell st st' = (st -> IO st', st' -> IO st, st' -> IO (ShellDescription st') )

-- | A record type which describes the attributes of a shell.
data ShellDescription st
   = ShDesc
   { shellCommands      :: [ShellCommand st]      -- ^ Commands for this shell
   , commandStyle       :: CommandStyle           -- ^ The style of shell commands
   , evaluateFunc       :: EvaluationFunction st  -- ^ The evaluation function for this shell
   , wordBreakChars     :: [Char]                 -- ^ The characters upon which readline will break words
   , beforePrompt       :: st -> IO ()            -- ^ an IO action to run before each prompt is printed
   , prompt             :: String                 -- ^ The prompt to print
   , defaultCompletions :: Maybe (st -> String -> IO [String])
                                                  -- ^ If set, this function provides completions when NOT
                                                  --   in the context of a shell command
   }

-- | A basic shell description with sane initial values
initialShellDescription :: IO (ShellDescription st)
initialShellDescription =
  do let wbc = " \t\n\r\v`~!@#$%^&*()=[]{};\\\'\",<>"
     return ShDesc
       { shellCommands      = []
       , commandStyle       = ColonCommands
       , evaluateFunc       = \_ st -> return st
       , wordBreakChars     = wbc
       , beforePrompt       = \_ -> putStrLn ""
       , prompt             = "> "
       , defaultCompletions = Just (\_ _ -> return [])
       }

-- | Creates a simple shell description from a list of shell commmands and
--   an evalation function.
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

-- | Run a shell.  Given a shell description and an initial state
--   this function runs the shell until it exits, and then returns
--   the final state.
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
     do beforePrompt desc st
        RL.setAttemptedCompletionFunction (Just (completionFunction desc st))
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

-- | Prints the help message for this shell, which lists all avaliable
--   commands with their syntax and a short informative message about each.
showShellHelp :: ShellDescription st -> String
showShellHelp desc = concat [ concat [syn,"\t\t",msg,"\n"] | (_,_,syn,msg) <- getShellCommands desc ]

-- | Print the help message for a particular shell command
showCmdHelp :: ShellDescription st -> String -> String
showCmdHelp desc cmd =
  case cmds of
     [(n,_,syn,msg)] -> concat [syn,"\n",msg]
     _               -> concat ["bad command name: '",cmd,"'"]

 where cmds = filter (\ (n,_,_,_) -> n == cmd) (getShellCommands desc)

-- | Creates a shell command which will exit the shell.
exitCommand :: String            -- ^ the name of the command
            -> ShellCommand st
exitCommand name desc = ( name
                        , \_ -> [CompleteParse (\_ -> return (Left ShellExit))]
                        , concat [maybeColon desc,name]
                        , "Exit the shell"
                        )

-- | Creates a command which will print the shell help message.
helpCommand :: String           -- ^ the name of the command
            -> ShellCommand st
helpCommand name desc = ( name
                        , \_ -> [CompleteParse (\_ -> return (Left (ShellHelp Nothing)))]
                        , concat [maybeColon desc,name]
                        , "Display the shell command help"
                        )

-- | Creates a simple subshell from a state mapping function 
--   and a shell description.
simpleSubshell :: (st -> IO st')       -- ^ A function to generate the initial subshell state from the outer shell state
               -> ShellDescription st' -- ^ A shell description for the subshell
               -> IO (Subshell st st')
simpleSubshell toSubSt desc = do
  ref <- newEmptyMVar
  let toSubSt' st     = putMVar ref st >> toSubSt st
  let fromSubSt subSt = takeMVar ref
  let mkDesc _        = return desc
  return (toSubSt',fromSubSt,mkDesc)

-- | Execute a subshell.
runSubshell :: Subshell st st' -- ^ the subshell to execute
            -> st              -- ^ the current state
            -> IO st           -- ^ the modified state
runSubshell (toSubSt, fromSubSt, mkSubDesc) st = do
  subSt   <- toSubSt st
  subDesc <- mkSubDesc subSt
  subSt'  <- runShell subDesc subSt
  st'     <- fromSubSt subSt'
  return st'

-- | A shell command which can return shell special commands as well as 
--   modifying the shell state
newtype FullCommand st   = FullCommand (st -> IO (CommandResult st))

-- | A shell command which can modify the shell state.
newtype StateCommand st  = StateCommand (st -> IO st)

-- | A shell command which does not alter the shell state.
newtype SimpleCommand st = SimpleCommand (IO ())

-- | Represents a command argument which is a filename
newtype File = File String

-- | Represents a command argument which is a username
newtype Username = Username String

-- | Represents a command argument which is an arbitrary
--   completable item.  The type argument determines the
--   instance of 'Completion' which is used to create
--   completions for this command argument.
newtype Completable compl = Completable String

-- | A typeclass representing user definable completion functions.
class Completion compl st | compl -> st where
  -- | Actually generates the list of possible completions, given the
  --   current shell state and a string representing the beginning of the word.
  complete :: compl -> (st -> String -> IO [String])

  -- | generates a label for the argument for use in the help displays.
  completableLabel :: compl -> String

-- | Creates a user defined shell commmand.
cmd :: CommandFunction f st 
    => String           -- ^ the name of the command
    -> f                -- ^ the command function.  See 'CommandFunction' for restrictions
                        --   on the type of this function.
    -> String           -- ^ the help string for this command
    -> ShellCommand st

cmd name f helpMsg desc =
      (name
      ,parseCommand (wordBreakChars desc) f
      ,concat [maybeColon desc,name,commandSyntax f]
      ,helpMsg
      )


-- | This class is used in the 'cmd' function to automaticly generate
--   the command parsers and command syntax strings for user defined
--   commands.  The type of 'f' is restricted to have a restricted set of
--   monomorphic arguments ('Bool', 'Int', 'Integer', 'Float', 'Double', 'String', 
--   'File', 'Username', and 'Completable') and the head type must be one of the three
--   types 'FullCommand', 'StateCommand', or 'SimpleCommand'.  For example:
--
-- @
--   f :: Int -> File -> FullCommand MyShellState 
--   g :: Double -> StateCommand MyOtherShellState
--   h :: SimpleCommand SomeShellState
-- @
--
--   are all legal types, whereas:
--
-- @
--   bad1 :: a -> FullCommand (MyShellState a)
--   bad2 :: [Int] -> SimpleCommand MyShellState
--   bad3 :: Bool -> MyShellState
-- @
--
--   are not.

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
