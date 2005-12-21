{-
 - 
 -  Copyright 2005, Robert Dockins.
 -  
 -}

{- | This module implements a framework for creating read-eval-print style
     command shells.  Shells are created by declaratively defining evaluation
     functions and \"shell commands\".  Input is read using the standard Haskell
     readline bindings, and the shell framework handles command history and word completion
     features.
 
     The basic idea is:
 
      (1) Create a list of shell commands and an evaluation function

      (2) Create a shell description

      (3) Set up the initial shell state

      (4) Run the shell
-}


module System.Console.Shell (

-- * Shell Descriptions
  ShellDescription (..)
, initialShellDescription
, mkShellDescription
, defaultExceptionHandler

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
, CommandParser
, CommandParseResult (..)
, CommandResult

-- * Subshells
, Subshell
, simpleSubshell
, runSubshell

-- * Printing Help Messages
, showShellHelp
, showCmdHelp

-- * Type Synonyms and Auxiliary Types
, CommandStyle (..)
, ShellSpecial (..)
, EvaluationFunction
) where

import Maybe (isJust)
import Data.Char (toLower, isDigit, isSpace)
import Data.List ( (\\), isPrefixOf, find )
import Control.Monad (when, MonadPlus(..) )
import Control.Monad.Error ()
import Control.Monad.Trans
import qualified Control.Exception as Ex
import Control.Concurrent (ThreadId, threadDelay, killThread, forkIO)
import Control.Concurrent.MVar
import System.IO (stdout, hFlush)
import System.Posix.Signals (Handler (..), installHandler, keyboardSignal)
import Numeric (readDec, readHex, readFloat)
import qualified System.Console.Readline as RL
import PPrint

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
type EvaluationFunction st = String -> st -> IO (Either ShellSpecial st)

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
--     (command name,command parser,command syntax document,help message document)
type ShellCommand st = ShellDescription st -> (String,CommandParser st,Doc,Doc)


-- | The type of subshells.  The tuple consists of:
--
--    (1) A function to generate the initial subshell state from the outer shell state
--
--    (2) A function to generate the outer shell state from the final subshell state
--
--    (3) A function to generate the shell description from the inital subshell state

type Subshell st st' = (st -> IO st', st' -> IO st, st' -> IO (ShellDescription st') )



------------------------------------------------------------------------
-- The shell description and utility functions


-- | A record type which describes the attributes of a shell.
data ShellDescription st
   = ShDesc
   { shellCommands      :: [ShellCommand st]        -- ^ Commands for this shell
   , commandStyle       :: CommandStyle             -- ^ The style of shell commands
   , evaluateFunc       :: EvaluationFunction st    -- ^ The evaluation function for this shell
   , wordBreakChars     :: [Char]                   -- ^ The characters upon which readline will break words
   , beforePrompt       :: st -> IO ()              -- ^ an IO action to run before each prompt is printed
   , prompt             :: String                   -- ^ The prompt to print
   , exceptionHandler   :: Ex.Exception -> st -> IO st -- ^ A function called when an exception occurs
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
       , evaluateFunc       = \_ st -> return (Right st)
       , wordBreakChars     = wbc
       , beforePrompt       = \_ -> putStrLn ""
       , prompt             = "> "
       , exceptionHandler   = defaultExceptionHandler
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


-------------------------------------------------------------------
-- A record to hold some of the internal muckety-muck needed
-- to make the shell go


data InternalShellState st
   = InternalShellState
     { evalMVar         :: MVar (Maybe (Either ShellSpecial st))
     , evalThreadMVar   :: MVar ThreadId
     , cancelHandler    :: Handler
     }


-------------------------------------------------------------------
-- Main entry point for the shell.  Sets up the crap needed to
-- run shell commands and evaluation in a separate thread.


-- | Run a shell.  Given a shell description and an initial state
--   this function runs the shell until it exits, and then returns
--   the final state.
runShell :: ShellDescription st -> st -> IO st
runShell desc init = Ex.bracket setupShell exitShell (\iss -> shellLoop desc iss init)

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

-------------------------------------------------------------------------
-- This function is installed as the readline completion function
-- It attempts to match the prefix of the input buffer against a
-- command.  If it matches, it supplies the completions appropriate
-- for that point in the command.  Otherwise it returns Nothing; in
-- that case, readline will fall back on the default completion function
-- set in the shell description.

completionFunction :: ShellDescription st    
                   -> st 
                   -> String
                   -> Int 
                   -> Int 
                   -> IO (Maybe (String,[String]))

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


-----------------------------------------------------------
-- The real meat.  We setup readline stuff, call readline
-- and then handle the input string.


shellLoop :: ShellDescription st -> InternalShellState st -> st -> IO st
shellLoop desc iss init = loop init
 where
   loop st =
     do hFlush stdout
        beforePrompt desc st
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
              r <- handleExceptions desc f (return . Right) st
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
     in do tid <- forkIO (handleExceptions desc (e inp) (return . Right) st >>= putMVar m . Just)
           putMVar t tid
           result <- Ex.bracket
              (installHandler keyboardSignal h Nothing)
              (\oldh -> installHandler keyboardSignal oldh Nothing)
              (\_ -> do
                  result <- takeMVar m
                  tryTakeMVar t
                  return result)

           case result of
             Nothing          -> putStrLn "canceled..." >> loop st
             Just (Left spec) -> handleSpecial st spec
             Just (Right st') -> loop st'


------------------------------------------------------------------------
-- Keeps exceptions from bubbling out to the main shell loop and killing it.
-- We invoke the exception handler from the shell description.

handleExceptions :: ShellDescription st -> (st -> IO a) -> (st -> IO a) -> st -> IO a
handleExceptions desc m f st = Ex.catch (m st) $ \ex -> do
   st' <- (exceptionHandler desc) ex st
   f st'

-------------------------------------------------------------------------
-- | The default shell exception handler.  It simply prints the exception
--   and returns the shell state unchanged.  (However, it specificaly
--   ignores the thread killed exception, because that is used to 
--   implement execution canceling)

defaultExceptionHandler :: Ex.Exception -> st -> IO st

defaultExceptionHandler (Ex.AsyncException Ex.ThreadKilled) st = return st
defaultExceptionHandler ex st = do
  putStrLn $ concat ["The following exception occurred:\n   ",show ex]
  return st


-----------------------------------------------------------------------
-- | Prints the help message for this shell, which lists all avaliable
--   commands with their syntax and a short informative message about each.

showShellHelp :: ShellDescription st -> String

showShellHelp desc = show (commandHelpDoc desc (getShellCommands desc))


-------------------------------------------------------------------------
-- | Print the help message for a particular shell command

showCmdHelp :: ShellDescription st -> String -> String

showCmdHelp desc cmd =
  case cmds of
     [_] -> show (commandHelpDoc desc cmds)
     _   -> show (text "bad command name: " <> squotes (text cmd))

 where cmds = filter (\ (n,_,_,_) -> n == cmd) (getShellCommands desc)


commandHelpDoc :: ShellDescription st ->  [(String,CommandParser st,Doc,Doc)] -> Doc

commandHelpDoc desc cmds = 

   vcat [ (fillBreak 20 syn) <+> msg | (_,_,syn,msg) <- cmds ]


------------------------------------------------------------------------------
-- | Creates a shell command which will exit the shell.
exitCommand :: String            -- ^ the name of the command
            -> ShellCommand st
exitCommand name desc = ( name
                        , \_ -> [CompleteParse (\_ -> return (Left ShellExit))]
                        , text (maybeColon desc) <> text name
                        , text "Exit the shell"
                        )


--------------------------------------------------------------------------
-- | Creates a command which will print the shell help message.
helpCommand :: String           -- ^ the name of the command
            -> ShellCommand st
helpCommand name desc = ( name
                        , \_ -> [CompleteParse (\_ -> return (Left (ShellHelp Nothing)))]
                        , text (maybeColon desc) <> text name
                        , text "Display the shell command help"
                        )


----------------------------------------------------------------------------
-- | Creates a simple subshell from a state mapping function 
--   and a shell description.
simpleSubshell :: (st -> IO st')       -- ^ A function to generate the initial subshell 
                                       --   state from the outer shell state
               -> ShellDescription st' -- ^ A shell description for the subshell
               -> IO (Subshell st st')

simpleSubshell toSubSt desc = do
  ref <- newEmptyMVar
  let toSubSt' st     = putMVar ref st >> toSubSt st
  let fromSubSt subSt = takeMVar ref
  let mkDesc _        = return desc
  return (toSubSt',fromSubSt,mkDesc)


----------------------------------------------------------------------------
-- | Execute a subshell, suspending the outer shell until the subshell exits.
runSubshell :: Subshell st st' -- ^ the subshell to execute
            -> st              -- ^ the current state
            -> IO st           -- ^ the modified state


runSubshell (toSubSt, fromSubSt, mkSubDesc) st = do
  subSt   <- toSubSt st
  subDesc <- mkSubDesc subSt
  subSt'  <- runShell subDesc subSt
  st'     <- fromSubSt subSt'
  return st'



-------------------------------------------------------------
-- And now, a clever way to generate shell commands
-- from function signatures by abusing the typeclass
-- mechanism.



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


------------------------------------------------------------------
-- | A typeclass representing user definable completion functions.


class Completion compl st | compl -> st where
  -- | Actually generates the list of possible completions, given the
  --   current shell state and a string representing the beginning of the word.
  complete :: compl -> (st -> String -> IO [String])

  -- | generates a label for the argument for use in the help displays.
  completableLabel :: compl -> String


-------------------------------------------------------------------
-- | Creates a user defined shell commmand.  This relies on the
--   typeclass machenery defined by 'CommandFunction'.
cmd :: CommandFunction f st 
    => String           -- ^ the name of the command
    -> f                -- ^ the command function.  See 'CommandFunction' for restrictions
                        --   on the type of this function.
    -> String           -- ^ the help string for this command
    -> ShellCommand st


cmd name f helpMsg desc =
      ( name
      , parseCommand (wordBreakChars desc) f
      , text (maybeColon desc) <> text name <+> hsep (commandSyntax f)
      , text helpMsg
      )


------------------------------------------------------------------------------
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
  commandSyntax :: f -> [Doc]


-------------------------------------------------------------
-- Instances for the base cases.

instance CommandFunction (FullCommand st) st where
  parseCommand wbc (FullCommand f) str = 
         do (x,[]) <- runRegex (maybeSpaceBefore (Epsilon (CompleteParse f))) str
            return x

  commandSyntax _ = []

instance CommandFunction (StateCommand st) st where
  parseCommand wbc (StateCommand f) str = 
         do (x,[]) <- runRegex (maybeSpaceAfter (Epsilon (CompleteParse (\st -> f st >>= return . Right)))) str
            return x

  commandSyntax _ = []

instance CommandFunction (SimpleCommand st) st where
  parseCommand wbc (SimpleCommand f) str = 
         do (x,[]) <- runRegex (maybeSpaceAfter (Epsilon (CompleteParse (\st -> f >> return (Right st))))) str
            return x

  commandSyntax _ = []


--------------------------------------------------------------
-- Instances for the supported command argument types.


instance CommandFunction r st
      => CommandFunction (Int -> r) st where
  parseCommand = doParseCommand Nothing intRegex id
  commandSyntax f = text (show intRegex) : commandSyntax (f undefined)

instance CommandFunction r st
      => CommandFunction (Integer -> r) st where
  parseCommand = doParseCommand Nothing intRegex id
  commandSyntax f =  text (show intRegex) : commandSyntax (f undefined)

instance CommandFunction r st
      => CommandFunction (Float -> r) st where
  parseCommand = doParseCommand Nothing floatRegex id
  commandSyntax f = text (show floatRegex) : commandSyntax (f undefined)

instance CommandFunction r st
      => CommandFunction (Double -> r) st where
  parseCommand = doParseCommand Nothing floatRegex id
  commandSyntax f = text (show floatRegex) : commandSyntax (f undefined)

instance CommandFunction r st
      => CommandFunction (String -> r) st where
  parseCommand wbc = doParseCommand Nothing (wordRegex wbc) id wbc
  commandSyntax f = text (show (wordRegex "")) : commandSyntax (f undefined)

instance CommandFunction r st
      => CommandFunction (File -> r) st where
  parseCommand wbc = doParseCommand
                        (Just (\st -> RL.filenameCompletionFunction)) (wordRegex wbc) File wbc
  commandSyntax f = text "<file>" : commandSyntax (f undefined)

instance CommandFunction r st
      => CommandFunction (Username -> r) st where
  parseCommand wbc = doParseCommand
                        (Just (\st -> RL.usernameCompletionFunction))
                        (wordRegex wbc)
                        Username
                        wbc
  commandSyntax f = text "<username>" : commandSyntax (f undefined)

instance (CommandFunction r st,Completion compl st)
      => CommandFunction (Completable compl -> r) st where
  parseCommand wbc = doParseCommand
                        (Just (complete (undefined::compl)))
                        (wordRegex wbc)
                        Completable
                        wbc
  commandSyntax f = text (completableLabel (undefined::compl)) : commandSyntax (f undefined)


----------------------------------------------------------------
-- Helper functions used in the above instance declarations
-- These make use of the hackish regex library.

doParseCommand compl re proj wbc f []  = return (IncompleteParse compl)
doParseCommand compl re proj wbc f str =
  let xs = runRegex (maybeSpaceBefore (maybeSpaceAfter re)) str
  in case xs of
        [] -> return (IncompleteParse compl)
        _  -> do (x,str') <- xs; parseCommand wbc (f (proj x)) str'

commandsRegex :: ShellDescription st -> Regex Char (String,CommandParser st,Doc,Doc)
commandsRegex desc =
   case commandStyle desc of
      ColonCommands -> colonCommandsRegex (getShellCommands desc)
      OnlyCommands  -> onlyCommandsRegex  (getShellCommands desc)

onlyCommandsRegex :: [(String,CommandParser st,Doc,Doc)] -> Regex Char (String,CommandParser st,Doc,Doc)
onlyCommandsRegex xs =
    Concat (\_ x -> x) maybeSpaceRegex $
    Concat (\x _ -> x) (anyOfRegex (map (\ (x,y,z,w) -> (x,(x,y,z,w))) xs)) $
                       spaceRegex

colonCommandsRegex :: [(String,CommandParser st,Doc,Doc)] -> Regex Char (String,CommandParser st,Doc,Doc)
colonCommandsRegex xs =
    Concat (\_ x -> x) maybeSpaceRegex $
    Concat (\_ x -> x) (strTerminal ':') $
    Concat (\x _ -> x) (anyOfRegex (map (\ (x,y,z,w) -> (x,(x,y,z,w))) xs)) $
                       spaceRegex
