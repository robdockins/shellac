{-
 -
 -  Copyright 2005-2006, Robert Dockins.
 -
 -}

{- | This module implements a framework for creating read-eval-print style
     command shells.  Shells are created by declaratively defining evaluation
     functions and \"shell commands\".  Input is read using a plugable backend.
     The shell framework handles command history and word completion if the
     backend supports it.

     The basic idea is for creating a shell is:

      (1) Create a list of shell commands and an evaluation function

      (2) Create a shell description (using 'mkShellDescription')

      (3) Set up the initial shell state

      (4) Run the shell (using 'runShell')
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

-- * Printing Help Messages
, showShellHelp
, showCmdHelp

-- * Type Synonyms and Auxiliary Types
, CommandStyle (..)
, ShellSpecial (..)
, EvaluationFunction
, OutputCommand
) where

import Maybe                       ( isJust )
import Data.Char                   ( isSpace )
import Data.List                   ( isPrefixOf, find )
import Data.IORef                  ( IORef, newIORef, readIORef, writeIORef )
import Control.Monad               ( when, MonadPlus(..) )
import Control.Monad.Error         ()
import Control.Concurrent          ( ThreadId, threadDelay, killThread, forkIO )
import Control.Concurrent.STM      ( atomically, retry )
import Control.Concurrent.STM.TVar ( newTVar, readTVar, writeTVar, TVar )
import System.Directory            ( doesFileExist )
import System.Posix.Signals        ( Handler (..), installHandler, keyboardSignal )
import qualified Control.Exception as Ex

import System.Console.Shell.PPrint
import System.Console.Shell.Regex
import System.Console.Shell.Backend


-- | Datatype describing the style of shell commands.  This
--   determines how shell input is parsed.
data CommandStyle
   = OnlyCommands       -- ^ Indicates that all input is to be interpreted as shell commands; no
                        --   input will be passed to the evaluation function.
   | ColonCommands      -- ^ Indicates that commands are prefaced with a colon ':' character.
   | SingleCharCommands -- ^ Commands consisit of a single character




-- | The type of results from shell commands.  They are a modified
--   shell state and possibly a shell \"special\" action to execute.

type CommandResult st = (st,Maybe (ShellSpecial st))


-- | The type of commands which produce output on the shell console.

type OutputCommand = String -> IO ()


-- | The type of an evaluation function for a shell.  The function
--   takes three arguments:
--
--   1) An output function -- this function writes a string to the 
--      shell "console".  Use this function rather than putStr and friends.
--
--   2) The input string
--
--   3) The current shell state
--
--   Evaluation functions should return the new shell state and
--   possibly a shell special action to execute.

type EvaluationFunction st  = OutputCommand
                           -> String 
                           -> st
                           -> IO (st,Maybe (ShellSpecial st))





-- | Special commands for the shell framework.
data ShellSpecial st
  = ShellExit                  -- ^ Causes the shell to exit
  | ShellHelp (Maybe String)   -- ^ Causes the shell to print an informative message.
                               --   If a command name is specified, only information about
                               --   that command will be displayed
  | ShellNothing               -- ^ Instructs the shell to do nothing; redisplay the prompt and continue
  | forall st'. ExecSubshell
      (Subshell st st')        -- ^ Causes the shell to execute a subshell

data CommandCompleter st
  = FilenameCompleter
  | UsernameCompleter
  | OtherCompleter (st -> String -> IO [String])

-- | The result of parsing a command.
data CommandParseResult st

  = CompleteParse (OutputCommand -> st -> IO (CommandResult st))
          -- ^ A complete parse.  A command function is returned.

  | IncompleteParse (Maybe (CommandCompleter st))
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
   , wordBreakChars     :: [Char]                   -- ^ The characters upon which the backend will break words
   , beforePrompt       :: OutputCommand
                        -> st 
                        -> IO ()                    -- ^ an IO action to run before each prompt is printed
   , prompt             :: String                   -- ^ The prompt to print
   , exceptionHandler   :: OutputCommand
                        -> Ex.Exception 
                        -> st 
                        -> IO st                    -- ^ A function called when an exception occurs
   , defaultCompletions :: Maybe (st -> String 
                                  -> IO [String])   -- ^ If set, this function provides completions when NOT
                                                    --   in the context of a shell command
   , historyFile        :: Maybe FilePath           -- ^ If set, this provides the path to a file to contain a
                                                    --   history of entered shell commands
   , maxHistoryEntries  :: Int                      -- ^ The maximum number of history entries to maintain
   , historyEnabled     :: Bool                     -- ^ If true, the history mechanism of the backend (if any)
                                                    --   will be used; false will disable history features.
   }

-- | A basic shell description with sane initial values
initialShellDescription :: IO (ShellDescription st)
initialShellDescription =
  do let wbc = " \t\n\r\v`~!@#$%^&*()=[]{};\\\'\",<>"
     return ShDesc
       { shellCommands      = []
       , commandStyle       = ColonCommands
       , evaluateFunc       = \_ _ st -> return (st,Nothing)
       , wordBreakChars     = wbc
       , beforePrompt       = \putCmd _ -> putCmd ""
       , prompt             = "> "
       , exceptionHandler   = defaultExceptionHandler
       , defaultCompletions = Just (\_ _ -> return [])
       , historyFile        = Nothing
       , maxHistoryEntries  = 100
       , historyEnabled     = True
       }

-- | Creates a simple shell description from a list of shell commmands and
--   an evalation function.
mkShellDescription :: [ShellCommand st]
                   -> EvaluationFunction st 
                   -> IO (ShellDescription st)

mkShellDescription cmds func =
   do desc <- initialShellDescription
      return desc
             { shellCommands = cmds
             , evaluateFunc  = func
             }


-------------------------------------------------------------------
-- A record to hold some of the internal muckety-muck needed
-- to make the shell go.  This is mostly concurrency variables
-- needed to handle keyboard interrupts.

data InternalShellState st bst
   = InternalShellState
     { evalTVar         :: TVar (Maybe (st,Maybe (ShellSpecial st)))
     , evalThreadTVar   :: TVar (Maybe ThreadId)
     , evalCancelTVar   :: TVar Bool
     , cancelHandler    :: Handler
     , backendState     :: bst
     }


-------------------------------------------------------------------
-- Main entry point for the shell.  Sets up the crap needed to
-- run shell commands and evaluation in a separate thread and 
-- initializes the backend.


-- | Run a shell.  Given a shell description, a shell backend to use
--   and an initial state this function runs the shell until it exits,
--   and then returns the final state.

runShell :: ShellDescription st
         -> ShellBackend bst
         -> st
         -> IO st

runShell desc backend init = Ex.bracket setupShell exitShell (\iss -> shellLoop desc backend iss init)

  where setupShell = do
            evalVar   <- atomically (newTVar Nothing)
            thVar     <- atomically (newTVar Nothing)
            cancelVar <- atomically (newTVar False)
            bst       <- initBackend backend

            when (historyEnabled desc) (do
   	       setMaxHistoryEntries backend bst (maxHistoryEntries desc)
               loadHistory desc backend bst)

            return InternalShellState
                   { evalTVar       = evalVar
                   , evalThreadTVar = thVar
                   , evalCancelTVar = cancelVar
                   , cancelHandler  = Catch (handleINT evalVar cancelVar thVar)
                   , backendState = bst
                   }

        exitShell iss = do
            when (historyEnabled desc) (do
               saveHistory desc backend (backendState iss)
	       clearHistoryState backend (backendState iss))
	    flushOutput backend (backendState iss)

        handleINT evalVar cancelVar thVar = do
            maybeTid <- atomically (do
 	    	         result <- readTVar evalVar
	                 if isJust result
                            then return Nothing
                            else do writeTVar cancelVar True
                                    writeTVar evalVar Nothing
                                    tid <- readTVar thVar
	                            case tid of
                                       Nothing  -> retry
                                       Just tid -> return (Just tid))


            case maybeTid of
               Nothing  -> return ()
               Just tid -> killThread tid


-------------------------------------------------------------------------
-- This function is installed as the attempted completion function.
-- It attempts to match the prefix of the input buffer against a
-- command.  If it matches, it supplies the completions appropriate
-- for that point in the command.  Otherwise it returns Nothing; in
-- that case, the backend will fall back on the default completion function
-- set in the shell description.

completionFunction :: ShellDescription st
                   -> ShellBackend bst
                   -> bst
                   -> st
                   -> (String,String,String)
                   -> IO (Maybe (String,[String]))

completionFunction desc backend bst st line@(before,word,after) = do
   if all isSpace before
     then completeCommands desc line
     else case runRegex (commandsRegex desc) before of
         [((_,cmdParser,_,_),before')] -> do
                let completers  = [ z | IncompleteParse (Just z) <- cmdParser before' ]
                strings <- case completers of
                              FilenameCompleter:_  -> completeFilename backend bst word >>= return . Just
                              UsernameCompleter:_  -> completeUsername backend bst word >>= return . Just
                              (OtherCompleter f):_ -> f st word >>= return . Just
                              _ -> return Nothing
                case strings of
                   Nothing -> return Nothing
                   Just [] -> return Nothing
                   Just xs -> return (Just (maximalPrefix xs,xs))

         _ -> return Nothing



completeCommands :: ShellDescription st
                 -> (String,String,String)
                 -> IO (Maybe (String,[String]))

completeCommands desc (before,word,after) =
    case matchingNames of
       [] -> return $ Nothing
       xs -> return $ Just (maximalPrefix xs,xs)

  where matchingNames = filter (word `isPrefixOf`) cmdNames
        cmdNames      = map (\ (n,_,_,_) -> (maybeColon desc)++n) (getShellCommands desc)

maybeColon :: ShellDescription st -> String
maybeColon desc = case commandStyle desc of ColonCommands -> ":"; _ -> ""

getShellCommands desc = map ($ desc) (shellCommands desc)

maximalPrefix :: [String] -> String
maximalPrefix [] = []
maximalPrefix (x:xs) = f x xs
  where f p [] = p
        f p (x:xs) = f (fst $ unzip $ takeWhile (\x -> fst x == snd x) $ zip p x) xs



-----------------------------------------------------------
-- Deal with reading and writing history files.

loadHistory :: ShellDescription st
            -> ShellBackend bst
            -> bst
            -> IO ()

loadHistory desc backend bst =
  case historyFile desc of
     Nothing   -> return ()
     Just path -> do
        fexists <- doesFileExist path
        when fexists $
           Ex.handle (\ex -> (outputErrString backend bst) $ concat ["could not read history file '",path,"'\n   ",show ex])
             (readHistory backend bst path)

saveHistory :: ShellDescription st
            -> ShellBackend bst
            -> bst
            -> IO ()

saveHistory desc backend bst =
  case historyFile desc of
    Nothing   -> return ()
    Just path ->
       Ex.handle (\ex -> (outputErrString backend bst) $ concat ["could not write history file '",path,"'\n    ",show ex])
          (writeHistory backend bst path)


-----------------------------------------------------------
-- The real meat.  We setup backend stuff, call the backend
-- to get the input string, and then handle the input.


shellLoop :: forall st bst
           . ShellDescription st
          -> ShellBackend bst
          -> InternalShellState st bst
          -> st
          -> IO st

shellLoop desc backend iss init = loop init
 where
   bst = backendState iss

   loop :: st -> IO st

   loop st =
     do flushOutput backend bst
        beforePrompt desc (outputString backend bst) st
        setAttemptedCompletionFunction backend bst
	      (completionFunction desc backend bst st)

        case defaultCompletions desc of
           Nothing -> setDefaultCompletionFunction backend bst $ Nothing
           Just f  -> setDefaultCompletionFunction backend bst $ Just (f st)

        setWordBreakChars backend bst (wordBreakChars desc)

        inp <- case commandStyle desc of

                 SingleCharCommands -> do
                      c <- getSingleChar backend bst (prompt desc)
                      return (fmap (:[]) c)

                 _ -> getInput backend bst (prompt desc)


        case inp of
           Nothing   -> return st
           Just inp' -> handleInput inp' st


   handleInput :: String -> st -> IO st
   handleInput inp st = do
     when (historyEnabled desc && (isJust (find (not . isSpace) inp)))
          (addHistory backend bst inp)

     let inp' = inp++" " -- hack, makes commands unambiguous

     case runRegex (commandsRegex desc) inp' of
       (x,inp''):_ -> executeCommand x inp'' st
       []          -> evaluateInput inp st


   executeCommand :: (String,CommandParser st,Doc,Doc) -> String -> st -> IO st
   executeCommand (cmdName,cmdParser,_,_) inp st =
      let parses  = cmdParser inp
          parses' = concatMap (\x -> case x of CompleteParse z -> [z]; _ -> []) parses
      in case parses' of
          f:_ -> do
              r <- handleExceptions desc (f (outputString backend bst)) st
              case r of
                  (st',Just spec) -> handleSpecial st' spec
                  (st',Nothing)   -> loop st'

          _   -> outputString backend bst (showCmdHelp desc cmdName) >> loop st

   handleSpecial :: st -> ShellSpecial st -> IO st
   handleSpecial st ShellExit               = return st
   handleSpecial st ShellNothing            = loop st
   handleSpecial st (ShellHelp Nothing)     = outputString backend bst (showShellHelp desc)   >> loop st
   handleSpecial st (ShellHelp (Just cmd))  = outputString backend bst (showCmdHelp desc cmd) >> loop st
   handleSpecial st (ExecSubshell subshell) = runSubshell desc subshell backend bst st >>= loop

   handleExceptions :: ShellDescription st 
                    -> (st -> IO (st,Maybe (ShellSpecial st))) 
                    -> st 
                    -> IO (st,Maybe (ShellSpecial st))
   handleExceptions desc f st = Ex.catch (f st) $ \ex -> do
      st' <- (exceptionHandler desc) (outputString backend bst) ex st
      return (st',Nothing)


   runThread :: EvaluationFunction st
             -> String
             -> InternalShellState st bst 
             -> st 
             -> IO ()

   runThread eval inp iss st = do
      val <- handleExceptions desc (eval (outputString backend bst) inp) st
      atomically (do
         cancled <- readTVar (evalCancelTVar iss)
	 if cancled then return () else do
           writeTVar (evalTVar iss) (Just val))

   evaluateInput :: String
                 -> st
                 -> IO st

   evaluateInput inp st =
     let eVar = evalTVar iss
         cVar = evalCancelTVar iss
         tVar = evalThreadTVar iss
         h = cancelHandler iss
         e = evaluateFunc desc
     in do atomically (writeTVar cVar False >> writeTVar eVar Nothing >> writeTVar tVar Nothing)
           tid <- forkIO (runThread e inp iss st)
	   atomically (writeTVar tVar (Just tid))
           result <- Ex.bracket
              (installHandler keyboardSignal h Nothing)
              (\oldh -> installHandler keyboardSignal oldh Nothing)
              (\_ -> atomically (do
                  canceled <- readTVar cVar
                  if canceled then return Nothing else do
                    result <- readTVar eVar
                    case result of
                       Nothing -> retry
                       Just r  -> return (Just r)))

           case result of
             Nothing              -> onCancel backend bst >> loop st
             Just (st',Just spec) -> handleSpecial st' spec
             Just (st',Nothing)   -> loop st'

-------------------------------------------------------------------------
-- | The default shell exception handler.  It simply prints the exception
--   and returns the shell state unchanged.  (However, it specificaly
--   ignores the thread killed exception, because that is used to
--   implement execution canceling)

defaultExceptionHandler :: OutputCommand -> Ex.Exception -> st -> IO st

defaultExceptionHandler putCmd (Ex.AsyncException Ex.ThreadKilled) st = return st
defaultExceptionHandler putCmd ex st = do
  putCmd $ concat ["The following exception occurred:\n   ",show ex]
  return st


-----------------------------------------------------------------------
-- | Prints the help message for this shell, which lists all avaliable
--   commands with their syntax and a short informative message about each.

showShellHelp :: ShellDescription st -> String

showShellHelp desc = show (commandHelpDoc desc (getShellCommands desc)) ++ "\n"


-------------------------------------------------------------------------
-- | Print the help message for a particular shell command

showCmdHelp :: ShellDescription st -> String -> String

showCmdHelp desc cmd =
  case cmds of
     [_] -> show (commandHelpDoc desc cmds) ++ "\n"
     _   -> show (text "bad command name: " <> squotes (text cmd)) ++ "\n"

 where cmds = filter (\ (n,_,_,_) -> n == cmd) (getShellCommands desc)


commandHelpDoc :: ShellDescription st ->  [(String,CommandParser st,Doc,Doc)] -> Doc

commandHelpDoc desc cmds =

   vcat [ (fillBreak 20 syn) <+> msg | (_,_,syn,msg) <- cmds ]


------------------------------------------------------------------------------
-- | Creates a shell command which will exit the shell.
exitCommand :: String            -- ^ the name of the command
            -> ShellCommand st
exitCommand name desc = ( name
                        , \_ -> [CompleteParse (\_ st -> return (st,Just ShellExit))]
                        , text (maybeColon desc) <> text name
                        , text "Exit the shell"
                        )


--------------------------------------------------------------------------
-- | Creates a command which will print the shell help message.
helpCommand :: String           -- ^ the name of the command
            -> ShellCommand st
helpCommand name desc = ( name
                        , \_ -> [CompleteParse (\_ st -> return (st,Just (ShellHelp Nothing)))]
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
  ref <- newIORef undefined
  let toSubSt' st     = writeIORef ref st >> toSubSt st
  let fromSubSt subSt = readIORef ref
  let mkDesc _        = return desc
  return (toSubSt',fromSubSt,mkDesc)


----------------------------------------------------------------------------
-- | Execute a subshell, suspending the outer shell until the subshell exits.
runSubshell :: ShellDescription desc -- ^ the description of the outer shell
            -> Subshell st st'       -- ^ the subshell to execute
            -> ShellBackend bst      -- ^ the shell backend to use
            -> bst                   -- ^ the backendstate
            -> st                    -- ^ the current state
            -> IO st                 -- ^ the modified state


runSubshell desc (toSubSt, fromSubSt, mkSubDesc) backend bst st = do
  subSt   <- toSubSt st
  subDesc <- mkSubDesc subSt
  subSt'  <- runShell subDesc backend subSt
  st'     <- fromSubSt subSt'
  return st'


-------------------------------------------------------------
-- And now, a clever way to generate shell commands
-- from function signatures by abusing the typeclass
-- mechanism.


-- | A shell command which can return shell special commands as well as
--   modifying the shell state
newtype FullCommand st   = FullCommand (OutputCommand -> st -> IO (CommandResult st))

-- | A shell command which can modify the shell state.
newtype StateCommand st  = StateCommand (OutputCommand -> st -> IO st)

-- | A shell command which does not alter the shell state.
newtype SimpleCommand st = SimpleCommand (OutputCommand -> IO ())

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
--   commands.  The type of \'f\' is restricted to have a restricted set of
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
         do (x,[]) <- runRegex (maybeSpaceAfter (Epsilon (CompleteParse (\putCmd st -> f putCmd st >>= \st' -> return (st',Nothing))))) str
            return x

  commandSyntax _ = []

instance CommandFunction (SimpleCommand st) st where
  parseCommand wbc (SimpleCommand f) str =
         do (x,[]) <- runRegex (maybeSpaceAfter (Epsilon (CompleteParse (\putCmd st -> f putCmd >> return (st,Nothing))))) str
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
                        (Just FilenameCompleter)
                        (wordRegex wbc)
                        File
                        wbc
  commandSyntax f = text "<file>" : commandSyntax (f undefined)

instance CommandFunction r st
      => CommandFunction (Username -> r) st where
  parseCommand wbc = doParseCommand
                        (Just UsernameCompleter)
                        (wordRegex wbc)
                        Username
                        wbc
  commandSyntax f = text "<username>" : commandSyntax (f undefined)

instance (CommandFunction r st,Completion compl st)
      => CommandFunction (Completable compl -> r) st where
  parseCommand wbc = doParseCommand
                        (Just (OtherCompleter (complete (undefined::compl))))
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
      ColonCommands      -> colonCommandsRegex     (getShellCommands desc)
      OnlyCommands       -> onlyCommandsRegex      (getShellCommands desc)
      SingleCharCommands -> singleCharCommandRegex (getShellCommands desc)

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

singleCharCommandRegex :: [(String,CommandParser st,Doc,Doc)] -> Regex Char (String,CommandParser st,Doc,Doc)
singleCharCommandRegex xs =
    altProj
       (anyOfRegex (map (\ (x,y,z,w) -> ([head x],(x,y,z,w))) xs))
       (Epsilon ("",\_ -> [CompleteParse (\_ st -> return (st,Just ShellNothing))],empty,empty))
