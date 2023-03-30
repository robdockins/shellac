{-
 -
 -  Copyright 2005-2008, Robert Dockins.
 -
 -}

module System.Console.Shell.Types where

import qualified Control.Exception as Ex

import Control.Monad.Reader
import Control.Monad.State

import System.Console.Shell.PPrint
import System.Console.Shell.Backend


-- | Datatype describing the style of shell commands.  This
--   determines how shell input is parsed.
data CommandStyle
   = OnlyCommands            -- ^ Indicates that all input is to be interpreted as shell commands;
                             --   input is only passed to the evaluation function if it cannot be
                             --   parsed as a command.
   | CharPrefixCommands Char -- ^ Indicates that commands are prefixed with a particular character.
                             --   Colon \':\' is the default character (a la GHCi).
   | SingleCharCommands      -- ^ Commands consist of a single character.


data CommandCompleter st
  = FilenameCompleter
  | UsernameCompleter
  | OtherCompleter (st -> String -> IO [String])

type ShellacException = Ex.SomeException

-- | The result of parsing a command.
data CommandParseResult st

  = CompleteParse (Sh st ())
          -- ^ A complete parse.  A command function is returned.

  | IncompleteParse (Maybe (CommandCompleter st))
          -- ^ An incomplete parse.  A word completion function may be returned.


-- | The type of a command parser.
type CommandParser st = String -> [CommandParseResult st]


-- | The type of a shell command.  The shell description is passed in, and the
--   tuple consists of
--     (command name,command parser,command syntax document,help message document)
type ShellCommand st = ShellDescription st -> (String,CommandParser st,Doc,Doc)


-- | The type of results from shell commands.  They are a modified
--   shell state and possibly a shell \"special\" action to execute.
type CommandResult st = (st,Maybe (ShellSpecial st))


-- | The type of commands which produce output on the shell console.
type OutputCommand = BackendOutput -> IO ()


-- | The type of shell commands.  This monad is a state monad layered over @IO@.
--   The type parameter @st@ allows the monad to carry around a package of
--   user-defined state.
newtype Sh st a = Sh { unSh :: StateT (CommandResult st) (ReaderT OutputCommand IO) a }
   deriving (Monad, MonadIO, MonadFix, Functor, Applicative)

------------------------------------------------------------------------
-- The shell description and utility functions


-- | A record type which describes the attributes of a shell.
data ShellDescription st
   = ShDesc
   { shellCommands      :: [ShellCommand st]        -- ^ Commands for this shell
   , commandStyle       :: CommandStyle             -- ^ The style of shell commands
   , evaluateFunc       :: String -> Sh st ()       -- ^ The evaluation function for this shell

   , greetingText       :: Maybe String             -- ^ Text to print when the shell starts
   , wordBreakChars     :: [Char]                   -- ^ The characters upon which the backend will break words
   , beforePrompt       :: Sh st ()                 -- ^ A shell action to run before each prompt is printed
   , prompt             :: st -> IO String          -- ^ A command to generate the prompt to print
   , secondaryPrompt    :: Maybe (st -> IO String)  -- ^ A command to generate the secondary prompt.  The secondary
                                                    --   prompt is used for multi-line input.  If not set, the
                                                    --   regular prompt is used instead.
   , exceptionHandler   :: ShellacException ->
                              Sh st ()              -- ^ A set of handlers to call when an exception occurs
   , defaultCompletions :: Maybe (st -> String 
                                  -> IO [String])   -- ^ If set, this function provides completions when NOT
                                                    --   in the context of a shell command
   , historyFile        :: Maybe FilePath           -- ^ If set, this provides the path to a file to contain a
                                                    --   history of entered shell commands
   , maxHistoryEntries  :: Int                      -- ^ The maximum number of history entries to maintain
   , historyEnabled     :: Bool                     -- ^ If true, the history mechanism of the backend (if any)
                                                    --   will be used; false will disable history features.
   }

-- | The type of subshells.  The tuple consists of:
--
--    (1) A function to generate the initial subshell state from the outer shell state
--
--    (2) A function to generate the outer shell state from the final subshell state
--
--    (3) A function to generate the shell description from the initial subshell state

type Subshell st st' = (st -> IO st', st' -> IO st, st' -> IO (ShellDescription st') )


-- | Special commands for the shell framework.
data ShellSpecial st
  = ShellExit                  -- ^ Causes the shell to exit
  | ShellHelp (Maybe String)   -- ^ Causes the shell to print an informative message.
                               --   If a command name is specified, only information about
                               --   that command will be displayed
  | ShellNothing               -- ^ Instructs the shell to do nothing; redisplay the prompt and continue
  | ShellContinueLine String   -- ^ Ask the shell to continue accepting input on another line, which should
                               --   be appended to the given string
  | forall st'. ExecSubshell
      (Subshell st st')        -- ^ Causes the shell to execute a subshell
