{-# OPTIONS -fglasgow-exts #-}

module System.Console.Shell where

import Data.Char (toLower, isDigit)
import Data.List ( (\\), isPrefixOf )
import Control.Monad (when)
import Control.Monad.Error ()
import Control.Monad.Trans
import Control.Exception (bracket)
import Control.Concurrent (ThreadId, threadDelay, killThread, forkIO)
import Control.Concurrent.MVar
import System.Posix.Signals (Handler (..), installHandler, keyboardSignal)
import Numeric (readDec, readHex, readFloat)
import qualified System.Console.Readline as RL

data CommandStyle
   = OnlyCommands
   | ColonCommands

type CommandResult st = Either ShellSpecial st
type EvaluationFunction st = String -> [RawToken] -> st -> IO st

data ShellSpecial
  = ShellExit
  | ShellHelp (Maybe String)

type ShellCommand st = 
   (String
   ,[CommandTokenDesc st]
   ,[CommandToken] -> st -> IO (CommandResult st)
   )

data CommandTokenDesc st
   = CTD_Bool
   | CTD_Int
   | CTD_Float
   | CTD_Operator
   | CTD_Word
   | CTD_Words
   | CTD_Filename
   | CTD_Username
   | CTD_Completable String (st -> [CommandToken] -> String -> IO [String])

data CommandToken
   = CT_Bool Bool
   | CT_Int Integer
   | CT_Float Integer Int
   | CT_Word String
   | CT_Words [String]
   | CT_Operator String

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
       , evaluateFunc       = \_ _ st -> return st
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

   case lexer desc before of
      Nothing -> return Nothing
      Just rawToks ->
         case beginCommand desc rawToks of
           Nothing -> if null rawToks then completeCommands desc before word after else return Nothing
           Just (cmdName,cmdToks) ->
              case lookupCommand cmdName (shellCommands desc) of
                 Nothing -> return Nothing
                 Just (cmdDesc,_) ->
                   case doLexCmd cmdDesc rawToks [] of
                      Left _ -> return Nothing
                      Right (cmdTokens,(tokDesc:_)) -> applyCompletions st cmdTokens tokDesc before word after


completeCommands :: ShellDescription st -> String -> String -> String -> IO (Maybe (String,[String]))
completeCommands desc before word after = do
    case cmdPart of
       Nothing  -> return Nothing
       Just cmd -> 
          let matches = matchingNames cmd
              word'   = maximalPrefix matches
          in if null matches
                then return Nothing
                else return $ Just (mkCmd word',map mkCmd matches)

   where matchingNames cmd = filter (cmd `isPrefixOf`) cmdNames
         cmdNames = map (\(x,_,_) -> x) (shellCommands desc)

         mkCmd cmd = case commandStyle desc of
                       ColonCommands -> ':':cmd
                       OnlyCommands  -> cmd

         cmdPart = case commandStyle desc of
                     ColonCommands -> 
                        case word of
                         ':':cmd   -> Just cmd
                         _         -> Nothing
                     OnlyCommands  -> Just word


applyCompletions :: st -> [CommandToken] -> CommandTokenDesc st -> String -> String -> String -> IO (Maybe (String,[String]))
applyCompletions st cmdToks tokDesc before word after = 
     do allCompletions <- generateCompletions st cmdToks word tokDesc
        let word' = maximalPrefix allCompletions
        if null allCompletions
          then return Nothing
          else return (Just (before++word'++after,allCompletions))

generateCompletions :: st -> [CommandToken] -> String -> CommandTokenDesc st -> IO [String]

generateCompletions st cmdToks word CTD_Bool =
    let word' = map toLower word 
        possibleBools = ["yes","no","true","false","0","1"]
    in return $ filter (word' `isPrefixOf`) possibleBools

generateCompletions st cmdToks word (CTD_Completable _ f) = f st cmdToks word
generateCompletions st cmdToks word CTD_Filename        = RL.filenameCompletionFunction word
generateCompletions st cmdToks word CTD_Username        = RL.usernameCompletionFunction word
generateCompletions st cmdToks word CTD_Int             = return []
generateCompletions st cmdToks word CTD_Float           = return []
generateCompletions st cmdToks word CTD_Operator        = return []
generateCompletions st cmdToks word CTD_Word            = return []
generateCompletions st cmdToks word CTD_Words           = return []


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

   handleInput inp st =
     case lexer desc inp of
        Nothing -> error "lexer failed to analyse line"
        Just rawToks -> do
           when (not $ null rawToks) (RL.addHistory inp)
           putStrLn (show rawToks)
           case beginCommand desc rawToks of
             Nothing                -> evaluateInp inp rawToks st
             Just (cmdName,cmdToks) -> 
                do cmdResult <- executeCommand desc cmdName cmdToks st
                   case cmdResult of
                      Left spec -> handleSpecial st spec
                      Right st' -> loop st'

   handleSpecial st ShellExit              = return st
   handleSpecial st (ShellHelp Nothing)    = putStrLn (showShellHelp desc)   >> loop st
   handleSpecial st (ShellHelp (Just cmd)) = putStrLn (showCmdHelp desc cmd) >> loop st


   evaluateInp inp rawToks st =
     let m = evalMVar iss
         t = evalThreadMVar iss
         h = cancelHandler iss
         e = evaluateFunc desc
     in do tid <- forkIO (e inp rawToks st >>= putMVar m . Just)
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
showShellHelp desc = "you need help..."

showCmdHelp :: ShellDescription st -> String -> String
showCmdHelp desc cmd = "cmd help: "++cmd

lookupCommand :: String -> [ShellCommand st] -> Maybe ([CommandTokenDesc st],[CommandToken] -> st -> IO (CommandResult st))
lookupCommand cmd cmds = 
     case matches of
       [(_,desc,action)] -> Just (desc,action)
       _         -> Nothing

   where matches = filter (\(x,_,_) -> cmd `isPrefixOf` x) cmds

executeCommand :: ShellDescription st -> String -> [RawToken] -> st -> IO (CommandResult st)
executeCommand desc cmd toks st =
  case lookupCommand cmd (shellCommands desc) of
      Nothing -> putStrLn (concat ["unknown command '",cmd,"'"]) >> return (Right st)
      Just (cmdDesc,cmdAction) ->
         case doLexCmd cmdDesc toks [] of
            Right (cmdToks,[]) -> cmdAction cmdToks st
            Left msg ->
               do putStrLn msg 
                  putStrLn $ "    "++(showCmdSyntax desc cmd cmdDesc)
                  return (Right st)
            _  ->
               do putStrLn "bad command syntax"
                  putStrLn $ "    "++(showCmdSyntax desc cmd cmdDesc)
                  return (Right st)  


showCmdSyntax :: ShellDescription st -> String -> [CommandTokenDesc st] -> String
showCmdSyntax desc cmdName cmdDesc = cmdForm++args

  where args = concat $ map (" "++) $ map formatArg cmdDesc
        cmdForm = case commandStyle desc of 
                    OnlyCommands -> cmdName
                    ColonCommands -> ":"++cmdName

        formatArg CTD_Bool  = "<boolean>"
        formatArg CTD_Int   = "<integer>"
        formatArg CTD_Float = "<float>"
        formatArg CTD_Operator = "<operator>"
        formatArg CTD_Word  = "<word>"
        formatArg CTD_Words = "{<word>}"
        formatArg CTD_Filename = "<filename>"
        formatArg CTD_Username = "<username>"
        formatArg (CTD_Completable l _) = "<"++l++">" 



beginCommand :: ShellDescription st -> [RawToken] -> Maybe (String,[RawToken])
beginCommand desc words =
  case commandStyle desc of
      OnlyCommands  ->
         case words of
            ((Raw_Word cmd)
              : rest) -> Just (cmd,rest)
            _         -> Nothing

      ColonCommands ->
         case words of
            ((Raw_Word (':':cmd))
               : rest) -> Just (cmd,rest)
            _          -> Nothing


doLexCmd :: [CommandTokenDesc st] -> [RawToken] -> [CommandToken] 
         -> Either String ([CommandToken],[CommandTokenDesc st])

doLexCmd desc [] ts   = return (reverse ts,desc)
doLexCmd [] toks ts   = fail "bad command syntax"

doLexCmd (CTD_Bool:cs) (Raw_Word w:ws) ts
   | lw `elem` trues  = doLexCmd cs ws (CT_Bool True  : ts)
   | lw `elem` falses = doLexCmd cs ws (CT_Bool False : ts) 
   | otherwise        = fail $ concat ["'",w,"' is not a valid boolean"]
 where lw = map toLower w
       trues  = ["yes","true","t","y","1"]
       falses = ["no","false","f","n","0"]

doLexCmd (CTD_Int:cs) (Raw_Word w:ws) ts =
    case w of
        '+':w' -> doRead w' id
        '-':w' -> doRead w' negate
        w'     -> doRead w' id

  where doRead w f =
         case readDec w of
           (val,""):_ -> doLexCmd cs ws (CT_Int (f val) : ts)
           _          -> fail $ concat ["'",w,"' is not a valid integer"]


doLexCmd (CTD_Float:cs) (Raw_Word w:ws) ts =
   case w of
        '+':w' -> doRead w' id
        '-':w' -> doRead w' negate
        w'     -> doRead w' id

  where doRead w f =
         case readFloat w of
             (val::Double,""):_ -> let (a,b) = decodeFloat (f val) in doLexCmd cs ws (CT_Float a b : ts)
             _                  -> fail $ concat ["'",w,"' is not a valid floating point number"]

doLexCmd (CTD_Operator:cs) (Raw_Op op:ws) ts = 
    doLexCmd cs ws (CT_Operator op : ts)

doLexCmd (CTD_Word:cs) (Raw_Word w:ws) ts =
    doLexCmd cs ws (CT_Word w : ts)

doLexCmd (CTD_Filename:cs) (Raw_Word w:ws) ts =
    doLexCmd cs ws (CT_Word w : ts)

doLexCmd (CTD_Username:cs) (Raw_Word w:ws) ts =
    doLexCmd cs ws (CT_Word w : ts)

doLexCmd (CTD_Completable _ _:cs) (Raw_Word w:ws) ts =
    doLexCmd cs ws (CT_Word w : ts)

doLexCmd [CTD_Words] ws ts = 
    sequence (map toCTWord ws) >>= \ws' -> return (reverse (CT_Words ws' : ts),[])

    where toCTWord (Raw_Word w) = return w
          toCTWord (Raw_Op _)   = fail "bad command syntax"

doLexCmd _ _ _ = fail "bad command syntax"

data RawToken
   = Raw_Word String
   | Raw_Op String
 deriving Show

-- break the string into "words" and "operators"
lexer :: ShellDescription st -> String -> Maybe [RawToken]
lexer desc input = lexSpace [] input
 where
   space     = " \t\n\r\v"
   operators = (wordBreakChars desc) \\ space
   mkOp str = Raw_Op (reverse str)
   mkWd str = Raw_Word (reverse str)

   lexSpace ws []                = Just (reverse ws)
   lexSpace ws ('\'':is)         = lexSQuotes ws [] is
   lexSpace ws ('\"':is)         = lexDQuotes ws [] is
   lexSpace ws (i:is)
     | i `elem` space            = lexSpace ws is
     | i `elem` operators        = lexOps   ws [i] is
     | otherwise                 = lexWord  ws [i] is

   lexOps ws acc []              = Just (reverse (mkOp acc:ws))
   lexOps ws acc ('\'':is)       = lexSQuotes (mkOp acc:ws) [] is
   lexOps ws acc ('\"':is)       = lexDQuotes (mkOp acc:ws) [] is
   lexOps ws acc (i:is)
     | i `elem` space            = lexSpace (mkOp acc:ws) is
     | i `elem` operators        = lexOps   ws (i:acc) is
     | otherwise                 = lexWord  (mkOp acc:ws) [i] is

   lexWord ws acc []             = Just (reverse (mkWd acc:ws))
   lexWord ws acc ('\'':is)      = lexSQuotes (mkWd acc:ws) [] is
   lexWord ws acc ('\"':is)      = lexDQuotes (mkWd acc:ws) [] is
   lexWord ws acc (i:is)
     | i `elem` space            = lexSpace (mkWd acc:ws) is
     | i `elem` operators        = lexOps   (mkWd acc:ws) [i] is
     | otherwise                 = lexWord  ws (i:acc) is

   lexSQuotes ws acc []          = Nothing
   lexSQuotes ws acc ('\'':is)   = lexSpace (mkWd acc:ws) is
   lexSQuotes ws acc (i:is)      = lexSQuotes ws (i:acc) is

   lexDQuotes ws acc []          = Nothing
   lexDQuotes ws acc ('\"':is)   = lexSpace (mkWd acc:ws) is
   lexDQuotes ws acc ('\\':is)   = lexEscape ws acc is
   lexDQuotes ws acc (i:is)      = lexDQuotes ws (i:acc) is

   lexEscape ws acc []           = Nothing
   lexEscape ws acc ('a':is)     = lexDQuotes ws ('\a':acc) is
   lexEscape ws acc ('b':is)     = lexDQuotes ws ('\b':acc) is
   lexEscape ws acc ('f':is)     = lexDQuotes ws ('\f':acc) is
   lexEscape ws acc ('n':is)     = lexDQuotes ws ('\n':acc) is
   lexEscape ws acc ('r':is)     = lexDQuotes ws ('\r':acc) is
   lexEscape ws acc ('t':is)     = lexDQuotes ws ('\t':acc) is
   lexEscape ws acc ('v':is)     = lexDQuotes ws ('\v':acc) is
   lexEscape ws acc ('\\':is)    = lexDQuotes ws ('\\':acc) is
   lexEscape ws acc ('\"':is)    = lexDQuotes ws ('\"':acc) is
   lexEscape ws acc ('0':'x':is) = lexHexEscape ws acc is
   lexEscape ws acc ('x':is)     = lexHexEscape ws acc is
   lexEscape ws acc (i:is)
       | isDigit i               = lexDecEscape ws acc (i:is)
       | otherwise               = lexDQuotes ws (i:acc) is

   lexDecEscape = lexNumericEscape readDec
   lexHexEscape = lexNumericEscape readHex

   lexNumericEscape reader ws acc is =
      case reader is of
           []         -> Nothing
           [(v,rest)] -> lexDQuotes ws (toEnum v:acc) rest
           _          -> error "impossible: multiple parses for numeric escape sequence"


-------------------------------------------------------------
-- command creation

data SimpleCommand st
data StateCommand st
data FullCommand st

simpleCommand :: forall f st. CommandFunction f st (SimpleCommand st) => String -> f -> ShellCommand st
simpleCommand name f = (name,getDescriptor f (undefined::SimpleCommand st),runCommand f (undefined::SimpleCommand st))

stateCommand :: forall f st. CommandFunction f st (StateCommand st) => String -> f -> ShellCommand st
stateCommand name f = (name,getDescriptor f (undefined::StateCommand st),runCommand f (undefined::StateCommand st))

fullCommand :: forall f st. CommandFunction f st (FullCommand st) => String -> f -> ShellCommand st
fullCommand name f = (name,getDescriptor f (undefined::FullCommand st),runCommand f (undefined::FullCommand st))

exitCommand :: String -> ShellCommand st
exitCommand name = (name, [], \_ _ -> return (Left ShellExit))

helpCommand :: String -> ShellCommand st
helpCommand name = (name, [], \_ _ -> return (Left (ShellHelp Nothing)))

--------------------------------------------------------------
-- typeclass machenary for command creation

class CommandFunction f st base | base -> st where
   runCommand    :: f -> base -> [CommandToken] -> st -> IO (CommandResult st)
   getDescriptor :: f -> base -> [CommandTokenDesc st]

instance CommandFunction (IO ()) st (SimpleCommand st) where
   runCommand f _ [] st = f >> return (Right st)
   getDescriptor _ _ = []

instance CommandFunction (st -> IO st) st (StateCommand st) where
   runCommand f _ [] st = f st >>= return . Right
   getDescriptor _ _ = []

instance CommandFunction (st -> IO (CommandResult st)) st (FullCommand st) where
   runCommand f _ [] st = f st
   getDescriptor _ _ = []

instance CommandFunction f st base => CommandFunction (Bool -> f) st base where
   runCommand f b (CT_Bool bool :xs) st = runCommand (f bool) b xs st
   getDescriptor f b = CTD_Bool : getDescriptor (f undefined) b

instance CommandFunction f st base => CommandFunction (Int -> f) st base where
   runCommand f b (CT_Int i:xs) st = runCommand (f (fromInteger i)) b xs st
   getDescriptor f b = CTD_Int : getDescriptor (f undefined) b

instance CommandFunction f st base => CommandFunction (Integer -> f) st base where
   runCommand f b (CT_Int i:xs) st = runCommand (f i) b xs st
   getDescriptor f b = CTD_Int : getDescriptor (f undefined) b

instance CommandFunction f st base => CommandFunction (Float -> f) st base where
   runCommand f b (CT_Float x y:xs) st = runCommand (f (encodeFloat x y)) b xs st
   getDescriptor f b = CTD_Float : getDescriptor (f undefined) b

instance CommandFunction f st base => CommandFunction (Double -> f) st base where
   runCommand f b (CT_Float x y:xs) st = runCommand (f (encodeFloat x y)) b xs st
   getDescriptor f b = CTD_Float : getDescriptor (f undefined) b

instance CommandFunction f st base => CommandFunction (String -> f) st base where
   runCommand f b ((CT_Word w) : xs)     st = runCommand (f w) b xs st
   getDescriptor f b = CTD_Word : getDescriptor (f undefined) b

instance CommandFunction f st base => CommandFunction ([String] -> f) st base where
   runCommand f b ((CT_Words ws) : xs) st = runCommand (f ws) b xs st
   getDescriptor f b = CTD_Words : getDescriptor (f undefined) b
