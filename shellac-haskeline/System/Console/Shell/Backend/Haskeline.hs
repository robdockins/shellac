module System.Console.Shell.Backend.Haskeline
                    (haskelineBackend,
                    ShellacState) where

import System.Console.Shell.Backend
import System.Console.Haskeline hiding (completeFilename)
import qualified System.Console.Haskeline.History as History
import System.Console.Haskeline.IO
import System.IO
import Data.IORef
import Control.Monad.State
import Data.Maybe(fromMaybe)

data ShellacState = ShellacState {
                        inputState :: InputState,
                        wordBreakChars  :: IORef String,
                        completer :: IORef CompletionFunction,
                        defaultCompleter :: IORef (Maybe (String -> IO [String]))
                    }

initShellacState :: IO ShellacState
initShellacState = do
    wbcsRef <- newIORef filenameWordBreakChars
    complRef <- newIORef (wrapHaskelineCompleter listFiles)
    dcomplRef <- newIORef Nothing
    let completionWrapper = \line -> do
         wbcs <- readIORef wbcsRef
         compl <- readIORef complRef
         dcompl <- readIORef dcomplRef
         wrapShellacCompleter wbcs compl dcompl line
    is <- initializeInput Settings {complete = completionWrapper,
                        autoAddHistory = False,
                        historyFile = Nothing}
    return ShellacState {inputState = is, wordBreakChars = wbcsRef,
                completer = complRef, defaultCompleter = dcomplRef}

queryInputState :: ShellacState -> (InputT IO a) -> IO a
queryInputState = queryInput . inputState

--------------
haskelineBackend :: ShellBackend ShellacState
haskelineBackend = ShBackend {
            initBackend = initShellacState,
            shutdownBackend = closeInput . inputState,
            outputString = \ss -> queryInputState ss . outputter,
            flushOutput = \_ -> hFlush stdout,
            getSingleChar = \ss pre -> queryInputState ss $ getInputChar pre,
            getInput = \ss pre -> queryInputState ss $ getInputLine pre,
            addHistory = \ss line -> queryInputState ss
                    $ modifyHistory $ History.addHistory line,
            setWordBreakChars = \ss -> writeIORef (wordBreakChars ss),
            getWordBreakChars = readIORef . wordBreakChars,
            onCancel = cancelInput . inputState,
            setAttemptedCompletionFunction = \ss -> writeIORef (completer ss),
            setDefaultCompletionFunction = \ss -> writeIORef (defaultCompleter ss),
            completeFilename = \_ -> fmap (map replacement) . listFiles,
            completeUsername = \_ _ -> return [],
            clearHistoryState = \ss -> queryInputState ss $ putHistory History.emptyHistory,
            setMaxHistoryEntries = \ss n -> let
                        stifle = if n < 0 then Nothing else Just n
                        in queryInputState ss $ modifyHistory $ History.stifleHistory stifle,
            getMaxHistoryEntries = \ss -> queryInputState ss
                            $ fmap (fromMaybe (-1) . History.stifleAmount)
                                getHistory,
            readHistory = \ss file -> History.readHistory file
                                >>= queryInputState ss . putHistory,
            writeHistory = \ss file -> queryInputState ss getHistory
                                        >>= History.writeHistory file
            }


outputter :: BackendOutput -> InputT IO ()
outputter (RegularOutput str) = outputStr str
outputter (InfoOutput str) = outputStr str
-- Haskeline has no way to directly write to stderr.
-- (outputStr may or may not go to the tty, depending on whether
-- we're running in interactive or file mode.)
-- So instead, we just use hPutStr and rely on ghc>=6.12's I/O encoding.
outputter (ErrorOutput str) = liftIO $ hPutStr stderr str


wrapShellacCompleter :: String -> CompletionFunction -> Maybe (String -> IO [String])
                            -> CompletionFunc IO
wrapShellacCompleter breakChars f mg (left,right) = do
    let (rword,rleft') = break (`elem` breakChars) left
    let (left', word) = (reverse rleft', reverse rword)
    result <- f (left',word,right)
    completions <- case result of
                        Nothing -> case mg of
                            Nothing -> return []
                            Just g -> g word
                        Just (str,[]) -> return [str]
                        Just (_,alts) -> return alts
    return (rleft', map makeCompletion completions)

-- a hack to avoid adding a trailing space to completed folders.
-- I could go a little further and test whether it corresponds to an
-- actual file.
makeCompletion :: String -> Completion
makeCompletion "" = simpleCompletion ""
makeCompletion s = (simpleCompletion s) {
                isFinished = not (last s `elem` "/\\")
                    }

longestPrefix :: [String] -> String
longestPrefix = foldl1 commonPrefix
    where
        commonPrefix (x:xs) (y:ys) | x==y = x : commonPrefix xs ys
        commonPrefix _ _ = ""

wrapHaskelineCompleter :: (String -> IO [Completion]) -> CompletionFunction
wrapHaskelineCompleter f (_,w,_) = do
    ws <- fmap (map replacement) (f w)
    return $ case ws of
        [] -> Nothing
        _ -> Just (longestPrefix ws,ws)
