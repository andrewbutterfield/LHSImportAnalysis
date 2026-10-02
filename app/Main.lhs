\section{Main Program}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2026

LICENSE: BSD3, see file LICENSE at lhsimport root
\end{verbatim}
\begin{code}
module Main(main) where

import System.Console.Haskeline
import System.IO
import System.Process
import Control.Monad.IO.Class

import Data.Char
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import ReadImports
import ImportAnalysis

import Debugger
\end{code}

\subsection{Version}

\begin{code}
progName :: [Char]     ; progName = "lhsimports"
version :: [Char]      ; version = "0.0.1.0"
name_version :: [Char] ; name_version = progName++" "++version
\end{code}

\subsection{Mainline}

\begin{code}
main :: IO ()
main
  = do putStrLn name_version
       imports <- readImports
       termw <- liftIO getDisplayWidth
       putStrLn $ replicate termw '='
       putStrLn $ prettyImports termw imports
       let closedup = tclose imports
       showClosureChanges termw imports closedup
       let loops = showImportLoops termw closedup
       if null loops 
       then putStrLn "\nNo Import Loops\n"
       else putStrLn ("\nIMPORT LOOPS (!)\n"++loops)
       putStrLn "starting lhsimport TUI..."
       runTUI imports closedup
\end{code}

\begin{code}
getDisplayWidth :: IO Int
getDisplayWidth = do
  system ( "tput cols > " ++ tpc )
  colstxt <- readFile tpc
  let dw = readNat $ trim colstxt
  return $ if dw > 0 then dw else 80
tpc = ".tput_cols"
\end{code}

\begin{code}
trim :: String -> String
trim = ltrim . reverse . ltrim . reverse where ltrim = dropWhile isSpace
readNat :: String -> Int
readNat str
 | null str         =   -1
 | all isDigit str  =   read str
 | otherwise        =   -1
\end{code}

\begin{code}
showClosureChanges :: Int -> ImportMap -> ImportMap -> IO ()
showClosureChanges w imports closedup = do
  putStr "\nClosure: "
  if imports == closedup
  then putStrLn "NO CHANGE"
  else do 
    putStrLn "CHANGED!"
    putStrLn $ reportChanges w imports closedup
\end{code}

\begin{code}
runTUI :: ImportMap -> ImportMap -> IO ()
runTUI original closed = do
  response <- userPrompt "? for help> "
  dispatch original closed $ trim response

userPrompt :: String -> IO String
userPrompt str = putStr str >> hFlush stdout >> getLine

dispatch :: ImportMap -> ImportMap -> String -> IO ()
dispatch orig clsd ""   = runTUI orig clsd
dispatch orig clsd "x"  = putStrLn "Exiting...\n\n"
dispatch orig clsd "?"  = do putStrLn help_text ; runTUI orig clsd
dispatch orig clsd complex = dispatch' orig clsd (words complex)

dispatch' :: ImportMap -> ImportMap -> [String] -> IO ()
dispatch' orig clsd [] = runTUI orig clsd
dispatch' orig clsd (cmd:arg:rest)
  | cmd == "sh" = doShow orig clsd arg
dispatch' orig clsd resp = do putStrLn "unknown command" ; runTUI orig clsd

help_text = unlines
  [ "x - exit this program"
  , "? - show this help text"
  , "sh modname - show original and closed imports"
  ]
\end{code}

\begin{code}
doShow orig clsd arg = do
  case M.lookup arg orig of
    Nothing -> putStrLn ("Module '"++arg++"' not found")
    Just set -> do
      putStrLn ("original("++arg++") = "++show (S.toList set))
  runTUI orig clsd
\end{code}