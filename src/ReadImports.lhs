\section{Read LHS Imports}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2026

LICENSE: BSD3, see file LICENSE at lhsimport root
\end{verbatim}
\begin{code}
module ReadImports (
  readImports
)
where

import System.Directory 
import System.FilePath

import ParseModuleImports

--import Debug.Trace
--dbg msg x = trace (msg++show x) x
\end{code}


\begin{code}
readImports :: IO [(String,[String])]
readImports = do
  dirOk <- checkDirectoryStructure
  if dirOk  -- app/Main.lhs exists, src/ exists
  then do 
    mainImports <- readModule main_path
    putStrLn ("main imports: "++show mainImports)
    putStrLn "readImports NYfI"
    return [mainImports]
  else fail "invalid directory structure"
\end{code}


\begin{code}
checkDirectoryStructure :: IO Bool
checkDirectoryStructure = do
  has_app <- doesDirectoryExist "app"
  putStrLn ("'app' directory present? "++show has_app) 
  has_Main <- doesFileExist main_path
  putStrLn ("'app/Main.lhs' directory present? "++show has_Main) 
  has_src <- doesDirectoryExist "src"
  putStrLn ("'src' directory present? "++show has_src) 
  return $ and [has_app,has_Main,has_src]

main_path :: FilePath
main_path = "app" </> "Main" <.> "lhs"
\end{code}

\begin{code}
readModule :: String -> IO (String,[String])
readModule path = do
  good_path <- doesFileExist path
  if good_path then do
    putStrLn ("Reading Module at "++path)
    modtext <- readFile path
    return $ parseModule modtext
  else do
    putStrLn ("Path "++path++" does not exist")
    return ("!",[])
\end{code}

\begin{code}
-- code here
\end{code}

\begin{code}
-- code here
\end{code}
