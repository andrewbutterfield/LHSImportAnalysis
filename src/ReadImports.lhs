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
--import Debug.Trace
--dbg msg x = trace (msg++show x) x
\end{code}


\begin{code}
readImports :: IO [(String,[String])]
readImports = do
  dirOk <- checkDirectoryStructure
  if dirOk  -- app/Main.lhs exists, src/ exists
  then do 
    putStrLn "readImports NYI"
    return []
  else fail "invalid directory structure"
\end{code}


\begin{code}
checkDirectoryStructure :: IO Bool
checkDirectoryStructure = do
  has_app <- doesDirectoryExist "app"
  putStrLn ("'app' directory present? "++show has_app) 
  has_Main <- doesFileExist ("app" </> "Main" <.> "lhs")
  putStrLn ("'app/Main.lhs' directory present? "++show has_Main) 
  has_src <- doesDirectoryExist "src"
  putStrLn ("'src' directory present? "++show has_src) 
  return $ and [has_app,has_Main,has_src]
\end{code}

\begin{code}
-- code here
\end{code}

\begin{code}
-- code here
\end{code}

\begin{code}
-- code here
\end{code}

\begin{code}
-- code here
\end{code}
