\section{Main Program}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2026

LICENSE: BSD3, see file LICENSE at lhsimport root
\end{verbatim}
\begin{code}
module Main(main) where

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
       putStrLn $ prettyImports 80 imports
       let closedup = tclose imports
       if imports == closedup
       then putStrLn "Closure: NO CHANGE"
       else putStrLn "Closure: CHANGED!"
\end{code}
