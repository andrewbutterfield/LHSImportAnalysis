\section{Main Program}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2026

LICENSE: BSD3, see file LICENSE at lhsimport root
\end{verbatim}
\begin{code}
module Main(main) where

import ReadImports
import ImportAnalysis

--import Debug.Trace
--dbg :: Show a => [Char] -> a -> a  ; dbg msg x = trace (msg++show x) x
--pdbg :: Show a => [Char] -> a -> a ; pdbg nm x = dbg ('@':nm++":\n") x
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
       putStrLn ("imports: "++show imports)
       let closedup = tclose imports
       putStrLn ("\nclosedup: "++show closedup)
       if imports == closedup
       then putStrLn "NO CHANGE"
       else putStrLn "CHANGED!"
\end{code}
