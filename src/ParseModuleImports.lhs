\section{Parse LHS Module Imports}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2026

LICENSE: BSD3, see file LICENSE at lhsimport root
\end{verbatim}
\begin{code}
module ParseModuleImports (
  parseModule
)
where

--import Debug.Trace
--dbg msg x = trace (msg++show x) x
\end{code}

\begin{code}
parseModule :: String -> (String,[String])
parseModule = fuse . map lineParse . map words . lines
\end{code}

We are interested in the following lines:
\begin{verbatim}
module MName1.MName2. .. .MNameK .....
import MName1.MName2. .. .MNameK
\end{verbatim}
We ignore \verb"import" lines with the \verb"qualified" keyword.

\begin{code}
lineParse :: [String] -> (String,[String])
lineParse ("module":modName:_)              =  (modName,[])
lineParse ("import":"qualified":modName:_)  =  ("",[modName])
lineParse ("import":modName:_)              =  ("",[modName])
lineParse _ = ("",[])

fuse :: [(String,[String])] -> (String,[String])
fuse []                       = ("",[])
fuse [kns]                    = kns
fuse ((n1,ms1):(n2,ms2):kns)  = fuse ((n1++n2,ms1++ms2):kns)
\end{code}

