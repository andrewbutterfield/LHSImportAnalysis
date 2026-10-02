\section{Import Analysis}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2026

LICENSE: BSD3, see file LICENSE at lhsimport root
\end{verbatim}
\begin{code}
module ImportAnalysis (
  tclose
, reportChanges
, showImportLoops
)
where

--import System.Directory 
--import System.FilePath

import Data.Set (Set)
import qualified Data.Set as S
-- import Data.Map (Map)
import qualified Data.Map as M
import Data.List ((\\),delete,isPrefixOf,nub,intercalate)

import ReadImports

import Debugger
\end{code}

We start with basic sanity checking: no loops in import graph.

\subsection{Transitive Closure}

Given a mapping $\rho : N \rightarrow \mathcal{P}~N$,
we can compute the transitive closure $\rho_n$ w.r.t. $n$,
as follows.
We assume we have current $\rho$ of the form:
\begin{eqnarray*}
   n_1 &\mapsto& n_{11}, \dots, n_{1j}, \dots, n_{1k_1}
\\ n_i &\mapsto& n_{i1}, \dots, n_{ij}, \dots, n_{ik_i}
\\ n_N &\mapsto& n_{N1}, \dots, n_{Nj}, \dots, n_{Nk_N}
\end{eqnarray*}
We take $n_1,\dots,n_i,\dots,n_N$ as our \emph{working domain}.
We then lookup each $n_{ij}$, from the working domain, in $\rho$ to obtain:
\begin{eqnarray*}
   n_1 &\mapsto& \rho(n_{11}), \dots, \rho(n_{1j}), \dots, \rho(n_{1k_1})
\\ n_i &\mapsto& \rho(n_{i1}), \dots, \rho(n_{ij}), \dots, \rho(n_{ik_i})
\\ n_N &\mapsto& \rho(n_{N1}), \dots, \rho(n_{Nj}), \dots, \rho(n_{Nk_N})
\end{eqnarray*}
Next we gather them all together as a new mapping $\nu$.
Note that $\nu \supseteq \rho$.
\begin{eqnarray*}
   n_1 &\mapsto& 
  \rho(n_1) \cup 
  \rho(n_{11})\cup,\dots,\cup\rho(n_{1j})\cup,\dots,\cup\rho(n_{1k_1})
\\ n_i &\mapsto&
   \rho(n_i) \cup 
   \rho(n_{i1})\cup,\dots,\cup\rho(n_{ij})\cup,\dots,\cup\rho(n_{ik_i})
\\ n_N &\mapsto&
   \rho(n_N) \cup 
   \rho(n_{N1})\cup,\dots,\cup\rho(n_{Nj})\cup,\dots,\cup\rho(n_{Nk_N})
\end{eqnarray*}
We now define the next working domain to be the domain of $\nu$,
minus the working domain just used to produce $\nu$:
$$ \nu \setminus \{n_1,\dots,n_i,\dots,n_N\} $$
Rinse and repeat until the regenerated working domain becomes empty.


\begin{code}
slookup :: ImportMap -> ModName -> Set ModName
slookup rho n 
  = case M.lookup n rho of
      Nothing        ->  S.empty
      Just modnames  ->  modnames
\end{code}

\begin{code}
tclose :: ImportMap -> ImportMap
tclose rho  =  tcl (allnodes rho) $ totalise rho
tcl :: [ModName] -> ImportMap -> ImportMap
tcl [] rho = rho
tcl (n:ns) rho
  = let 
      rho_n = slookup rho n 
      rho2_n = S.unions $ map (slookup rho) $ S.toList rho_n
      rho'_n = rho_n `S.union` rho2_n
      rho' = M.insert n rho'_n rho
    in if rho'_n == rho_n 
        then tcl ns rho
        else tcl (n:ns) rho'
\end{code}

\subsection{Change Reporting}

\begin{code}
reportChanges :: Int -> ImportMap -> ImportMap -> String
reportChanges w original closed 
  = showDelta w [] (M.assocs original) (M.assocs closed)
  
showDelta :: Int -> [String] 
          -> [(ModName,Set ModName)] -> [(ModName,Set ModName)] 
          -> String
showDelta _ stroper [] [] = unlines $ reverse stroper
showDelta _ stroper [] closed 
  = unlines $ reverse ("Extra modules in 2nd":stroper) 
showDelta _ stroper original []  
  = unlines $ reverse ("Extra modules in 1st":stroper)
showDelta w stroper orig@((n1,set1):rest1) clsd@((n2,set2):rest2)
  | n1 < n2       =  showDelta w (only1 n1:stroper)        rest1 clsd
  | n1 > n2       =  showDelta w (only2 n2:stroper)        orig  rest2
  | set1 == set2  =  showDelta w stroper                   rest1 rest2
  | otherwise     =  showDelta w (sdiff n1 set1 set2:stroper) rest1 rest2
  where
   only1 n        =  n++": original only!"
   only2 n        =  n++": closed only!"
   sdiff n s1 s2  
     =  n++" += "
         ++ (show $ length closedXtra)
        --  ++(intercalate " " $ ppImportedNames (w-(length n+4)) closedXtra)
         ++ wierd
     where 
       closedXtra = S.toList (s2 S.\\ s1)
       importXtra = S.toList (s1 S.\\ s2)
       wierd =
         if null importXtra then ""
         else "\nWIERD: import -= "++show importXtra
\end{code}

\subsection{Import Cycle Reporting}

\begin{code}
showImportLoops :: Int -> ImportMap -> String
showImportLoops w closedup  =  showLoops w [] $ M.assocs closedup

showLoops w spool [] = unlines $ reverse spool
showLoops w spool ((n,set):rest)
  | n `S.member` set  =  showLoops w (dispLoop : spool) rest
  | otherwise         =  showLoops w                   spool  rest
  where 
    list = S.toList set
    loopnames = intercalate " " $ ppImportedNames (w-(length n+6)) list
    dispLoop = n ++ " loops " ++ loopnames
\end{code}

\subsection{Analysis Tests}


\begin{code}
[a,b,c,d,e] = ["A","B","C","D","E"]
mlet :: ModName -> [ModName] -> ImportMap 
mlet x ys = M.fromList [(x,S.fromList ys)]
mnull x = mlet x []
mrng m = concat $ map S.toList $ M.elems m
mmap =  M.unionsWith S.union
totalise m = mmap ( m : map mnull (mrng m) )
allnodes m = nub (M.keys m ++ mrng m)

a2b = (a,S.fromList [b])
b2c = (b,S.fromList [c])
c2d = (c,S.fromList [d])

b2cd = (b,S.fromList [c,d])

ma2b2c = M.fromList [a2b,b2c]

matod = M.fromList [a2b,b2c,c2d]

mb2cd = M.fromList [a2b,b2cd]

ex0 = mmap [mlet a [b],mlet b [c,d],mlet d [e]]

ex1 = mmap [mlet a [b],mlet b [c,d],mlet c [],mlet d [e],mlet e []]

ex2 = mmap [mlet a [b],mlet b [a,c,d],mlet c [],mlet d [e],mlet e []]

ex3 = mmap [mlet a [b],mlet b [c,d],mlet c [],mlet d [e],mlet e [a]]

ex4 = mmap [mlet a [b],mlet b [c],mlet c [d],mlet d [b,e]]

ex5 = mmap [mlet a [b],mlet e [d],mlet c [a],mlet d [e],mlet b [c]]
\end{code}

