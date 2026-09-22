\section{Import Analysis}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2026

LICENSE: BSD3, see file LICENSE at lhsimport root
\end{verbatim}
\begin{code}
module ImportAnalysis (
  tclose
)
where

--import System.Directory 
--import System.FilePath

import Data.Set (Set)
import qualified Data.Set as S
-- import Data.Map (Map)
import qualified Data.Map as M
import Data.List ((\\),delete,isPrefixOf)

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
tclose :: ImportMap -> ImportMap
tclose rho  =  process rho (M.keys rho)

process :: ImportMap -> [ModName] -> ImportMap
process rho [] = rho
process rho wdom 
  = let
      lookups = map (slookup rho) $ pdbg "wdom" wdom
      newmaps = M.fromList $ zip wdom lookups
      nu = M.unionWith S.union rho newmaps
      wdom' = M.keys nu \\ wdom 
    in process nu wdom'

slookup :: ImportMap -> ModName -> Set ModName
slookup rho n 
  = case M.lookup n rho of
      Nothing        ->  S.empty
      Just modnames  ->  modnames
\end{code}

