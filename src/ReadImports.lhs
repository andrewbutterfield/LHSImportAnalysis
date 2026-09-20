\section{Read LHS Imports}
\begin{verbatim}
Copyright  Andrew Buttefield (c) 2026

LICENSE: BSD3, see file LICENSE at lhsimport root
\end{verbatim}
\begin{code}
module ReadImports (
  ModName
, ImportMap
, readImports
, parseModule
)
where

import System.Directory 
import System.FilePath

import Data.Set(Set)
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.Map as M

--import ParseModuleImports

--import Debug.Trace
--dbg msg x = trace (msg++show x) x
\end{code}

\newpage
\subsection{Reading Imports}


\begin{code}
type ModName = String
type ImportMap  =  Map ModName (Set ModName)
\end{code}


\begin{code}
readImports :: IO ImportMap
readImports = do
  dirOk <- checkDirectoryStructure
  if dirOk  -- app/Main.lhs exists, src/ exists
  then do 
    (_,mainImports) <- readModule main_path
    let mainRoot = M.singleton "Main" $ S.fromList mainImports
    buildImportMap mainRoot mainImports
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
    return ("",[])
\end{code}

\newpage
\subsection{Parsing Modules}

\begin{code}
parseModule :: String -> (String,[String])
parseModule = fuse . map lineParse . map words . lines
\end{code}

We are interested in the the module names referenced 
in \texttt{module} and \texttt{import} lines.
\begin{code}
lineParse :: [String] -> (String,[String])
lineParse ("module":modName:_)              =  (modName,[])
lineParse ("import":"qualified":modName:_)  =  ("",[modName])
lineParse ("import":modName:_)              =  ("",[modName])
lineParse _                                 =  ("",[])
\end{code}

This gathers everything up
\begin{code}
fuse :: [(String,[String])] -> (String,[String])
fuse []                       = ("",[])
fuse [kns]                    = kns
fuse ((n1,ms1):(n2,ms2):kns)  = fuse ((nfuse n1 n2,ms1++ms2):kns)
\end{code}

The following is purely defensive, in case ``module'' is mentioned elsewhere,
e.g., in comments.
\begin{code}
nfuse :: String -> String -> String
nfuse n1 "" = n1
nfuse "" n2 = n2
nfuse n1 n2 = n1 ++ '|':n2 -- shouldn't really happen
\end{code}


\begin{code}
buildImportMap :: ImportMap -> [String] -> IO ImportMap
buildImportMap importMap [] = return importMap
buildImportMap importMap (importName:rest)
  | importName `M.member` importMap  =  buildImportMap importMap rest
  | otherwise = do
      let path = mkpathname importName
      (modnm,imports) <- readModule path
      let importMap' = M.insertWith S.union modnm (S.fromList imports) importMap
      buildImportMap importMap' (imports++rest)
\end{code}

\begin{code}
mkpathname :: FilePath -> FilePath
mkpathname path = "src" </> fix path <.> "lhs"

fix :: String -> String
fix ""  =  ""
fix (c:cs)
  | c == '.'   = '/' : fix cs
  | otherwise  =  c  : fix cs
\end{code}
