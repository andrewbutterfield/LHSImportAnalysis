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
, prettyImports
)
where

import System.Directory 
import System.FilePath

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.List (isPrefixOf,intercalate)

import Debugger
\end{code}

\subsection{Module Import Types}

\begin{code}
type ModName = String
type ImportMap  =  Map ModName (Set ModName)
\end{code}

\subsection{Identifying Interesting Imports}

We assume the program mainline lives in the ``app'' folder,
and that we have the following source folders as default:
\begin{code}
defaultSrcDirs :: [String]
defaultSrcDirs = ["builtin","src"]  -- for now
\end{code}

We are not interested in standard Haskell modules, 
identifiable as having module names that are/start with any of the following:
\begin{code}
standardModules :: Set ModName
standardModules = S.fromList ["Control","Data","System","Test","Debug"]

isStandard :: ModName -> Bool
isStandard modname = any (`isPrefixOf` modname) standardModules

removeStd :: [ModName] -> [ModName]
removeStd = filter (not . isStandard)
\end{code}

\newpage
\subsection{Reading Imports}


\begin{code}
readImports :: IO ImportMap
readImports = do
  (dirOk,srcdirs) <- checkDirectoryStructure
  if dirOk && not (null srcdirs) -- app/Main.lhs exists, source-dirs exists
  then do 
    (_,mainImports) <- readModule main_path
    let projectImports = removeStd mainImports
    let mainRoot = M.singleton "Main" $ S.fromList projectImports
    putStrLn $ unlines
      [ "Building Import Map"
      , "Source Directories: " ++ show srcdirs
      , "Main Imports: " ++ show mainImports
      , "Project Imports: "  ++ show projectImports
      ]
    buildImportMap srcdirs mainRoot projectImports
  else fail "invalid directory structure"
\end{code}


\begin{code}
checkDirectoryStructure :: IO (Bool,[String])
checkDirectoryStructure = do
  has_app <- doesDirectoryExist "app"
  putStrLn ("'app' directory present? "++show has_app) 
  has_Main <- doesFileExist main_path
  putStrLn ("'app/Main.lhs' directory present? "++show has_Main) 
  available_src_dirs <- availableSourceDirectories defaultSrcDirs
  putStrLn ("Available source directories: "++show available_src_dirs) 
  return (and [has_app,has_Main],available_src_dirs)

availableSourceDirectories :: [String] -> IO [String]
availableSourceDirectories [] = return []
availableSourceDirectories (d:ds) = do
  has_d <- doesDirectoryExist d
  avail_ds <- availableSourceDirectories ds
  if has_d then return (d:avail_ds) else return avail_ds

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
  else return ("",[])
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
buildImportMap :: [String] -> ImportMap -> [ModName] -> IO ImportMap
buildImportMap _ importMap [] = return importMap
buildImportMap sdirs importMap (importName:rest)
  | isStandard importName            =  buildImportMap sdirs importMap rest
  | importName `M.member` importMap  =  buildImportMap sdirs importMap rest
  | otherwise = do -- importName is unseen so far, non-standard 
    (importMap',more_imports) <- getImportedModule importName importMap sdirs
    buildImportMap sdirs importMap' (more_imports++rest)
\end{code}

To get an imported module, we need to search the available source directories.
\begin{code}
getImportedModule :: ModName -> ImportMap -> [String] 
                  -> IO (ImportMap,[ModName])
getImportedModule  importName importMap []  = return (importMap,[])
getImportedModule  importName importMap (sdir:sdirs) = do
  (ok,importMap1,more1) <- addImportedModule sdir importName importMap
  if ok then do
    let path = mkpathname sdir importName
    (modnm,imports) <- readModule path
    let prjImports = removeStd imports
    let importMap' 
          = M.insertWith S.union modnm (S.fromList prjImports) importMap
    return (importMap',prjImports)
  else getImportedModule  importName importMap sdirs
\end{code}

\begin{code}
addImportedModule :: String -> ModName -> ImportMap 
                  -> IO (Bool,ImportMap,[ModName])
addImportedModule srcdir importName importMap = do
  let path = mkpathname srcdir importName
  (modnm,imports) <- readModule path
  if null modnm then return (False,importMap,[])
  else do
    let prjImports = removeStd imports
    let importMap' 
          = M.insertWith S.union modnm (S.fromList prjImports) importMap
    return (True,importMap',prjImports)
\end{code}

\begin{code}
mkpathname :: String -> FilePath -> FilePath
mkpathname srcdir path = srcdir </> fix path <.> "lhs"

fix :: String -> String
fix ""  =  ""
fix (c:cs)
  | c == '.'   = '/' : fix cs
  | otherwise  =  c  : fix cs
\end{code}

\newpage
\subsection{Displaying Imports}


\begin{code}
prettyImports :: Int -> ImportMap -> String
prettyImports _ m = concat $ map ppModule (M.assocs m)

ppModule :: (ModName,Set ModName) -> String
ppModule (n,imps)
  | S.null imps = "\n"++n++" - no local imports."
  | otherwise = unlines 
    [ ""
    , n 
    , " <-- " ++ (intercalate " ; " $ S.toList imps)
    ]
\end{code}
 