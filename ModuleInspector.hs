module ModuleInspector where

import           Control.Arrow (first, second)
import           Control.Monad (forM_)
import qualified Data.Either as Either
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Generics.Uniplate.Data as Uniplate
import qualified Language.Haskell.Exts as H
import qualified System.Environment as Environment
import qualified System.FilePath as FilePath

-- | All the information extracted from a codebase.
data ModuleInfo = ModuleInfo
    { _moduleName :: String
    , _exportList :: Maybe [String]
    , _importList :: [String]
    , _declarations :: [DeclarationInfo]
    }
    deriving (Show)

data DeclarationInfo = DeclarationInfo
    { _nameOfDeclaration :: String
    , _typeInfo :: String
    }
    deriving (Show)

-- | A fully-qualified name.
type QualifiedName = String

-- | Process a single file's contents.
analyzeModule :: String -> Either String ModuleInfo
analyzeModule source = case H.parseFileContents source of
    H.ParseFailed location reason -> Left
        ("Parse failed at " ++ show location ++ ": " ++ reason)
    H.ParseOk (H.Module _ (H.ModuleName moduleName) _ _ exports imports declarations) -> Right $
        ModuleInfo
            { _moduleName = moduleName
            , _exportList = Nothing
            , _importList = []
            , _declarations = concatMap nameOfDecl declarations
            }

-- | Get the relevant information about of a top-level declaration.
-- Return Nothing if the declaration is not interesting.
nameOfDecl :: H.Decl -> [DeclarationInfo]
nameOfDecl decl = case decl of
    H.TypeSig _ names typeSignature -> map
        (\n -> DeclarationInfo
            (identOfName n)
            (":: " ++ H.prettyPrint typeSignature))
        names
    H.TypeDecl _ n _ _ -> [DeclarationInfo (identOfName n) "(type)"]
    H.DataDecl _ _ _ n _ _ _ -> [DeclarationInfo (identOfName n) "(data)"]
    H.ClassDecl _ _ n _ _ _ -> [DeclarationInfo (identOfName n) "(class)"]
    _ -> []

identOfName :: H.Name -> String
identOfName name = case name of
    H.Ident s -> s
    H.Symbol s -> s

dumpModuleInfo :: ModuleInfo -> IO ()
dumpModuleInfo info = do
    putStrLn ("Module: " ++ _moduleName info)
    putStrLn ""
    putStrLn "Declarations:"
    forM_ (_declarations info) $ \(DeclarationInfo name typeInfo) ->
        putStrLn (name ++ " " ++ typeInfo)

-- | Analyze the specified file and dump the collected information to stdout.
main :: IO ()
main = do
    programName <- Environment.getProgName
    args <- Environment.getArgs
    case args of
        [filename] -> do
            source <- readFile filename
            case analyzeModule source of
                Left excuse -> putStrLn ("Error: " ++ excuse)
                Right info -> dumpModuleInfo info
        _ -> putStrLn ("Usage: " ++ programName ++ " FILENAME")
