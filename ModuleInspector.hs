{-# LANGUAGE OverloadedStrings #-}

-- Currently only exports top-level functions with handwritten
-- type signature.
module Main where

import qualified Data.Aeson as Json
import           Data.Aeson ((.=))
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Language.Haskell.Exts as H
import qualified System.Environment as Environment

-- | All the information extracted from a codebase.
data ModuleInfo = ModuleInfo
    { _moduleName :: String
    , _exportList :: Maybe [String]
    , _imports :: [ImportInfo]
    , _declarations :: [DeclarationInfo]
    }
    deriving (Show)

instance Json.ToJSON ModuleInfo where
    toJSON info = Json.object
        [ "moduleName" .= _moduleName info
        , "exportList" .= Json.Null
        , "imports" .= _imports info
        , "declarations" .= _declarations info
        ]

-- | Information about import
data ImportInfo = ImportInfo
    { _importName :: String
    , _importQualified :: Bool
    , _importAs :: Maybe String
    }
    deriving (Show)

instance Json.ToJSON ImportInfo where
    toJSON info = Json.object
        [ "importName" .= _importName info
        , "qualified" .= _importQualified info
        , "as" .= _importAs info
        ]

-- | Information about a single type or function declaration.
data DeclarationInfo = DeclarationInfo
    { _declLocation :: H.SrcLoc
    , _nameOfDeclaration :: String
    , _typeInfo :: String
    }
    deriving (Show)

instance Json.ToJSON DeclarationInfo where
    toJSON (DeclarationInfo (H.SrcLoc _ l c) name typeInfo) = Json.object
        [ "info" .= typeInfo
        , "identifier" .= name
        , "line" .= l
        , "column" .= c
        ]

-- | Process a single file's contents.
analyzeModule :: String -> Either String ModuleInfo
analyzeModule source = case H.parseFileContents source of
    H.ParseFailed location reason -> Left
        ("Parse failed at " ++ show location ++ ": " ++ reason)
    H.ParseOk (H.Module _ (H.ModuleName moduleName) _ _ _ imports declarations) -> Right
        ModuleInfo
            { _moduleName = moduleName
            , _exportList = Nothing
            , _imports = map infoOfImport imports
            , _declarations = concatMap nameOfDecl declarations
            }

-- | Get module name for import
infoOfImport :: H.ImportDecl -> ImportInfo
infoOfImport d = ImportInfo (mname (H.importModule d)) (H.importQualified d) (fmap mname $ H.importAs d) where
    mname (H.ModuleName n) = n

-- | Get the relevant information about of a top-level declaration.
-- Return Nothing if the declaration is not interesting.
nameOfDecl :: H.Decl -> [DeclarationInfo]
nameOfDecl decl = case decl of
    H.TypeSig loc names typeSignature -> map
        (\n -> DeclarationInfo
            loc
            (identOfName n)
            (":: " ++ H.prettyPrint typeSignature))
        names
    H.TypeDecl loc n _ _ -> [DeclarationInfo loc (identOfName n) "(type)"]
    H.DataDecl loc _ _ n _ _ _ -> [DeclarationInfo loc (identOfName n) "(data)"]
    H.ClassDecl loc _ n _ _ _ -> [DeclarationInfo loc (identOfName n) "(class)"]
    _ -> []

identOfName :: H.Name -> String
identOfName name = case name of
    H.Ident s -> s
    H.Symbol s -> s

-- | Analyze the specified file and dump the collected information to stdout.
main :: IO ()
main = do
    programName <- Environment.getProgName
    args <- Environment.getArgs
    case args of
        [filename] -> do
            source <- readFile filename
            let output = case analyzeModule source of
                    Left excuse -> Json.toJSON $ Json.object ["error" .= excuse]
                    Right info -> Json.toJSON info
            T.putStrLn . T.decodeUtf8 . Json.encode $ output
        _ -> putStrLn ("Usage: " ++ programName ++ " FILENAME")
