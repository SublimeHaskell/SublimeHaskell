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
    , _declarationWhat :: String
    , _nameOfDeclaration :: String
    , _functionType :: Maybe String
    , _typeContext :: Maybe [String]
    , _typeArgs :: Maybe [String]
    }
    deriving (Show)

instance Json.ToJSON DeclarationInfo where
    toJSON (DeclarationInfo (H.SrcLoc _ l c) what name funType ctx args) = Json.object [
        "line" .= l,
        "column" .= c,
        "what" .= what,
        "name" .= name,
        "type" .= funType,
        "context" .= ctx,
        "args" .= args]

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
            , _declarations = concatMap declInfo declarations
            }

-- | Get module name for import
infoOfImport :: H.ImportDecl -> ImportInfo
infoOfImport d = ImportInfo (mname (H.importModule d)) (H.importQualified d) (fmap mname $ H.importAs d) where
    mname (H.ModuleName n) = n

-- | Get the relevant information about of a top-level declaration.
-- Return Nothing if the declaration is not interesting.
declInfo :: H.Decl -> [DeclarationInfo]
declInfo decl = case decl of
    H.TypeSig loc names typeSignature -> map
        (\n -> DeclarationInfo
            loc
            "function"
            (identOfName n)
            (Just $ H.prettyPrint typeSignature)
            Nothing
            Nothing)
        names
    H.TypeDecl loc n args _ -> [DeclarationInfo loc "type" (identOfName n) Nothing Nothing (Just $ map H.prettyPrint args)]
    H.DataDecl loc _ ctx n args _ _ -> [DeclarationInfo loc "data" (identOfName n) Nothing (Just $ map H.prettyPrint ctx) (Just $ map H.prettyPrint args)]
    H.GDataDecl loc _ ctx n args _ _ _ -> [DeclarationInfo loc "data" (identOfName n) Nothing (Just $ map H.prettyPrint ctx) (Just $ map H.prettyPrint args)]
    H.ClassDecl loc ctx n args _ _ -> [DeclarationInfo loc "class" (identOfName n) Nothing (Just $ map H.prettyPrint ctx) (Just $ map H.prettyPrint args)]
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
