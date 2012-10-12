{-# LANGUAGE OverloadedStrings #-}

-- Currently only exports top-level functions with handwritten
-- type signature.
module Main where

import qualified Data.Aeson as Json
import           Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Language.Haskell.Exts as H
import qualified System.Environment as Environment

-- | All the information extracted from a codebase.
data ModuleInfo = ModuleInfo
    { _moduleName :: String
    , _exportList :: Maybe [String]
    , _importList :: [String]
    , _declarations :: [DeclarationInfo]
    }
    deriving (Show)

instance Json.ToJSON ModuleInfo where
    toJSON info = Json.object
        [ "moduleName" .= _moduleName info
        , "exportList" .= Json.Null
        , "importList" .= _importList info
        , "declarations" .= _declarations info
        ]

-- | Information about a single type or function declaration.
data DeclarationInfo = DeclarationInfo
    { _nameOfDeclaration :: String
    , _typeInfo :: String
    }
    deriving (Show)

instance Json.ToJSON DeclarationInfo where
    toJSON (DeclarationInfo name typeInfo) = Json.object
        [ "info" .= typeInfo
        , "identifier" .= name
        ]

-- | Process a single file's contents.
analyzeModule :: String -> Either String ModuleInfo
analyzeModule source = case H.parseFileContents source of
    H.ParseFailed location reason -> Left
        ("Parse failed at " ++ show location ++ ": " ++ reason)
    H.ParseOk (H.Module _ (H.ModuleName moduleName) _ _ exports imports declarations) -> Right $
        ModuleInfo
            { _moduleName = moduleName
            , _exportList = Nothing
            , _importList = map nameOfModule imports
            , _declarations = concatMap nameOfDecl declarations
            }

-- | Get module name for import
nameOfModule :: H.ImportDecl -> String
nameOfModule = (\(H.ModuleName n) -> n) . H.importModule

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
            LazyByteString.putStrLn . Json.encode $ output
        _ -> putStrLn ("Usage: " ++ programName ++ " FILENAME")
