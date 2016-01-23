{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Exception as E
import Control.Monad (forM)

import qualified Data.Aeson as Json
import           Data.Aeson ((.=))
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.List (intercalate)
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Language.Haskell.Exts as H
import qualified System.Environment as Environment
import qualified System.Directory as Dir
import System.IO

import qualified Name (Name, getOccString, occNameString)
import qualified Module (moduleNameString)
import qualified SrcLoc as Loc
import qualified HsDecls
import qualified HsBinds
import qualified Documentation.Haddock as Doc

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
    , _declDocs :: Maybe String
    }
    deriving (Show)

instance Json.ToJSON DeclarationInfo where
    toJSON (DeclarationInfo (H.SrcLoc _ l c) what name funType ctx args docs) = Json.object [
        "line" .= l,
        "column" .= c,
        "what" .= what,
        "name" .= name,
        "type" .= funType,
        "context" .= ctx,
        "args" .= args,
        "docs" .= docs]

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
            , _declarations = collectDeclInfo declarations
            }

-- | Get module name for import
infoOfImport :: H.ImportDecl -> ImportInfo
infoOfImport d = ImportInfo (mname (H.importModule d)) (H.importQualified d) (fmap mname $ H.importAs d) where
    mname (H.ModuleName n) = n

-- | Collect information about declarations
-- Prefer info from declInfo and use defInfo only if declInfo returns nothing about declaration
collectDeclInfo :: [H.Decl] -> [DeclarationInfo]
collectDeclInfo decls = infos ++ defs where
    infos = concatMap declInfo decls
    names = map _nameOfDeclaration infos
    defs = filter noInfo $ concatMap defInfo decls
    noInfo :: DeclarationInfo -> Bool
    noInfo d = _nameOfDeclaration d `notElem` names


-- | Get the relevant information about of a top-level declaration.
declInfo :: H.Decl -> [DeclarationInfo]
declInfo decl = case decl of
    H.TypeSig loc names typeSignature -> map
        (\n -> DeclarationInfo
            loc
            "function"
            (identOfName n)
            (Just $ H.prettyPrint typeSignature)
            Nothing
            Nothing
            Nothing)
        names
    H.TypeDecl loc n args _ -> [DeclarationInfo loc "type" (identOfName n) Nothing Nothing (Just $ map H.prettyPrint args) Nothing]
    H.DataDecl loc dataOrNew ctx n args _ _ -> [DeclarationInfo loc (toStr dataOrNew) (identOfName n) Nothing (Just $ map H.prettyPrint ctx) (Just $ map H.prettyPrint args) Nothing]
    H.GDataDecl loc dataOrNew ctx n args _ _ _ -> [DeclarationInfo loc (toStr dataOrNew) (identOfName n) Nothing (Just $ map H.prettyPrint ctx) (Just $ map H.prettyPrint args) Nothing]
    H.ClassDecl loc ctx n args _ _ -> [DeclarationInfo loc "class" (identOfName n) Nothing (Just $ map H.prettyPrint ctx) (Just $ map H.prettyPrint args) Nothing]
    _ -> []
    where
        toStr :: H.DataOrNew -> String
        toStr H.DataType = "data"
        toStr H.NewType = "newtype"

-- | Get function declarations by pattern bindings
defInfo :: H.Decl -> [DeclarationInfo]
defInfo (H.FunBind []) = []
defInfo (H.FunBind (H.Match loc n _ _ _ _ : _)) = [DeclarationInfo loc "function" (identOfName n) Nothing Nothing Nothing Nothing]
defInfo (H.PatBind loc pat _ _ _) = map (\name -> DeclarationInfo loc "function" (identOfName name) Nothing Nothing Nothing Nothing) $ names pat where
    names :: H.Pat -> [H.Name]
    names (H.PVar n) = [n]
    names (H.PNeg n) = names n
    names (H.PNPlusK n _) = [n]
    names (H.PInfixApp l _ r) = names l ++ names r
    names (H.PApp _ ns) = concatMap names ns
    names (H.PTuple _ ns) = concatMap names ns
    names (H.PList ns) = concatMap names ns
    names (H.PParen n) = names n
    names (H.PRec _ pf) = concatMap fieldNames pf
    names (H.PAsPat n ns) = n : names ns
    names H.PWildCard = []
    names (H.PIrrPat n) = names n
    names (H.PatTypeSig _ n _) = names n
    names (H.PViewPat _ n) = names n
    names (H.PBangPat n) = names n
    names _ = []

    fieldNames :: H.PatField -> [H.Name]
    fieldNames (H.PFieldPat _ n) = names n
    fieldNames (H.PFieldPun n) = [n]
    fieldNames H.PFieldWildcard = []
defInfo _ = []

identOfName :: H.Name -> String
identOfName name = case name of
    H.Ident s -> s
    H.Symbol s -> s

-- | Get Map from declaration name to its documentation
documentationMap :: Doc.Interface -> M.Map String String
documentationMap iface = M.fromList $ concatMap toDoc $ Doc.ifaceExportItems iface where
    toDoc :: Doc.ExportItem Name.Name -> [(String, String)]
    toDoc Doc.ExportDecl{ Doc.expItemDecl = decl, Doc.expItemMbDoc = docs } = maybe [] (zip (extractNames decl) . repeat) $ extractDocs docs
    toDoc _ = []

    extractNames :: HsDecls.LHsDecl Name.Name -> [String]
    extractNames (Loc.L _ d) = case d of
        HsDecls.TyClD ty -> [locatedName $ HsDecls.tcdLName ty]
        HsDecls.SigD sig -> case sig of
            HsBinds.TypeSig names _ _ -> map locatedName names
            HsBinds.GenericSig names _ -> map locatedName names
            _ -> []
        _ -> []

    extractDocs :: Doc.DocForDecl Name.Name -> Maybe String
    extractDocs (mbDoc, _) = fmap printDoc $ Doc.documentationDoc mbDoc where
        printDoc :: Doc.Doc Name.Name -> String
        printDoc Doc.DocEmpty = ""
        printDoc (Doc.DocAppend l r) = printDoc l ++ printDoc r
        printDoc (Doc.DocString s) = s
        printDoc (Doc.DocParagraph p) = printDoc p
        printDoc (Doc.DocIdentifier i) = Name.getOccString i
        printDoc (Doc.DocIdentifierUnchecked (m, i)) = Module.moduleNameString m ++ "." ++ Name.occNameString i
        printDoc (Doc.DocModule m) = m
        printDoc (Doc.DocWarning w) = printDoc w
        printDoc (Doc.DocEmphasis e) = printDoc e
        printDoc (Doc.DocMonospaced m) = printDoc m
        printDoc (Doc.DocUnorderedList lst) = concatMap printDoc lst -- Is this right?
        printDoc (Doc.DocOrderedList lst) = concatMap printDoc lst -- And this
        printDoc (Doc.DocDefList defs) = concatMap (\(l, r) -> printDoc l ++ " = " ++ printDoc r) defs -- ?
        printDoc (Doc.DocCodeBlock code) = printDoc code
        printDoc (Doc.DocAName a) = a
        printDoc (Doc.DocExamples exs) = unlines $ map showExample exs where
            showExample (Doc.Example expr results) = expr ++ " => " ++ intercalate ", " results
        -- TODO these two are not in haddock-2.11.1 (HP 2012.4)
        -- but exist in 2.13.*
        -- printDoc (Doc.DocHyperlink link) = fromMaybe (Doc.hyperlinkUrl link) (Doc.hyperlinkLabel link)
        -- printDoc (Doc.DocProperty prop) = prop
        -- TODO This one only works with haddock 2.13, but has a different type in 2.14
        -- printDoc (Doc.DocPic pic) = pic
        -- Catch all unsupported ones
        printDoc _ = "[unsupported-by-extractDocs]" -- TODO

    locatedName :: Loc.Located Name.Name -> String
    locatedName (Loc.L _ nm) = Name.getOccString nm

-- | Adds documentation to declaration
addDoc :: M.Map String String -> DeclarationInfo -> DeclarationInfo
addDoc docsMap decl = decl { _declDocs = M.lookup (_nameOfDeclaration decl) docsMap }

-- | Adds documentations to ModuleInfo
addDocs :: M.Map String String -> ModuleInfo -> ModuleInfo
addDocs docsMap info = info { _declarations = map (addDoc docsMap) (_declarations info) }

-- | Analyze the specified file and dump the collected information to stdout.
main :: IO ()
main = do
    hSetEncoding stdout utf8
    programName <- Environment.getProgName
    args <- Environment.getArgs
    case args of
        [filename] -> main' filename []
        (filename:ghcopts) -> main' filename (map Doc.Flag_OptGhc ghcopts)
        _ -> putStrLn ("Usage: " ++ programName ++ " FILENAME [GHCOPTS]")
    where
        main' :: FilePath -> [Doc.Flag] -> IO ()
        main' filename opts = do
            let
                noReturn :: E.SomeException -> IO [Doc.Interface]
                noReturn e = print e >> return []
            source <- readFileUtf8 filename
            absFilename <- Dir.canonicalizePath filename
            docsMap <- fmap (fmap documentationMap . lookup absFilename) $ do
                is <- E.catch (Doc.createInterfaces opts [filename]) noReturn
                forM is $ \i -> do
                    moduleFile <- Dir.canonicalizePath $ Doc.ifaceOrigFilename i
                    return (moduleFile, i)
            let output = case analyzeModule source of
                    Left excuse -> Json.toJSON $ Json.object ["error" .= excuse]
                    Right info -> Json.toJSON $ maybe id addDocs docsMap info
            T.putStr "ModuleInfo:" -- workardound, Haddock prints to output, so this string is used to find result
            T.putStrLn . T.decodeUtf8 . Json.encode $ output

-- | Read file contents in UTF8
readFileUtf8 :: FilePath -> IO String
readFileUtf8 f = do
    h <- openFile f ReadMode
    hSetEncoding h utf8
    hGetContents h
