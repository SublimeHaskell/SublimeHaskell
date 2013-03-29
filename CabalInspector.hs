{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import qualified Data.Aeson as Json
import Data.Aeson ((.=))
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.ModuleName (components)
import qualified System.Environment as Environment

data CabalInfo = CabalInfo {
    cabalLibrary :: Maybe CabalLibrary,
    cabalExecutables :: [CabalExecutable] }
        deriving (Show)

instance Json.ToJSON CabalInfo where
    toJSON info = Json.object [
        "library" .= cabalLibrary info,
        "executables" .= cabalExecutables info]

data CabalLibrary = CabalLibrary {
    libraryModules :: [[String]],
    libraryBuildInfo :: Info }
        deriving (Show)

instance Json.ToJSON CabalLibrary where
    toJSON lib = Json.object [
        "modules" .= libraryModules lib,
        "info" .= libraryBuildInfo lib]

data CabalExecutable = CabalExecutable {
    executableName :: String,
    executablePath :: FilePath,
    executableBuildInfo :: Info }
        deriving (Show)

instance Json.ToJSON CabalExecutable where
    toJSON exe = Json.object [
        "name" .= executableName exe,
        "path" .= executablePath exe,
        "info" .= executableBuildInfo exe]

data Info = Info {
    infoSourceDirs :: [FilePath] }
        deriving (Show)

instance Json.ToJSON Info where
    toJSON i = Json.object [
        "source-dirs" .= infoSourceDirs i]

analyzeCabal :: String -> Either String CabalInfo
analyzeCabal source = case parsePackageDescription source of
    ParseOk _ r -> Right CabalInfo {
        cabalLibrary = fmap (toLibrary . condTreeData) $ condLibrary r,
        cabalExecutables = fmap (toExecutable . second condTreeData) $ condExecutables r }
    ParseFailed e -> Left $ "Parse failed: " ++ show e
    where
        toLibrary (Library exposeds _ info) = CabalLibrary (map components exposeds) (toInfo info)
        toExecutable (name, Executable _ path info) = CabalExecutable name path (toInfo info)
        toInfo info = Info {
            infoSourceDirs = hsSourceDirs info }

main :: IO ()
main = do
    programName <- Environment.getProgName
    args <- Environment.getArgs
    case args of
        [filename] -> do
            source <- readFile filename
            let
                output = case analyzeCabal source of
                    Left excuse -> Json.toJSON $ Json.object ["error" .= excuse]
                    Right info -> Json.toJSON info
            T.putStrLn . T.decodeUtf8 . Json.encode $ output
        _ -> putStrLn ("Usage: " ++ programName ++ " FILENAME")
