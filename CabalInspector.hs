{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import qualified Data.Aeson as Json
import Data.Aeson ((.=))
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Parse
import Distribution.ModuleName (components)
import qualified System.Environment as Environment

data CabalInfo = CabalInfo {
    cabalLibrary :: Maybe CabalLibrary,
    cabalExecutables :: [CabalExecutable],
    cabalTests :: [CabalTest] }
        deriving (Show)

instance Json.ToJSON CabalInfo where
    toJSON info = Json.object [
        "library" .= cabalLibrary info,
        "executables" .= cabalExecutables info,
        "tests" .= cabalTests info]

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

data CabalTest = CabalTest {
    testName :: String,
    testEnabled :: Bool,
    testBuildInfo :: Info }
        deriving (Show)

instance Json.ToJSON CabalTest where
    toJSON tst = Json.object [
        "name" .= testName tst,
        "enabled" .= testEnabled tst,
        "info" .= testBuildInfo tst]

data Info = Info {
    infoSourceDirs :: [FilePath] }
        deriving (Show)

instance Json.ToJSON Info where
    toJSON i = Json.object [
        "source-dirs" .= infoSourceDirs i]

analyzeCabal :: String -> Either String CabalInfo
analyzeCabal source = case parsePackageDescription source of
    ParseOk _ r -> Right CabalInfo {
        cabalLibrary = fmap (toLibrary . PD.condTreeData) $ PD.condLibrary r,
        cabalExecutables = fmap (toExecutable . second PD.condTreeData) $ PD.condExecutables r,
        cabalTests = fmap (toTest . second PD.condTreeData) $ PD.condTestSuites r }
    ParseFailed e -> Left $ "Parse failed: " ++ show e
    where
        toLibrary library = CabalLibrary (map components (PD.exposedModules library)) (toInfo (PD.libBuildInfo library))
        toExecutable (name, PD.Executable _ path info) = CabalExecutable name path (toInfo info)
        toTest (name, PD.TestSuite _ _ info enabled) = CabalTest name enabled (toInfo info)
        toInfo info = Info {
            infoSourceDirs = PD.hsSourceDirs info }

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
