{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import qualified Data.Aeson as Json
import Data.Aeson ((.=))
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy.IO as T
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import qualified System.Environment as Environment

data CabalInfo = CabalInfo {
    cabalExecutables :: [CabalExecutable],
    cabalSourceDirs :: [FilePath] }
        deriving (Show)

instance Json.ToJSON CabalInfo where
    toJSON info = Json.object [
        "executables" .= cabalExecutables info,
        "source-dirs" .= cabalSourceDirs info]

data CabalExecutable = CabalExecutable {
    executableName :: String,
    executablePath :: String }
        deriving (Show)

instance Json.ToJSON CabalExecutable where
    toJSON exe = Json.object [
        "name" .= executableName exe,
        "path" .= executablePath exe]

analyzeCabal :: String -> Either String CabalInfo
analyzeCabal source = case parsePackageDescription source of
    ParseOk _ r -> Right CabalInfo {
        cabalExecutables = map (uncurry CabalExecutable . second (exeName . condTreeData)) $ condExecutables r,
        cabalSourceDirs = maybe [] (hsSourceDirs . libBuildInfo . condTreeData) $ condLibrary r }
    ParseFailed e -> Left $ "Parse failed: " ++ show e

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
