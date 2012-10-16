{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Arrow
import qualified Data.Aeson as Json
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy.Char8 as LazyByteString
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import qualified System.Environment as Environment

data CabalInfo = CabalInfo {
    cabalExecutables :: [CabalExecutable] }
        deriving (Show)

instance Json.ToJSON CabalInfo where
    toJSON info = Json.object [
        "executables" .= cabalExecutables info]

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
    ParseOk _ r -> Right $ CabalInfo $ map (uncurry CabalExecutable . second (exeName . condTreeData)) $ condExecutables r
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
            LazyByteString.putStrLn . Json.encode $ output
        _ -> putStrLn ("Usage: " ++ programName ++ " FILENAME")
