#! /usr/bin/env nix-shell
#! nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs/archive/914ef51ffa88d9b386c71bdc88bffc5273c08ada.tar.gz
#! nix-shell -p "haskell.packages.ghc923.ghcWithPackages (pkgs: with pkgs; [directory tar zlib unix filepath])"
#! nix-shell -i runhaskell
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main (main) where

import Codec.Archive.Tar qualified as Tar
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeBaseName)
import System.Posix.Files (isDirectory, getFileStatus)
import qualified Codec.Archive.Tar as Tar
import Data.Foldable (for_, find)
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import Text.Printf (printf)
import Data.Maybe (fromMaybe, maybeToList)
import Control.Monad (when)

data Template
  = Template
  { name :: String
  , path :: FilePath
  , destination :: Maybe FilePath
  , shouldExportTar :: Bool
  }
  deriving stock (Show)

templateAt :: FilePath -> IO Template
templateAt fp = pure $ Template { name = takeBaseName fp , path = fp, destination = Nothing, shouldExportTar = True }

-- | The branch that the tars will live on.
githubPagesBranch :: String
githubPagesBranch = "gh-pages"

templateFilePath :: Template -> FilePath
templateFilePath template =
  printf "https://raw.githubusercontent.com/emiflake/nix.dance/%s/%s/"
    githubPagesBranch
    template.name

createBashScript :: Template -> String
createBashScript template =
  unlines
  [ "#!/bin/sh"
  , printf "export TAR=%s" (templateFilePath template </> (template.name <> ".tar.gz"))
  , "curl --silent -L https://raw.githubusercontent.com/emiflake/nix.dance/main/install.sh | bash"
  ]

bundleTemplate :: FilePath -> Template -> IO ()
bundleTemplate outDirectory template = do
  subFiles <- listDirectory template.path
  tar <- Tar.pack template.path subFiles

  let templateOut = fromMaybe (outDirectory </> template.name) template.destination
  printf "[info]: [%s] Creating directory %s\n" template.name templateOut
  createDirectoryIfMissing True templateOut

  printf "[info]: [%s] Creating file %s\n" template.name "index.html"
  writeFile (templateOut </> "index.html") (createBashScript template)
  printf "[info]: [%s] Creating file %s\n" template.name (template.name <> ".tar.gz")
  when (template.shouldExportTar) $ BS.writeFile (templateOut </> template.name <> ".tar.gz") . GZip.compress $ Tar.write tar

main :: IO ()
main = do
  let rootDirectory :: FilePath
      rootDirectory = "./"
      outDirectory :: FilePath
      outDirectory = "bundles"

  createDirectoryIfMissing True outDirectory
  templateDirectories <- fmap ((rootDirectory </>) . ("templates" </>)) <$> listDirectory (rootDirectory </> "templates")
  templates <- traverse templateAt templateDirectories
  let root = [ t { destination = Just outDirectory , shouldExportTar = False } | t <- maybeToList (find (\temp -> temp.name == "haskell") templates) ]
  for_ (templates <> root) (bundleTemplate outDirectory)
  writeFile (outDirectory </> "CNAME") "nix.dance"
