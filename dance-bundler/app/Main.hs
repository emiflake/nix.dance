{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module Main (main) where

import Codec.Archive.Tar qualified as Tar
import System.Directory (listDirectory, createDirectoryIfMissing)
import System.FilePath ((</>), takeBaseName)
import System.Posix.Files (isDirectory, getFileStatus)
import qualified Codec.Archive.Tar as Tar
import Data.Foldable (for_)
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Compression.GZip as GZip
import Text.Printf (printf)

data Template
  = Template
  { name :: String
  , path :: FilePath
  }
  deriving stock (Show)

templateAt :: FilePath -> IO Template
templateAt fp = pure $ Template { name = takeBaseName fp , path = fp }

-- | The branch that the tars will live on.
githubPagesBranch :: String
githubPagesBranch = "gh-pages"

templateFilePath :: Template -> FilePath
templateFilePath template = 
  printf "https://raw.githubusercontent.com/emiflake/nix.dance/%s/bundles/%s/" 
    githubPagesBranch 
    template.name 

createBashScript :: Template -> String
createBashScript template =
  unlines
  [ "#!/bin/sh"
  , printf "export TAR=%s" (templateFilePath template </> (template.name <> ".tar.gz"))
  , "curl --silent -L https://raw.githubusercontent.com/emiflake/nix.dance/main/install.sh | bash"
  ]

bundleTemplate :: FilePath -> FilePath -> IO ()
bundleTemplate outDirectory fp = do
  template <- templateAt fp
  print template
  subFiles <- listDirectory template.path
  tar <- Tar.pack template.path subFiles

  let templateOut = outDirectory </> template.name
  createDirectoryIfMissing True templateOut
 
  writeFile (templateOut </> "index.html") (createBashScript template)
  BS.writeFile (templateOut </> template.name <> ".tar.gz") . GZip.compress $ Tar.write tar

main :: IO ()
main = do
  let rootDirectory :: FilePath
      rootDirectory = "./"
      outDirectory :: FilePath
      outDirectory = "bundles"

  createDirectoryIfMissing True outDirectory
  templates <- fmap ((rootDirectory </>) . ("templates" </>)) <$> listDirectory (rootDirectory </> "templates")
  for_ templates (bundleTemplate outDirectory)
