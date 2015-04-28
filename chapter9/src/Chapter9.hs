{-# LANGUAGE ScopedTypeVariables #-}

module Chapter9 where

import Control.Exception (SomeException, bracket, handle)
import Control.Monad (filterM, forM)
import Data.Time.Clock (UTCTime(..))
import System.Directory (Permissions(..),
                         doesDirectoryExist,
                         getDirectoryContents,
                         getModificationTime,
                         getPermissions)
import System.FilePath ((</>))
import System.IO (IOMode(..), hClose, hFileSize, openFile)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return $ concat paths

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  return $ filter p names

type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- permissions
               -> Maybe Integer -- file size (Nothing if not file)
               -> UTCTime       -- last modified
               -> Bool

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check where
  check name = do
    perms <- getPermissions name
    size <- getFileSize2 name
    modified <- getModificationTime name
    return $ p name perms size modified

-- First set of exercises

-- 1.

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(_ :: SomeException) -> return Nothing) $ do
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return $ Just size

getFileSize2 :: FilePath -> IO (Maybe Integer)
getFileSize2 path = bracket (openFile path ReadMode) hClose $ \h -> do
  handle (\(_ :: SomeException) -> return Nothing) $ do
    size <- hFileSize h
    return $ Just size

-- So, functionally the two functions above do the exact same thing.
-- The only difference between having handle "inside" bracket and vice versa
-- that I can see is that if bracket is on the "outside" and either
-- the "acquire" or "release" function fail there is nothing to catch the
-- resultant exceptions. Having handle on the "outside" ensures that all
-- exceptions, whether they originate from the "acquire", "use", or "release"
-- functions, are handled. So... of the two functions above, the first is
-- preferable because it insures that all possible exceptions are caught
-- and it returns a value such that the function can be safely and easily
-- be involved in other computations.
