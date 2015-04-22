{-# LANGUAGE ScopedTypeVariables #-}
module Chapter8 (globToRegex, matchesGlob) where

import Control.Exception (SomeException, handle)
import Control.Monad (forM)
import Data.Char (isUpper, toLower, toUpper)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath ((</>), dropTrailingPathSeparator, splitFileName)
import System.Info (os)
import System.Posix.Files (fileExist)
import Text.Regex.Posix

-- First set of exercises.

-- 2.

-- | User-facing function to convert glob to regex pattern,
-- with case-sensitivity turned on or off depending in the input flag.
globToRegex :: Bool -> String -> String
globToRegex caseFlag glob = '^' : globToRegex' caseFlag glob ++ "$"

-- | User-facing function to match files according to the input glob
-- with case-sensitivity turned on or off depending in the input flag.
matchesGlob :: Bool-> String -> FilePath -> Bool
matchesGlob caseFlag glob name = name =~ globToRegex caseFlag glob

-- | Helper function for glob to regex conversion.
globToRegex' :: Bool-> String -> String
globToRegex' _ "" = ""

globToRegex' caseFlag ('*':cs) = ".*" ++ globToRegex' caseFlag cs

globToRegex' caseFlag ('?':cs) = '.' : globToRegex' caseFlag cs

globToRegex' caseFlag ('[':'!':cs) = "[^" ++ charClass caseFlag cs
globToRegex' caseFlag ('[':cs)     = '['  :  charClass caseFlag cs

globToRegex' caseFlag (c:cs) = escape caseFlag c ++ globToRegex' caseFlag cs

-- | Helper function to add extra backslash for escape characters.
escape :: Bool -> Char -> String
escape caseFlag c
  | c `elem` regexChars = '\\' : [c]
  | caseFlag == True    = '[' : c : flipCase c : "]"
  | otherwise           = [c]
    where regexChars = "\\+()^$.{}]|"

-- | Helper function to produce regex character class string.
charClass :: Bool -> String -> String
charClass caseFlag (']':cs) = ']' : globToRegex' caseFlag cs
charClass True (c:cs)   = c : flipCase c : charClass True cs
charClass _ (c:cs)   = c : charClass False cs
charClass _ []       = error "unterminated character class"

-- | Helper function to return the character with the case opposite of the input.
flipCase :: Char -> Char
flipCase c
  | isUpper c = toLower c
  | otherwise = toUpper c

-- Second set of exercises.

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

isHidden :: String -> Bool
isHidden ('.':_) = True
isHidden _       = False

-- 1. listMatches modified to use new version of matchesGlob based on host OS.
--
-- N.B. Looks like something changed in handle since the book was
-- published and the type of Exception needs to be specified or
-- you'll get the following error upon compilation:
--
--   No instance for (GHC.Exception.Exception e0)
--     arising from a use of ‘handle’
--
-- I relied on this SO answer:
--
--   http://stackoverflow.com/questions/3670098/ghc-compilation-error-arising-from-import-of-control-exception#3670366
listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle (\(_ :: SomeException) -> return []) $ do
        names <- getDirectoryContents dirName'
        let caseFlag = if os =~ "mingw"
                       then True
                       else False
            names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        return (filter (matchesGlob caseFlag pat) names')

-- 2. Both namesMatching and listPlain modified to use fileExist.

namesMatching pat
  | not (isPattern pat) = do
    exists <- fileExist pat
    return (if exists then [pat] else [])
  | otherwise = do
    case splitFileName pat of
      ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
      (dirName, baseName) -> do
          dirs <- if isPattern dirName
                  then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
          pathNames <- forM dirs $ \dir -> do
                           baseNames <- listDir dir baseName
                           return (map (dir </>) baseNames)
          return (concat pathNames)

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
              then doesDirectoryExist dirName
              else fileExist (dirName </> baseName)
    return (if exists then [baseName] else [])
