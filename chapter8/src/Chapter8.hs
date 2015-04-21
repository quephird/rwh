module Chapter8 (globToRegex, matchesGlob) where

import Data.Char (isUpper, toLower, toUpper)
import Text.Regex.Posix

-- | User-facing function to convert glob to regex pattern,
-- with case-sensitivity turned on or off depending in the input flag.
globToRegex :: Bool -> String -> String
globToRegex caseFlag glob = '^' : globToRegex' caseFlag glob ++ "$"

-- | User-facing function to match files according to the input glob
-- with case-sensitivity turned on or off depending in the input flag.
matchesGlob :: FilePath -> Bool-> String -> Bool
matchesGlob name caseFlag glob = name =~ globToRegex caseFlag glob

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
