{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
-- on a Mac - run this with:
-- > fastAnagrams "Tony" "/usr/share/dict/words"
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams str fpath = flip filter (permutations str) . flip S.member . buildSet . lines <$> readFile fpath
  where buildSet = S.fromList . hlist

newtype NoCaseString =
  NoCaseString
    Chars

ncString ::
  NoCaseString
  -> Chars
ncString (NoCaseString s) =
  s

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
