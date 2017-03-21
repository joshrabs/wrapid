{-# LANGUAGE OverloadedStrings #-}

module Common.Utils.Basic where

import qualified Data.Set           as Set
import           Data.String.Utils
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE 
import           Data.Maybe
import           Data.List
import           Data.Function
import           Data.List (sortBy)
import           Data.List.Split
import qualified Data.ByteString.Char8      as BSC
import           Numeric
import           System.Random
import           System.IO.Unsafe
import           Safe

--------------------------------------------------------------------------------
-- Helper Functions

randomStr :: String
randomStr = take 10 $ randomRs ('a','z') $ unsafePerformIO newStdGen

cmp :: (Int, String, String) -> Int
cmp (l, w, y) = l      

rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if Set.member b a
    then rmdups' a c
    else b : rmdups' (Set.insert b a) c

formatFloatN :: RealFloat a => Int -> a -> String
formatFloatN  numOfDecimals floatNum =
  showFFloat (Just numOfDecimals) floatNum ""

-- | List of elements occurences inside
--   of list
occurs :: Eq a => [a] -> [(a, Int)]
occurs xs =
  occursAux [] xs 
    where
      occursAux :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
      occursAux init    []  = init 
      occursAux init (x:xs) = occursAux (initNew init x) xs
        where

          initNew :: Eq a => [(a, Int)] -> a -> [(a, Int)]
          initNew init x = (remove' init x) ++ [(eval init x)] 

          eval :: Eq a => [(a, Int)] -> a -> (a, Int)
          eval []   x = (x, 1)
          eval init x =
            case lookup x init of
              Nothing  -> (x, 1)
              Just val -> (x, val+1)

          remove' :: Eq a => [(a, Int)] -> a -> [(a, Int)]
          remove' [] _  = []
          remove' xs el = filter (\x@(a,i) -> a /= el) xs 

-- | Functions that converts Maybe Bytestring to Text
conBStoT :: Maybe BSC.ByteString -> T.Text          
conBStoT str =
  TE.decodeUtf8 $ fromMaybe "" str
