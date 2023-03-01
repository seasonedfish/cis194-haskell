{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)
import Data.Bits (xor)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret originalPath modifiedPath = do
  originalFile <- BS.readFile originalPath
  modifiedFile <- BS.readFile modifiedPath
  return $ BS.pack $ filter (/= 0) (BS.zipWith xor originalFile modifiedFile)

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key outputPath = do
  encryptedFile <- BS.readFile (outputPath ++ ".enc")
  let decryptedFile = BS.pack $ BS.zipWith xor encryptedFile (BS.cycle key)
  BS.writeFile outputPath decryptedFile
  
-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile jsonPath = do
  jsonFile <- BS.readFile jsonPath
  return $ decode jsonFile

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimsPath transactionsPath = do
  maybeVictims <- parseFile victimsPath
  maybeTransactions <- parseFile transactionsPath
  
  return $ getBadTs' maybeVictims maybeTransactions where
    getBadTs' :: Maybe [TId] -> Maybe [Transaction] -> Maybe [Transaction]
    getBadTs' (Just victims) (Just transactions) = do
      let victimsSet = Set.fromList victims
      let inVictimsSet (Transaction {tid=thisTid}) = thisTid `Set.member` victimsSet
      return $ filter inVictimsSet transactions
    getBadTs' _ _ = Nothing

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow transactions = Map.fromListWith (+) (toDeltas transactions) where
  toDeltas :: [Transaction] -> [(String, Integer)]
  toDeltas = concatMap (\t -> [(to t, amount t), (from t, -1 * amount t)])


-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal flowMap = Maybe.fromMaybe "Criminal not found" $ accumulateMax Nothing $ Map.toList flowMap where
  accumulateMax :: Maybe (String, Integer) -> [(String, Integer)] -> Maybe String
  accumulateMax Nothing list = case list of
    [] -> Nothing
    [(name, _)] -> Just name
    (flow:flows) -> accumulateMax (Just flow) flows
  accumulateMax (Just maxFlow@(maxName, maxDelta)) list = case list of
    [] -> Just maxName
    [(name, delta)] -> Just (if delta > maxDelta then name else maxName)
    (flow@(_, delta):flows) -> accumulateMax (Just (if delta > maxDelta then flow else maxFlow)) flows

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

