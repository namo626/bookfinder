{-# LANGUAGE OverloadedStrings #-}

module Main where

import Text.HTML.Scalpel
import System.Environment (getArgs)
import Control.Monad (forM_)

data Price = New String
           | Used String
           deriving (Eq)

instance Show Price where
  show (New str) = "New price is " ++ str
  show (Used str) = "Used price is " ++ str

type Title = String

prices :: Scraper String [String]
prices = texts $ "span" @: ["class" @= "results-price"]

tablePrices' :: Scraper String [[String]]
tablePrices' = chroots ("table" @: ["class" @= "results-table-Logo"]) prices

firstURL :: Scraper String URL
firstURL =
  attr "href" ("span" @: ["class" @= "select-titlename-top-match"] // "a")

firstURLs :: Scraper String URL
firstURLs =
  chroot ("ul" @: ["class" @= "select-titlenames"]) firstURL

title :: Scraper String [Title]
title =
  chroots ("div" @: ["class" @= "describe-enhanced-isbn-search"] // "strong") $ text "strong"


main :: IO ()
main = do
  searchURLs <- getContents
  let urls = lines searchURLs  -- :: [URL], multiple urls for different books
  forM_ urls processURL

processURL :: URL -> IO ()
processURL url = do
  firstLink <- scrapeURL (url) firstURLs  
  case firstLink of
    Nothing -> putStrLn "Book URL not found"
    Just link -> printResults link

printResults :: URL -> IO ()
printResults link = do
  results <- scrapeURL link $ do
    ps <- tablePrices'
    t <- title
    return (t, ps)
  case results of
    Nothing -> putStrLn "Prices not found"
    Just (t, (r:rs:_)) -> do
      putStrLn $ t !! 1
      putStrLn $ show $ head $ map New r
      putStrLn $ show $ head $ map Used rs
      putStrLn ""