module Markup.Internal where

import Numeric.Natural

type Document
    = [Structure]

data Structure
    = Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]
    deriving Show

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines [] [] = []
parseLines [] ("":ls) = parseLines [] ls 
parseLines acc [] = [(Paragraph . unwords) acc]
parseLines acc ("":ls) = [(Paragraph . unwords) acc] ++ (parseLines [] ls)
parseLines acc (l:ls) = parseLines (acc ++ [l]) ls

parse2 :: String -> Document
parse2 = parseLines [] . lines -- (1)

parseLines2 :: [String] -> [String] -> Document
parseLines2 currentParagraph txts =
  let
    paragraph = Paragraph (unlines (reverse currentParagraph)) -- (2), (3)
  in
    case txts of -- (4)
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then
            paragraph : parseLines [] rest -- (5)
          else
            parseLines (currentLine : currentParagraph) rest -- (6)

trim :: String -> String
trim = unwords . words


{-
instance Show Structure where
    show (Heading n txt) = "Structure: Heading (" ++ (show n) ++ "): \"" ++ txt ++ "\""
    show (Paragraph txt) = "Structure: Paragraph: \"" ++ txt ++ "\""
    show _ = "Structure"
-}
