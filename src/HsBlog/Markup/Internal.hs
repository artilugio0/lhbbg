module HsBlog.Markup.Internal where

import Numeric.Natural
import Data.Maybe

type Document
    = [Structure]

data Structure
    = Heading Natural String
    | Paragraph String
    | UnorderedList [String]
    | OrderedList [String]
    | CodeBlock [String]
    deriving (Show, Eq)

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines Nothing [] = []
parseLines Nothing ("":ls) = parseLines Nothing ls 
parseLines Nothing (('*': ' ' :line):rest) = (Heading 1 line) : parseLines Nothing rest 
parseLines (Just currentStructure) (('*': ' ' :line):rest) = currentStructure : (Heading 1 line) : parseLines Nothing rest 

-- This should never match. Headings are never added to the context of parseLines
parseLines (Just (Heading _ _)) _ = undefined

parseLines Nothing (('-': ' ' :line):rest) = parseLines (Just (UnorderedList [line])) rest 
parseLines (Just (UnorderedList items)) (('-': ' ' :line):rest) = parseLines (Just (UnorderedList (items ++ [line]))) rest 
parseLines (Just (UnorderedList items)) rest = (UnorderedList items) : parseLines Nothing rest
parseLines (Just currentStructure) (('-': ' ' :line):rest) = currentStructure : parseLines (Just (UnorderedList ([line]))) rest 

parseLines Nothing (('#': ' ' :line):rest) = parseLines (Just (OrderedList [line])) rest 
parseLines (Just (OrderedList items)) (('#': ' ' :line):rest) = parseLines (Just (OrderedList (items ++ [line]))) rest 
parseLines (Just (OrderedList items)) rest = (OrderedList items) : parseLines Nothing rest
parseLines (Just currentStructure) (('#': ' ' :line):rest) = currentStructure : parseLines (Just (OrderedList ([line]))) rest 

parseLines Nothing (('>': ' ' :line):rest) = parseLines (Just (CodeBlock [line])) rest 
parseLines (Just (CodeBlock items)) (('>': ' ' :line):rest) = parseLines (Just (CodeBlock (items ++ [line]))) rest 
parseLines (Just (CodeBlock items)) rest = (CodeBlock items) : parseLines Nothing rest
parseLines (Just currentStructure) (('>': ' ' :line):rest) = currentStructure : parseLines (Just (CodeBlock ([line]))) rest 

parseLines Nothing (l:ls) = parseLines (Just ((Paragraph . trim) l)) ls 
parseLines (Just currentStructure) [] = [currentStructure]
parseLines (Just currentStructure) ("":ls) = currentStructure : (parseLines Nothing ls)
parseLines (Just (Paragraph text)) (l:ls) = parseLines (Just (Paragraph (text ++ " " ++ (trim l)))) ls

parse2 :: String -> Document
parse2 = parseLines2 Nothing . lines

parseLines2 :: Maybe Structure -> [String] -> Document
parseLines2 context txts =
  case txts of
    -- done case
    [] -> maybeToList context

    -- Heading 1 case
    ('*' : ' ' : line) : rest ->
      maybe id (:) context (Heading 1 (trim line) : parseLines2 Nothing rest)

    -- Unordered list case
    ('-' : ' ' : line) : rest ->
      case context of
        Just (UnorderedList list) ->
          parseLines2(Just (UnorderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines2(Just (UnorderedList [trim line])) rest)

    -- Ordered list case
    ('#' : ' ' : line) : rest ->
      case context of
        Just (OrderedList list) ->
          parseLines2(Just (OrderedList (list <> [trim line]))) rest

        _ ->
          maybe id (:) context (parseLines2(Just (OrderedList [trim line])) rest)

    -- Code block case
    ('>' : ' ' : line) : rest ->
      case context of
        Just (CodeBlock code) ->
          parseLines2(Just (CodeBlock (code <> [line]))) rest

        _ ->
          maybe id (:) context (parseLines2(Just (CodeBlock [line])) rest)

    -- Paragraph case
    currentLine : rest ->
      let
        line = trim currentLine
      in
        if line == ""
          then
            maybe id (:) context (parseLines2 Nothing rest)
          else
            case context of
              Just (Paragraph paragraph) ->
                parseLines2(Just (Paragraph (unwords [paragraph, line]))) rest
              _ ->
                maybe id (:) context (parseLines2(Just (Paragraph line)) rest)


trim :: String -> String
trim = unwords . words
