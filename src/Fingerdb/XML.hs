{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Fingerdb.XML
  ( separateFingerings,
    mergeFingerings,
    adjustMeasures
  )
where

import Data.Data.Lens
import Fingerdb.Prelude
import qualified RIO.ByteString as B
import qualified RIO.Text as T
import RIO.List (sortOn)
import Text.XML.Light
import Fmt

-- | Remove the
-- <technical><fingering>f</fingering><string>s</string></technical>
-- elements from the top level score element
separateFingerings :: Element -> (Element, [Element])
separateFingerings score =
  ( score & nodes "technical" . elContent_ %~ filter (not . isFingering),
    score ^.. nodes "technical" <&> over elContent_ (filter isFingering)
  )

-- | Inverse of 'separateFingerings'
mergeFingerings :: Element -> [Element] -> Element
mergeFingerings music fingerings =
  over
    (partsOf $ nodes "technical")
    (zipWith merge fingerings)
    music
  where
    merge t1 t2 = over elContent_ (sortOn (view line) . (++ elContent t2)) t1

isFingering :: Content -> Bool
isFingering = \case
  Elem e -> e ^. name `elem` ["fingering", "string"]
  _ -> False

-- | Given start and end measure, adjust the measures of the given score to lie in
-- that range. Returns error if the number of non-implicit measures doesn't
-- correspond to the given parameters
adjustMeasures :: Int -> Int -> Element -> Either String Element
adjustMeasures beg end score =
  let newMeasures = map (Just . show) [beg .. end]
      oldMeasures = (score ^.. measureNumbers)
   in if length newMeasures == length oldMeasures
        then Right $ set (partsOf measureNumbers) newMeasures score
        else
          Left $
            "Expected "
              +| length newMeasures |+ " measures but got "
              +| length oldMeasures |+ " measures instead"

measureNumbers :: Traversal' Element (Maybe String)
measureNumbers =
  nodes "measure"
    . filtered (\measure -> measure ^. attr "implicit" /= Just "yes")
    . attr "number"

attr :: String -> Lens' Element (Maybe String)
attr key = lens getter setter
  where
    getter = findAttrBy (\q -> qName q == key)
    setter node val =
      -- Setting an attribute key's value to Nothing removes it from the node
      -- Setting an existing key moves it to the front of the attr list
      -- This might break laws...
      let oldAttrs = filter (\(Attr (QName k _ _) _) -> k /= key) (elAttribs node)
       in case val of
            Nothing -> node {elAttribs = oldAttrs}
            Just v -> node {elAttribs = Attr (QName key Nothing Nothing) v : oldAttrs}

nodes :: String -> Traversal' Element Element
nodes elementName = deepOf uniplate (filtered (\e -> e^.name == elementName))

name :: Lens' Element String
name = lens (qName . elName) (\e n -> e {elName = QName n Nothing Nothing})

line :: Lens' Content (Maybe Integer)
line = lens (\case Elem e -> elLine e; Text t -> cdLine t; CRef _ -> Nothing) setter
  where
    setter content l = case content of
      Elem e -> Elem $ e {elLine = l}
      Text t -> Text $ t {cdLine = l}
      CRef _ -> content

elContent_ :: Lens' Element [Content]
elContent_ = lens elContent (\e cs -> e {elContent = cs})

_Element :: Prism' Content Element
_Element = prism' Elem (\case (Elem e) -> Just e; _ -> Nothing)

--------------------------------------------------------------------------------
-- repl utils
--------------------------------------------------------------------------------

readXML :: FilePath -> IO Element
readXML = fmap
  (fromMaybe undefined . parseXMLDoc . T.unpack . decodeUtf8Lenient) . B.readFile

printElem :: Element -> IO ()
printElem = B.putStr . T.encodeUtf8 . T.pack . showElement

printElems :: [Element] -> IO ()
printElems es = printElem (unode "wrapper" es) >> B.putStr "\n"

prok = readXML "/home/joseph/Documents/MuseScore3/Scores/Prokofiev_violin_concerto_No_2_excerpt.musicxml"

brahms = readXML "/home/joseph/Documents/MuseScore3/Scores/Brahms_violin_concerto.musicxml"
