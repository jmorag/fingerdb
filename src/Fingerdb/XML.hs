{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Fingerdb.XML
  ( separateFingerings,
    mergeFingerings,
  )
where

import Control.Lens
import Data.Data.Lens
import RIO hiding
  ( ASetter,
    ASetter',
    Getting,
    Lens,
    Lens',
    SimpleGetter,
    (^.),
    lens,
    over,
    set,
    sets,
    to,
    view,
  )
import RIO.List (sortOn)
import Text.XML.Light

-- | Remove the
-- <technical><fingering>f</fingering><string>s</string></technical>
-- elements from the top level score element
separateFingerings :: Element -> (Element, [Element])
separateFingerings top =
  ( top & technical . elContent_ %~ filter (not . isFingering),
    top ^.. technical <&> over elContent_ (filter isFingering)
  )

mergeFingerings :: Element -> [Element] -> Element
mergeFingerings music fingerings =
  over
    (partsOf technical)
    (zipWith merge fingerings)
    music
  where
    merge t1 t2 = over elContent_ (sortOn (view line) . (++ elContent t2)) t1

isFingering :: Content -> Bool
isFingering = \case
  Elem e -> e ^. name `elem` ["fingering", "string"]
  _ -> False

technical :: Traversal' Element Element
technical = deepOf uniplate (filtered (\e -> e ^. name == "technical"))

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

measures :: Traversal' Element Element
measures = deepOf uniplate (filtered (\e -> e ^. name == "measure"))



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
