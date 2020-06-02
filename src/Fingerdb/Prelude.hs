{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Fingerdb.Prelude
  ( module X,
  )
where

import Control.Lens as X
import Fmt as X
  ( (+|),
    (+||),
    fmt,
    fmtLn,
    format,
    formatLn,
    (|+),
    (|++|),
    (|++||),
    (||+),
    (||++|),
    (||++||),
  )
import qualified Fmt.Internal.Core as Fmt
import RIO as X hiding
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

instance Fmt.FromBuilder Utf8Builder where
  fromBuilder = Utf8Builder . Fmt.fromBuilder
