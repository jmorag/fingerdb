{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Fingerdb.Prelude
-- Description : Exports RIO without lens names
module Fingerdb.Prelude
  ( module X,
  )
where

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
import Control.Lens as X
