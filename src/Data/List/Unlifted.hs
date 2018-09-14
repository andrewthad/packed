{-# language DataKinds #-}
{-# language GADTSyntax #-}
{-# language PolyKinds #-}
{-# language TypeInType #-}

module Data.List.Unlifted
  ( List(..)
  ) where

import GHC.Types (Type,TYPE,RuntimeRep(UnliftedRep))

data List :: TYPE 'UnliftedRep -> Type where
  Cons :: a -> List a -> List a
  Nil :: List a



