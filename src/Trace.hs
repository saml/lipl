{-# LANGUAGE TemplateHaskell #-}

module Trace where

import Language.Haskell.TH
import Debug.Trace (trace)

t name = [| trace ($(litE . StringL $ nameBase name)
    ++ ": " ++ show $(varE name)) $(varE name) |]

