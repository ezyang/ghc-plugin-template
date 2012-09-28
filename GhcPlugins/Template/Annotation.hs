{-# LANGUAGE DeriveDataTypeable #-}
module GhcPlugins.Template.Annotation where

import Data.Data

-- | Annotations are a convenient way of marking bindings and
-- expressions in a way that can be found by the core pass;
-- for example, if you only want to apply your transformation
-- when the user explicitly requests it for some code. See
-- http://www.haskell.org/ghc/docs/latest/html/users_guide/compiler-plugins.html#getting-annotations
-- for more details.  This particular annotation can be used as
-- {-# ANN identifierName Template #-} after importing this module; see
-- http://www.haskell.org/ghc/docs/latest/html/users_guide/extending-ghc.html#annotation-pragmas
-- for more details, including what extra data annotations support.

data Template = Template deriving (Typeable, Data)
