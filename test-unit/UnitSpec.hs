{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=UnitSpec #-}
module UnitSpec where

import qualified UnitSpec
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig UnitSpec.spec
