{-# OPTIONS_GHC -F -pgmF hspec-discover -optF --module-name=E2ESpec #-}
module E2ESpec where

import qualified E2ESpec
import           Test.Hspec.Formatters
import           Test.Hspec.Runner

main :: IO ()
main = hspecWith defaultConfig E2ESpec.spec
