module Wow.TestPrelude (module M) where

import Prelude as M

import Test.Hspec as M (Spec, fdescribe, it, describe, fit, xit, xdescribe)

import Test.QuickCheck as M (property)