module Wow.Data.CommandSpec where

import Wow.TestPrelude
import Wow.Data.Command (parseCommand, Command(..))
import Test.Hspec (shouldBe, shouldSatisfy)
import Data.Either (isLeft)

spec :: Spec
spec = describe "Command" $ do
  it "should parse valid greetings successfully" $ do
    let name = "Pierre"
    parseCommand (":greeting " <> name) `shouldBe` Right (CmdGreeting name)
  it "should disallow invalid chars" $ do
    let name = "Pierre{"
    parseCommand (":greeting " <> name) `shouldSatisfy` isLeft
  it "should parse valid listen commands" $ do
    parseCommand ":listen" `shouldBe` Right CmdListen
  it "should parse valid unlisten commands" $ do
    parseCommand ":unlisten" `shouldBe` Right CmdUnlisten
  it "should parse valid filter commands" $ do
    parseCommand ":filter music" `shouldBe` Right (CmdFilter "music")