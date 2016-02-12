{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Main (main) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Control.Exception (bracket)
import           Data.Aeson (Value(..), object, (.=), (.:), Object(..), decode, FromJSON(..))
import           Network.Wai.Test (SResponse(..))
import           Network.HTTP.Types.Status

import           CouchGames.Server (appTest)
import           CouchGames.Player
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    with appTest $ do
        describe "GET /" $ do
            it "responds with 200" $ do
                get "/" `shouldRespondWith` 200

            it "responds with 'hello'" $ do
                get "/" `shouldRespondWith` "hello"

            it "responds with 200 / 'hello'" $ do
                get "/" `shouldRespondWith` "hello" {matchStatus = 200}

            it "has 'Content-Type: text/plain; charset=utf-8'" $ do
                get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}

        describe "GET /some-json" $ do
            it "responds with some JSON" $ do
                get "/some-json" `shouldRespondWith` [json|{foo: 23, bar: 42}|]

        describe "GET /users" $ do
            it "fails to get an invalid id" $ do
                get "/users/1" `shouldRespondWith` [json|{code: 1, message:"User not found"}|] {matchStatus = 404}

            it "creates a user" $ do
                (SResponse s _ b) <- postHtmlForm "/users/register" [("name", "test_username"), ("email", "test@test.com"), ("password", "pass1234")]
                liftIO $ statusCode s `shouldBe` 200
                case (decode b :: Maybe RegisterUserR) of
                    Just rur    -> do
                        (SResponse s _ b) <- get (BS.append "/users/" (BS.pack (show (rurUserId rur))))
                        liftIO $ statusCode s `shouldBe` 200
                        case (decode b :: Maybe GetUserR) of 
                            Just gur    -> liftIO $ gurUserId gur `shouldBe` rurUserId rur
                            Nothing     -> liftIO $ expectationFailure "Could not decode response"
                    Nothing     -> liftIO $ expectationFailure "Could not decode response"

            it "fails to create a duplicate user" $ do
                postHtmlForm "/users/register" [("name", "test_username"), ("email", "test2@test.com"), ("password", "1223pass")]
                    `shouldRespondWith` [json|{code: 4, message: "Username already taken"}|] {matchStatus = 404}

data GetUserR = GetUserR
    { gurUserId :: Int
    , gurName   :: String
    } deriving (Show, Eq)

instance FromJSON GetUserR where
    parseJSON (Object v) =
        GetUserR <$> (v .: "userId") <*> (v .: "name")

data RegisterUserR = RegisterUserR
    { rurUserId :: Int
    , rurName   :: String
    } deriving (Show, Eq)

instance FromJSON RegisterUserR where
    parseJSON (Object v) =
        RegisterUserR <$> (v .: "userId") <*> (v .: "name")
