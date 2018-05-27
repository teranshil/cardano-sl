{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Pos.Binary.Indexed
    ( tests
    ) where

import           Universum

import           Data.Text.Buildable (Buildable (..))
import qualified Data.Text.Internal.Builder as Builder

import           Hedgehog (Gen, Property, discover, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Binary.Class (Bi, Cons (..), Field (..), cborError, deriveIndexedBi, serialize')

import qualified Serokell.Util.Base16 as B16

import           Test.Pos.Binary.Tripping (trippingBiBuildable, trippingBiShow)


--------------------------------------------------------------------------------
-- Test helpers
--------------------------------------------------------------------------------

mk_prop_round_trip_derived_bi_show_instance :: (Bi a, Eq a, Show a)
                                            => H.TestLimit -> Gen a -> Property
mk_prop_round_trip_derived_bi_show_instance count gen =
    H.withTests count . H.property $
        trippingBiShow =<< H.forAll gen

mk_prop_round_trip_derived_bi_buildable_instance :: (Bi a, Eq a, Show a, Buildable a)
                                                 => H.TestLimit -> Gen a -> Property
mk_prop_round_trip_derived_bi_buildable_instance count gen =
    H.withTests count . H.property $
        trippingBiBuildable =<< H.forAll gen

--------------------------------------------------------------------------------

data Test
    = TestInt Int
    | TestIntList [Int]
    | TestChar2 Char Char
    | TestInteger Integer
    | TestMaybeInt (Maybe Int)
    | TestChar2Permuted Char Char
    deriving (Eq, Show, Typeable)

deriveIndexedBi ''Test [
    Cons 'TestInt [
        Field [| 0 :: Int       |]
        ],
    Cons 'TestIntList [
        Field [| 0 :: [Int]     |]
        ],
    Cons 'TestChar2 [
        Field [| 0 :: Char      |],
        Field [| 1 :: Char      |]
        ],
    Cons 'TestInteger [
        Field [| 0 :: Integer   |]
        ],
    Cons 'TestMaybeInt [
        Field [| 0 :: Maybe Int |]
        ],
    Cons 'TestChar2Permuted [
        Field [| 1 :: Char      |],
        Field [| 0 :: Char      |]
        ]
    ]

instance Buildable Test where
    build = Builder.fromString . show


genTest :: Gen Test
genTest =
    Gen.choice
        [ TestInt <$> Gen.int Range.constantBounded
        , TestIntList <$> Gen.list (Range.linear 0 20) (Gen.int Range.constantBounded)
        , TestChar2 <$> Gen.unicode <*> Gen.unicode
        , TestInteger <$> Gen.integral (Range.linear (- bignum) bignum)
        , TestMaybeInt <$> Gen.maybe (Gen.int Range.constantBounded)
        , TestChar2Permuted <$> Gen.unicode <*> Gen.unicode
        ]
  where
    bignum = 2 ^ (80 :: Integer)


prop_golden_TestInt :: Property
prop_golden_TestInt =
    H.withTests 1 . H.property $
        B16.encode (serialize' $ TestInt 42) === "8200182a"

prop_golden_TestIntList :: Property
prop_golden_TestIntList =
    H.withTests 1 . H.property $
        B16.encode (serialize' $ TestIntList [1, 3, 5, 7]) === "82019f01030507ff"

prop_golden_TestChar2 :: Property
prop_golden_TestChar2 =
    H.withTests 1 . H.property $
        B16.encode (serialize' $ TestChar2 '\0' 'a') === "830261006161"

prop_golden_TestInteger :: Property
prop_golden_TestInteger =
    H.withTests 1 . H.property $
        B16.encode (serialize' $ TestInteger 123456789123456789123456789)
            === "8203c24b661efdf2e3b19f7c045f15"

prop_golden_TestMaybeIntNothing :: Property
prop_golden_TestMaybeIntNothing =
    H.withTests 1 . H.property $
        B16.encode (serialize' $ TestMaybeInt Nothing) === "820480"

prop_golden_TestMaybeIntJust :: Property
prop_golden_TestMaybeIntJust =
    H.withTests 1 . H.property $
        B16.encode (serialize' $ TestMaybeInt (Just 42)) === "820481182a"

prop_golden_TestChar2Permuted :: Property
prop_golden_TestChar2Permuted =
    H.withTests 1 . H.property $
        B16.encode (serialize' $ TestChar2Permuted '\0' 'a') === "830561616100"

prop_round_trip_derived_bi_show_instance_Test :: Property
prop_round_trip_derived_bi_show_instance_Test
  = mk_prop_round_trip_derived_bi_show_instance 5000 genTest
prop_round_trip_derived_bi_buildable_instance_Test :: Property
prop_round_trip_derived_bi_buildable_instance_Test
  = mk_prop_round_trip_derived_bi_buildable_instance 5000 genTest

-- -----------------------------------------------------------------------------

data TestTreeInt
    = Leaf
    | Node Int TestTreeInt TestTreeInt
    deriving (Eq, Show, Typeable)

deriveIndexedBi ''TestTreeInt [
    Cons 'Leaf [
        ],
    Cons 'Node [
        Field [| 0 :: Int          |],
        Field [| 1 :: TestTreeInt |],
        Field [| 2 :: TestTreeInt |]
        ]
    ]

instance Buildable TestTreeInt where
    build = Builder.fromString . show

prop_golden_TestTree0 :: Property
prop_golden_TestTree0 =
    H.withTests 1 . H.property $
        B16.encode (serialize' $ Leaf)
            === "8100"

prop_golden_TestTree1 :: Property
prop_golden_TestTree1 =
    H.withTests 1 . H.property $
        B16.encode (serialize' $ Node 7 Leaf Leaf)
            === "84010781008100"

genTestTreeInt :: Range.Size -> Gen TestTreeInt
genTestTreeInt sz
  | sz <= 0 = pure Leaf
  | otherwise =
    Gen.choice
        [ pure Leaf
        , Node <$> (Gen.int Range.constantBounded)
               <*> genTestTreeInt (sz `div` 2)
               <*> genTestTreeInt (sz `div` 2)
        ]

prop_round_trip_derived_bi_show_instance_TestTreeInt :: Property
prop_round_trip_derived_bi_show_instance_TestTreeInt
  = mk_prop_round_trip_derived_bi_show_instance 5000
                                                (Gen.sized genTestTreeInt)
prop_round_trip_derived_bi_buildable_instance_TestTreeInt :: Property
prop_round_trip_derived_bi_buildable_instance_TestTreeInt
  = mk_prop_round_trip_derived_bi_buildable_instance 5000
                                                     (Gen.sized genTestTreeInt)


--------------------------------------------------------------------------------

data Unit0 = Unit0
    deriving (Eq, Show, Typeable)

deriveIndexedBi ''Unit0 [
    Cons 'Unit0 []
    ]

instance Buildable Unit0 where
    build = Builder.fromString . show

prop_golden_Unit0 :: Property
prop_golden_Unit0 =
    H.withTests 1 . H.property $
        B16.encode (serialize' $ Unit0)
            === "80"

--------------------------------------------------------------------------------

data Unit1Int = Unit1Int Int
    deriving (Eq, Show, Typeable)

deriveIndexedBi ''Unit1Int [
    Cons 'Unit1Int [
        Field [| 0 :: Int |]
        ]
    ]

instance Buildable Unit1Int where
    build = Builder.fromString . show

prop_golden_Unit1Int :: Property
prop_golden_Unit1Int =
    H.withTests 1 . H.property $
        B16.encode (serialize' $ Unit1Int 3)
            === "8103"

genUnit1Int :: Gen Unit1Int
genUnit1Int = Unit1Int <$> Gen.int Range.constantBounded

prop_round_trip_derived_bi_show_instance_Unit1Int :: Property
prop_round_trip_derived_bi_show_instance_Unit1Int
  = mk_prop_round_trip_derived_bi_show_instance 5000 genUnit1Int
prop_round_trip_derived_bi_buildable_instance_Unit1Int :: Property
prop_round_trip_derived_bi_buildable_instance_Unit1Int
  = mk_prop_round_trip_derived_bi_buildable_instance 5000 genUnit1Int

--------------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
