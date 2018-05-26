import           Universum

import           Test.Hspec (hspec)

import           Spec (spec)

import           Test.Pos.Binary.Tripping (runTests)
import qualified Test.Pos.Binary.Simple
import qualified Test.Pos.Binary.Indexed

main :: IO ()
main = do
    hspec spec
    runTests
        [ Test.Pos.Binary.Simple.tests
        , Test.Pos.Binary.Indexed.tests
        ]

