module SmokeTests (runSmokeTests) where

import Kbgen
    ( Cond (..)
    , ConstNull (..)
    , Fact (..)
    , GatedReturn (..)
    , ReturnedValue (..)
    , locationify
    , prologify
    , restoreloc
    )
import Location (Location (..))
import Test.Hspec (Spec, hspec, it, shouldBe)

it' :: IO () -> String -> Spec
it' testBody label = it label testBody

completelyInvalidLocationString :: String
completelyInvalidLocationString = "Blahhh"

testCompletelyInvalidLocationString :: IO ()
testCompletelyInvalidLocationString = restoreloc completelyInvalidLocationString `shouldBe` Nothing

locationStringWithoutProperPrefix :: String
locationStringWithoutProperPrefix = "____startloc_1_2_endloc_3_4_foo_dot_c"

testLocationStringWithoutProperPrefix :: IO ()
testLocationStringWithoutProperPrefix = restoreloc locationStringWithoutProperPrefix `shouldBe` Nothing

locationStringWithoutProperFormatExample1 :: String
locationStringWithoutProperFormatExample1 = "startloc_1_2_end_3_4_foo_dot_c"

testLocationStringWithoutProperFormatExample1 :: IO ()
testLocationStringWithoutProperFormatExample1 = restoreloc locationStringWithoutProperFormatExample1 `shouldBe` Nothing

locationStringWithoutProperFormatExample2 :: String
locationStringWithoutProperFormatExample2 = "startloc_1_2_endloc____3_4_foo_dot_c"

testLocationStringWithoutProperFormatExample2 :: IO ()
testLocationStringWithoutProperFormatExample2 = restoreloc locationStringWithoutProperFormatExample2 `shouldBe` Nothing

locationStringWithoutProperFormatExample3 :: String
locationStringWithoutProperFormatExample3 = "startloc_1_2_endloc_3______4_foo_dot_c"

testLocationStringWithoutProperFormatExample3 :: IO ()
testLocationStringWithoutProperFormatExample3 = restoreloc locationStringWithoutProperFormatExample3 `shouldBe` Nothing

sampleLoc :: Word -> Location
sampleLoc n =
    Location
        { lineStart = n
        , colStart = n
        , lineEnd = n
        , colEnd = n
        , filename = "foo.c"
        }

testPrologifyConstNull :: IO ()
testPrologifyConstNull =
    let loc = sampleLoc 1
        expected = "kb_const_null( " ++ locationify loc ++ " )."
    in prologify (ConstNullCtor (ConstNull loc)) `shouldBe` expected

testPrologifyGatedReturn :: IO ()
testPrologifyGatedReturn =
    let condLoc = sampleLoc 1
        retLoc = sampleLoc 2
        gr = GatedReturn (Cond condLoc) (ReturnedValue retLoc)
        expected = "kb_gated_return( " ++ locationify condLoc ++ ", " ++ locationify retLoc ++ " )."
    in prologify (GatedReturnCtor gr) `shouldBe` expected

tests :: Spec
tests = do
    it' testCompletelyInvalidLocationString "completelyInvalidLocationString should return Nothing"
    it' testLocationStringWithoutProperPrefix "locationStringWithoutProperPrefix should return Nothing"
    it' testLocationStringWithoutProperFormatExample1 "locationStringWithoutProperFormatExample1 should return Nothing"
    it' testLocationStringWithoutProperFormatExample2 "locationStringWithoutProperFormatExample2 should return Nothing"
    it' testLocationStringWithoutProperFormatExample3 "locationStringWithoutProperFormatExample3 should return Nothing"
    it' testPrologifyConstNull "prologify_ConstNull should render kb_const_null( <loc> )."
    it' testPrologifyGatedReturn "prologify_GatedReturn should render kb_gated_return( <cond>, <ret> )."

runSmokeTests :: IO ()
runSmokeTests = hspec tests
