module SmokeTests (runSmokeTests) where

import Kbgen
    ( Call (..)
    , CallGuardedByCondInElsePart (..)
    , CallGuardedByCondInThenPart (..)
    , Callable (..)
    , Cond (..)
    , ConstNull (..)
    , ElsePart (..)
    , Fact (..)
    , FieldRead (..)
    , FieldReadResolved (..)
    , GatedReturn (..)
    , Method (..)
    , MethodHasName (..)
    , Resolved (..)
    , ReturnedValue (..)
    , ThenPart (..)
    , TopLevelIfThenElseGadget (..)
    , locationify
    , prologify
    , restoreloc
    )
import qualified Fqn
import Location (Location (..))
import qualified Token
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

testPrologifyMethodHasName :: IO ()
testPrologifyMethodHasName =
    let methodLoc = sampleLoc 1
        name = Token.MethodName (Token.Named "oidc" (sampleLoc 2))
        mn = MethodHasName (Method methodLoc) name
        expected = "kb_method_has_name( " ++ locationify methodLoc ++ ", 'oidc' )."
    in prologify (MethodHasNameCtor mn) `shouldBe` expected

testPrologifyFieldReadResolved :: IO ()
testPrologifyFieldReadResolved =
    let readLoc = sampleLoc 1
        requestMethod = Fqn.ThirdPartyImport (Fqn.ThirdPartyImportContent "org" ["springframework", "web", "bind", "annotation"] (Just "RequestMethod") Nothing)
        get = Token.FieldName (Token.Named "GET" (sampleLoc 2))
        r = FieldReadResolved (FieldRead readLoc) (Resolved (Fqn.FieldedAccess requestMethod get))
        expected = "kb_field_read_resolved( " ++ locationify readLoc ++ ", 'org.springframework.web.bind.annotation.RequestMethod.GET' )."
    in prologify (FieldReadResolvedCtor r) `shouldBe` expected

testPrologifyTopLevelIfThenElseGadget :: IO ()
testPrologifyTopLevelIfThenElseGadget =
    let callableLoc = sampleLoc 1
        condLoc = sampleLoc 2
        g = TopLevelIfThenElseGadget (Callable callableLoc) (Cond condLoc) (ThenPart condLoc) (ElsePart condLoc)
        expected = "kb_top_level_if_then_else_gadget( " ++ locationify callableLoc ++ ", " ++ locationify condLoc ++ ", then_part_" ++ locationify condLoc ++ ", else_part_" ++ locationify condLoc ++ " )."
    in prologify (TopLevelIfThenElseGadgetCtor g) `shouldBe` expected

testPrologifyCallGuardedByCondInThenPart :: IO ()
testPrologifyCallGuardedByCondInThenPart =
    let callLoc = sampleLoc 1
        condLoc = sampleLoc 2
        c = CallGuardedByCondInThenPart (Call callLoc) (Cond condLoc) (ThenPart condLoc)
        expected = "kb_call_guarded_by_cond_in_then_part( " ++ locationify callLoc ++ ", " ++ locationify condLoc ++ ", then_part_" ++ locationify condLoc ++ " )."
    in prologify (CallGuardedByCondInThenPartCtor c) `shouldBe` expected

testPrologifyCallGuardedByCondInElsePart :: IO ()
testPrologifyCallGuardedByCondInElsePart =
    let callLoc = sampleLoc 1
        condLoc = sampleLoc 2
        c = CallGuardedByCondInElsePart (Call callLoc) (Cond condLoc) (ElsePart condLoc)
        expected = "kb_call_guarded_by_cond_in_else_part( " ++ locationify callLoc ++ ", " ++ locationify condLoc ++ ", else_part_" ++ locationify condLoc ++ " )."
    in prologify (CallGuardedByCondInElsePartCtor c) `shouldBe` expected

tests :: Spec
tests = do
    it' testCompletelyInvalidLocationString "completelyInvalidLocationString should return Nothing"
    it' testLocationStringWithoutProperPrefix "locationStringWithoutProperPrefix should return Nothing"
    it' testLocationStringWithoutProperFormatExample1 "locationStringWithoutProperFormatExample1 should return Nothing"
    it' testLocationStringWithoutProperFormatExample2 "locationStringWithoutProperFormatExample2 should return Nothing"
    it' testLocationStringWithoutProperFormatExample3 "locationStringWithoutProperFormatExample3 should return Nothing"
    it' testPrologifyConstNull "prologify_ConstNull should render kb_const_null( <loc> )."
    it' testPrologifyGatedReturn "prologify_GatedReturn should render kb_gated_return( <cond>, <ret> )."
    it' testPrologifyMethodHasName "prologify_MethodHasName should render kb_method_has_name( <method>, '<name>' )."
    it' testPrologifyFieldReadResolved "prologify_FieldReadResolved should render kb_field_read_resolved( <field read>, '<fqn>' )."
    it' testPrologifyTopLevelIfThenElseGadget "prologify_TopLevelIfThenElseGadget should render kb_top_level_if_then_else_gadget( <callable>, <cond>, then_part_<cond>, else_part_<cond> )."
    it' testPrologifyCallGuardedByCondInThenPart "prologify_CallGuardedByCondInThenPart should render kb_call_guarded_by_cond_in_then_part( <call>, <cond>, then_part_<cond> )."
    it' testPrologifyCallGuardedByCondInElsePart "prologify_CallGuardedByCondInElsePart should render kb_call_guarded_by_cond_in_else_part( <call>, <cond>, else_part_<cond> )."

runSmokeTests :: IO ()
runSmokeTests = hspec tests
