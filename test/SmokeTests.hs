module SmokeTests (runSmokeTests) where

import Kbgen
    ( Call (..)
    , CallMethodOf1stPartyImportedName (..)
    , FieldReadOf1stPartyImportedName (..)
    , FieldName (..)
    , CallResolved (..)
    , ImportedFromFile (..)
    , ImportedName (..)
    , MethodName (..)
    , prologifyFqn
    , CallGuardedByCondInElsePart (..)
    , CallGuardedByCondInThenPart (..)
    , Callable (..)
    , Cond (..)
    , ConstNull (..)
    , ConstStr (..)
    , ConstString (..)
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

pathUtil :: Fqn.Fqn
pathUtil = Fqn.FirstPartyImport (Fqn.FirstPartyImportContent "util/PathUtil.java" (Just "PathUtil"))

member :: String -> Token.FieldName
member name = Token.FieldName (Token.Named name (sampleLoc 2))

-- no Haskell `show` leaks into a kb atom
testPrologifyCallResolved1stPartyImport :: IO ()
testPrologifyCallResolved1stPartyImport =
    let callLoc = sampleLoc 1
        c = CallResolved (Call callLoc) (Resolved (Fqn.FieldedAccess pathUtil (member "normalize")))
        expected = "kb_call_resolved( " ++ locationify callLoc ++ ", '<1st-party>[util/PathUtil.java].PathUtil.normalize' )."
    in prologify (CallResolvedCtor c) `shouldBe` expected

testPrologifyFqnShapes :: IO ()
testPrologifyFqnShapes =
    let klass = Token.ClassName (Token.Named "Foo" (sampleLoc 3))
        param = Token.ParamName (Token.Named "res" (sampleLoc 4))
    in map prologifyFqn
        [ Fqn.NativeTypeString
        , Fqn.NativeTypeConstInt 7
        , Fqn.ClassName (Fqn.ClassNameContent klass)
        , Fqn.FieldedAccess (Fqn.ClassInstance (Fqn.ClassInstanceContent Fqn.This klass)) (member "m")
        , Fqn.FieldedAccess (Fqn.CallMethodOfClass (sampleLoc 5) "build" klass) (member "run")
        , Fqn.CallMethodOfUntypedNamedParam (sampleLoc 5) "sendFile" param
        , Fqn.FieldedAccess (Fqn.CallFuncFromImportedFile (sampleLoc 5) "helper" "lib/a.ts") (member "x")
        , Fqn.FirstPartyDirImport (Fqn.FirstPartyDirImportContent "pkg/util" Nothing)
        , Fqn.FieldedAccess Fqn.Unknown (member "to_i")
        ] `shouldBe`
        [ "string"
        , "int"
        , "<1st-party-class>[Foo]"
        , "<this>[Foo].m"
        , "<1st-party-class>[Foo].build.run"
        , "<param>[res].sendFile"
        , "<1st-party>[lib/a.ts].helper.x"
        , "<1st-party-dir>[pkg/util]"
        , "Unknown.to_i"
        ]

testPrologifyCallMethodOf1stPartyImportedName :: IO ()
testPrologifyCallMethodOf1stPartyImportedName =
    let callLoc = sampleLoc 1
        c = CallMethodOf1stPartyImportedName (Call callLoc) (MethodName "normalize") (ImportedName "PathUtil") (ImportedFromFile "util/PathUtil.java")
        expected = "kb_call_method_of_1st_party_imported_name( " ++ locationify callLoc ++ ", 'normalize', 'PathUtil', 'util/PathUtil.java' )."
    in prologify (CallMethodOf1stPartyImportedNameCtor c) `shouldBe` expected

testPrologifyFieldReadOf1stPartyImportedName :: IO ()
testPrologifyFieldReadOf1stPartyImportedName =
    let readLoc = sampleLoc 1
        r = FieldReadOf1stPartyImportedName (FieldRead readLoc) (FieldName "class") (ImportedName "PathUtil") (ImportedFromFile "src/Util/PathUtil.php")
        expected = "kb_field_read_of_1st_party_imported_name( " ++ locationify readLoc ++ ", 'class', 'PathUtil', 'src/Util/PathUtil.php' )."
    in prologify (FieldReadOf1stPartyImportedNameCtor r) `shouldBe` expected

-- a capitalized method name ( C# ) must not read as a Prolog variable
testPrologifyCallResolvedMethodOfClassQuoted :: IO ()
testPrologifyCallResolvedMethodOfClassQuoted =
    let callLoc = sampleLoc 1
        klass = Token.ClassName (Token.Named "BackupController" (sampleLoc 3))
        c = CallResolved (Call callLoc) (Resolved (Fqn.CallMethodOfClass (sampleLoc 5) "GetBackup" klass))
        expected = "kb_call_method_of_class( " ++ locationify callLoc ++ ", 'GetBackup', " ++ locationify (sampleLoc 3) ++ " )."
    in prologify (CallResolvedCtor c) `shouldBe` expected

testPrologifyCallResolvedEscapesQuote :: IO ()
testPrologifyCallResolvedEscapesQuote =
    let callLoc = sampleLoc 1
        odd' = Fqn.FirstPartyImport (Fqn.FirstPartyImportContent "it's/a.rb" (Just "DB"))
        c = CallResolved (Call callLoc) (Resolved (Fqn.FieldedAccess odd' (member "exec")))
        expected = "kb_call_resolved( " ++ locationify callLoc ++ ", '<1st-party>[it\\'s/a.rb].DB.exec' )."
    in prologify (CallResolvedCtor c) `shouldBe` expected

-- a multi-line string constant must stay one kb line ( dhscanner.runner#236 )
testPrologifyConstStringEscapesLineBreaks :: IO ()
testPrologifyConstStringEscapesLineBreaks =
    let loc = sampleLoc 1
        value = "# first\nsecond\r\nit's \\ third"
        c = ConstString (ConstStr loc) (Token.ConstStr value (sampleLoc 1))
        expected = "kb_const_string( " ++ locationify loc ++ ", '# first\\nsecond\\r\\nit\\'s \\\\ third' )."
    in prologify (ConstStringCtor c) `shouldBe` expected

tests :: Spec
tests = do
    it' testPrologifyConstStringEscapesLineBreaks "kb_const_string escapes line breaks, so one fact = one line"
    it' testPrologifyCallResolved1stPartyImport "kb_call_resolved of a 1st party import member is tagged, not Haskell show"
    it' testPrologifyFqnShapes "prologifyFqn spells every fqn shape without Haskell show"
    it' testPrologifyCallMethodOf1stPartyImportedName "prologify_CallMethodOf1stPartyImportedName should render kb_call_method_of_1st_party_imported_name( <call>, '<method>', '<name>', '<file>' )."
    it' testPrologifyFieldReadOf1stPartyImportedName "prologify_FieldReadOf1stPartyImportedName should render kb_field_read_of_1st_party_imported_name( <field read>, '<field>', '<name>', '<file>' )."
    it' testPrologifyCallResolvedMethodOfClassQuoted "kb_call_method_of_class quotes the method name"
    it' testPrologifyCallResolvedEscapesQuote "kb_call_resolved escapes a quote in the fqn"
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
