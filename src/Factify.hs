module Factify

where

-- general imports
import Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe ( mapMaybe, catMaybes )
import qualified Data.Foldable as Foldable

-- project imports
import Cfg
import Callable
import qualified Token
import qualified Kbgen
import qualified Bitcode

factify :: Callable -> Set Kbgen.Fact
factify (Method m) = factifyMethod m
factify (Lambda l) = factifyLambda l
factify (Script s) = factifyScript s
factify (Function f) = factifyFunc f

factifyScript :: Callable.ScriptContent -> Set Kbgen.Fact
factifyScript s = List.foldl' Set.union Set.empty (factifyScript' s)

factifyScript' :: Callable.ScriptContent -> [ Set Kbgen.Fact ]
factifyScript' s = [
        getCallsRelatedFacts (Callable.scriptBody s),
        getConstStringsRelatedFacts (Callable.scriptBody s)
    ]

factifyLambda :: Callable.LambdaContent -> Set Kbgen.Fact
factifyLambda l = List.foldl' Set.union Set.empty (factifyLambda' l)

factifyLambda' :: Callable.LambdaContent -> [ Set Kbgen.Fact ]
factifyLambda' l = [
        getCallsRelatedFacts (Callable.lambdaBody l),
        getParamsRelatedFacts (Kbgen.Callable (Callable.lambdaLocation l)) (Callable.lambdaBody l),
        getConstStringsRelatedFacts (Callable.lambdaBody l)
    ]

factifyFunc :: Callable.FunctionContent -> Set Kbgen.Fact
factifyFunc f = List.foldl' Set.union Set.empty (factifyFunc' f)

factifyFunc' :: Callable.FunctionContent -> [ Set Kbgen.Fact ]
factifyFunc' f = [
        getCallsRelatedFacts (Callable.funcBody f),
        getParamsRelatedFacts (Kbgen.Callable (Callable.funcLocation f)) (Callable.funcBody f),
        getConstStringsRelatedFacts (Callable.funcBody f)
    ]

factifyMethod :: Callable.MethodContent -> Set Kbgen.Fact
factifyMethod m = List.foldl' Set.union Set.empty (factifyMethod' m)

factifyMethod' :: Callable.MethodContent -> [ Set Kbgen.Fact ]
factifyMethod' m = [
        getClassRelatedFacts m,
        getCallsRelatedFacts (Callable.methodBody m),
        getParamsRelatedFacts (Kbgen.Callable (Callable.methodLocation m)) (Callable.methodBody m),
        getConstStringsRelatedFacts (Callable.methodBody m)
    ]

getConstStringsRelatedFacts :: Cfg -> Set Kbgen.Fact
getConstStringsRelatedFacts = getConstStringsRelatedFacts' . instructions

getConstStringsRelatedFacts' :: Set Bitcode.Instruction -> Set Kbgen.Fact
getConstStringsRelatedFacts' = Foldable.foldl' getConstStringsRelatedFacts'' Set.empty

getConstStringsRelatedFacts'' :: Set Kbgen.Fact -> Bitcode.Instruction -> Set Kbgen.Fact
getConstStringsRelatedFacts'' acc i = Set.union acc (getConstStringsRelatedFacts''' i)

getConstStringsRelatedFacts''' :: Bitcode.Instruction -> Set Kbgen.Fact
getConstStringsRelatedFacts''' (Bitcode.Instruction _ i) = getConstStringsRelatedFacts'''' i

getConstStringsRelatedFacts'''' :: Bitcode.InstructionContent -> Set Kbgen.Fact
getConstStringsRelatedFacts'''' (Bitcode.Call c) = getConstStringsRelatedFactsFromCall c
getConstStringsRelatedFacts'''' (Bitcode.Unop u) = getConstStringsRelatedFactsFromUnop u
getConstStringsRelatedFacts'''' (Bitcode.Binop b) = getConstStringsRelatedFactsFromBinop b
getConstStringsRelatedFacts'''' (Bitcode.Return r) = getConstStringsRelatedFactsFromReturn r
getConstStringsRelatedFacts'''' (Bitcode.Assign a) = getConstStringsRelatedFactsFromAssign a
getConstStringsRelatedFacts'''' (Bitcode.FieldWrite f) = getConstStringsRelatedFactsFromFieldWrite f
getConstStringsRelatedFacts'''' (Bitcode.SubscriptRead s) = getConstStringsRelatedFactsFromSubscriptRead s
getConstStringsRelatedFacts'''' (Bitcode.SubscriptWrite s) = getConstStringsRelatedFactsFromSubscriptWrite s
getConstStringsRelatedFacts'''' _ = Set.empty

getConstStringsRelatedFactsFromCall :: Bitcode.CallContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromCall = getConstStringsFromValues . Bitcode.args

getConstStringsRelatedFactsFromUnop :: Bitcode.UnopContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromUnop = getConstStringsFromValue . Bitcode.unopLhs

getConstStringsRelatedFactsFromBinop :: Bitcode.BinopContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromBinop b = let
    lhs = getConstStringsFromValue (Bitcode.binopLhs b)
    rhs = getConstStringsFromValue (Bitcode.binopRhs b)
    in Set.union lhs rhs

getConstStringsRelatedFactsFromReturn :: Bitcode.ReturnContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromReturn (Bitcode.ReturnContent Nothing) = Set.empty
getConstStringsRelatedFactsFromReturn (Bitcode.ReturnContent (Just v)) = getConstStringsFromValue v

getConstStringsRelatedFactsFromAssign :: Bitcode.AssignContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromAssign = getConstStringsFromValue . Bitcode.assignInput

getConstStringsRelatedFactsFromFieldWrite :: Bitcode.FieldWriteContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromFieldWrite = getConstStringsFromValue . Bitcode.fieldWriteInput

getConstStringsRelatedFactsFromSubscriptRead :: Bitcode.SubscriptReadContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromSubscriptRead = getConstStringsFromValue . Bitcode.subscriptReadIdx

getConstStringsRelatedFactsFromSubscriptWrite :: Bitcode.SubscriptWriteContent -> Set Kbgen.Fact
getConstStringsRelatedFactsFromSubscriptWrite s = let
    index = getConstStringsFromValue (Bitcode.subscriptWriteIdx s)
    value = getConstStringsFromValue (Bitcode.subscriptWriteInput s)
    in Set.union index value

getConstStringsFromValues :: [ Bitcode.Value ] -> Set Kbgen.Fact
getConstStringsFromValues = List.foldl' Set.union Set.empty . map getConstStringsFromValue

getConstStringsFromValue :: Bitcode.Value -> Set Kbgen.Fact
getConstStringsFromValue (Bitcode.ConstValueCtor (Bitcode.ConstStrValue s)) = getConstStringsFromValue' s
getConstStringsFromValue _ = Set.empty

getConstStringsFromValue' :: Token.ConstStr -> Set Kbgen.Fact
getConstStringsFromValue' value = let
    location = Kbgen.ConstStr (Token.constStrLocation value)
    constString = Kbgen.ConstString location value
    in Set.singleton (Kbgen.ConstStringCtor constString)

getParamsRelatedFacts :: Kbgen.Callable -> Cfg -> Set Kbgen.Fact
getParamsRelatedFacts callable body = getParamsRelatedFacts' callable (justParams (instructions body))

getParamsRelatedFacts' :: Kbgen.Callable -> Set Bitcode.ParamDeclContent -> Set Kbgen.Fact
getParamsRelatedFacts' callable = Foldable.foldl' (getParamsRelatedFacts'' callable) Set.empty

getParamsRelatedFacts'' :: Kbgen.Callable -> Set Kbgen.Fact -> Bitcode.ParamDeclContent -> Set Kbgen.Fact
getParamsRelatedFacts'' c facts p = Set.union facts (Set.fromList (getParamsRelatedFacts''' c p))

getParamsRelatedFacts''' :: Kbgen.Callable -> Bitcode.ParamDeclContent -> [ Kbgen.Fact ]
getParamsRelatedFacts''' callable param = [
        getParamNameFact param,
        getParamResolvedTypeFact param,
        getParamiOfCallableFact param callable
    ]

getParamNameFact :: Bitcode.ParamDeclContent -> Kbgen.Fact
getParamNameFact (Bitcode.ParamDeclContent (Bitcode.ParamVariable _ _ name)) = let
    p = Kbgen.Param (Token.getParamNameLocation name)
    in Kbgen.ParamNameCtor (Kbgen.ParamName p name)

getParamResolvedTypeFact :: Bitcode.ParamDeclContent -> Kbgen.Fact
getParamResolvedTypeFact (Bitcode.ParamDeclContent (Bitcode.ParamVariable t _ name)) = let
    p = Kbgen.Param (Token.getParamNameLocation name)
    in Kbgen.ParamResolvedTypeCtor (Kbgen.ParamResolvedType p (Kbgen.ResolvedType t))

getParamiOfCallableFact :: Bitcode.ParamDeclContent -> Kbgen.Callable -> Kbgen.Fact
getParamiOfCallableFact (Bitcode.ParamDeclContent (Bitcode.ParamVariable _ i name)) c = let
    p = Kbgen.Param (Token.getParamNameLocation name)
    in Kbgen.ParamiOfCallableCtor (Kbgen.ParamiOfCallable p (Kbgen.ParamIndex i) c)

getCallsRelatedFacts :: Cfg -> Set Kbgen.Fact
getCallsRelatedFacts = getCallsRelatedFacts' . justCalls . instructions

getCallsRelatedFacts' :: Set Bitcode.CallContent -> Set Kbgen.Fact
getCallsRelatedFacts' = Foldable.foldMap' getCallsRelatedFacts''

getCallsRelatedFacts'' :: Bitcode.CallContent -> Set Kbgen.Fact
getCallsRelatedFacts'' call = List.foldl' Set.union Set.empty (getCallsRelatedFacts''' call)

getCallsRelatedFacts''' :: Bitcode.CallContent -> [ Set Kbgen.Fact ]
getCallsRelatedFacts''' call = [
        getArgiForCallFacts call,
        getKeywordArgsForCallFacts call
    ]

getKeywordArgsForCallFacts :: Bitcode.CallContent -> Set Kbgen.Fact
getKeywordArgsForCallFacts callContent = let
    call = Kbgen.Call (Bitcode.locationVariable (Bitcode.callee callContent))
    keywordArgs = keepOnlyKeywrodArgs (Bitcode.args callContent)
    in getKeywordArgsForCallFacts' call keywordArgs

getKeywordArgsForCallFacts' :: Kbgen.Call -> [ Bitcode.KeywordArgVariable ] -> Set Kbgen.Fact
getKeywordArgsForCallFacts' call = Set.fromList . getKeywordArgsForCallFacts'' call

getKeywordArgsForCallFacts'' :: Kbgen.Call -> [ Bitcode.KeywordArgVariable ] -> [ Kbgen.Fact ]
getKeywordArgsForCallFacts'' call = List.map (getKeywordArgsForCallFacts''' call)

getKeywordArgsForCallFacts''' :: Kbgen.Call -> Bitcode.KeywordArgVariable -> Kbgen.Fact
getKeywordArgsForCallFacts''' call kwarg = let
    keyword = Kbgen.Keyword (Bitcode.keywordArgName kwarg)
    arg = Kbgen.Arg (Bitcode.locationValue (Bitcode.keywordArgValue kwarg))
    in Kbgen.KeywordArgForCallCtor (Kbgen.KeywordArgForCall keyword arg call)

keepOnlyKeywrodArgs :: [ Bitcode.Value ] -> [ Bitcode.KeywordArgVariable ]
keepOnlyKeywrodArgs = mapMaybe keepOnlyKeywrodArgs'

keepOnlyKeywrodArgs' :: Bitcode.Value -> Maybe Bitcode.KeywordArgVariable
keepOnlyKeywrodArgs' (Bitcode.KeywordArgCtor keywordArg) = Just keywordArg
keepOnlyKeywrodArgs' _ = Nothing

getArgiForCallFacts :: Bitcode.CallContent -> Set Kbgen.Fact
getArgiForCallFacts callContent = let
    call = Kbgen.Call (Bitcode.locationVariable (Bitcode.callee callContent))
    args = List.map (Kbgen.Arg . Bitcode.locationValue) (Bitcode.args callContent)
    argis = zip args (List.map Kbgen.ArgIndex [0..])
    in getArgiForCallFacts' call argis

getArgiForCallFacts' :: Kbgen.Call -> [(Kbgen.Arg, Kbgen.ArgIndex)] -> Set Kbgen.Fact
getArgiForCallFacts' call = Set.fromList . List.map (getArgiForCallFacts'' call)

getArgiForCallFacts'' :: Kbgen.Call -> (Kbgen.Arg, Kbgen.ArgIndex) -> Kbgen.Fact
getArgiForCallFacts'' c (a, i) = Kbgen.ArgiForCallCtor (Kbgen.ArgiForCall a i c)

instructions :: Cfg -> Set Bitcode.Instruction
instructions = Set.map Cfg.theInstructionInside . Cfg.actualNodes . Cfg.nodes

justParams :: Set Bitcode.Instruction -> Set Bitcode.ParamDeclContent
justParams = Foldable.foldMap' justParams'

justParams' :: Bitcode.Instruction -> Set Bitcode.ParamDeclContent
justParams' (Bitcode.Instruction _ (Bitcode.ParamDecl p)) = Set.singleton p
justParams' _ = Set.empty

justCalls :: Set Bitcode.Instruction -> Set Bitcode.CallContent
justCalls = Foldable.foldMap' justCalls'

justCalls' :: Bitcode.Instruction -> Set Bitcode.CallContent
justCalls' (Bitcode.Instruction _ (Bitcode.Call c)) = Set.singleton c
justCalls' _ = Set.empty

getClassRelatedFacts :: Callable.MethodContent -> Set Kbgen.Fact
getClassRelatedFacts m = List.foldl' Set.union Set.empty (getClassRelatedFacts' m)

getClassRelatedFacts' :: Callable.MethodContent -> [ Set Kbgen.Fact ]
getClassRelatedFacts' m = [
        methodOfClass m,
        classNamedSuper m,
        classResolvedSuper m
    ]

methodOfClass :: Callable.MethodContent -> Set Kbgen.Fact
methodOfClass m = let
    className = Callable.hostingClassName m
    c = Kbgen.Class (Token.getClassNameLocation className)
    m' = Kbgen.Method (Callable.methodLocation m)
    in Set.singleton (Kbgen.MethodOfClassCtor (Kbgen.MethodOfClass m' c))

classNamedSuper :: Callable.MethodContent -> Set Kbgen.Fact
classNamedSuper m = let
    className = Callable.hostingClassName m
    c = Kbgen.Class (Token.getClassNameLocation className)
    namedSupers = List.map Callable.hostingClassSuperName (hostingClassSupers m)
    classNamedSupers = List.map (Kbgen.ClassNamedSuper c) namedSupers
    in Set.fromList (List.map Kbgen.ClassNamedSuperCtor classNamedSupers)

classResolvedSuper :: Callable.MethodContent -> Set Kbgen.Fact
classResolvedSuper m = let
    className = Callable.hostingClassName m
    c = Kbgen.Class (Token.getClassNameLocation className)
    resolvedSupers = List.map Callable.hostingClassSuperResolvedType (hostingClassSupers m)
    nonNullResolvedSupers = List.map Kbgen.ResolvedSuper (catMaybes resolvedSupers)
    nonNullResolvedSupers' = List.map (Kbgen.ClassResolvedSuper c) nonNullResolvedSupers
    in Set.fromList (List.map Kbgen.ClassResolvedSuperCtor nonNullResolvedSupers')

collectParamInstruction' ::  Bitcode.Instruction -> Set Bitcode.CallContent -> Set Bitcode.CallContent
collectParamInstruction' (Bitcode.Instruction _ (Bitcode.Call c)) = Set.insert c
collectParamInstruction' _ = id

collectParamInstruction :: Set Bitcode.CallContent -> Bitcode.Instruction -> Set Bitcode.CallContent
collectParamInstruction = flip collectParamInstruction'

params :: Set Bitcode.Instruction -> Set Bitcode.CallContent
params = Foldable.foldl' collectParamInstruction Set.empty
