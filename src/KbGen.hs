{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -Werror=missing-fields #-}

module KbGen

where

-- project imports
import Fqn
import Location
import Callable
import Cfg ( Cfg )

-- project qualified imports
import qualified Cfg
import qualified Token
import qualified Bitcode

-- general imports
import Data.Set
import Data.List
import Data.Maybe
import Data.Aeson

-- general imports (with hidden names)
import GHC.Generics hiding ( from, to )

data KnowledgeBase
   = KnowledgeBase
     {
          calls :: [ Call ],
          args :: [ Arg ],
          lambdas :: [ KBLambda ],
          params :: [ Param ],
          dataflow :: [ Edge ],
          funcs :: [ KBCallable ]
     }
     deriving ( Show, Generic, ToJSON )

data Edge
   = Edge
     {
         from :: Bitcode.Variable,
         to :: Bitcode.Variable
     }
     deriving ( Show, Generic, ToJSON )

data Call
   = Call
     {
         calleeFqn :: Fqn,
         callLocation :: Location
     }
     deriving ( Show, Generic, ToJSON )

data Arg
   = Arg
     {
         argFqn :: Fqn,
         argSerialIdx :: Word,
         argLocation :: Location,
         callContext :: Location
     }
     deriving ( Show, Generic, ToJSON )

data KBLambda
   = KBLambda
     {
         lambdaLocation :: Location
     }
     deriving ( Show, Generic, ToJSON )

data KBCallable
   = KBCallable
     {
         callableFqn :: Fqn,
         funcLocation :: Location
     }
     deriving ( Show, Generic, ToJSON )


data Param
   = Param
     {
         paramSerialIdx :: Word,
         paramName :: Token.ParamName,
         callableContext :: Location
     }
     deriving ( Show, Generic, ToJSON )

emptyKnowledgeBase :: KnowledgeBase
emptyKnowledgeBase = KnowledgeBase [] [] [] [] [] []

-- | API: generate a prolog knowledge base from
-- a collection of callables
kbGen :: Callables -> KnowledgeBase
kbGen = kbGen' . actualCallables

kbGen' :: [ Callable ] -> KnowledgeBase
kbGen' [] = emptyKnowledgeBase
kbGen' (c:cs) = let
    kb = kbGen'' c
    rest = kbGen' cs
    calls' = (calls kb) ++ (calls rest)
    args' = (args kb) ++ (args rest)
    lambdas' = (lambdas kb) ++ (lambdas rest)
    params' = (params kb) ++ (params rest)
    dataflow' = (dataflow kb) ++ (dataflow rest)
    funcs' = (funcs kb) ++ (funcs rest)
    in KnowledgeBase calls' args' lambdas' params' dataflow' funcs' 

kbGen'' :: Callable -> KnowledgeBase
kbGen'' (Callable.Script script) = kbGenScript script
kbGen'' (Callable.Lambda lambda) = kbGenLambda lambda
kbGen'' (Callable.Method method) = kbGenMethod method
kbGen'' (Callable.Function func) = kbGenFunction func

kbGenMethod:: Callable.MethodContent -> KnowledgeBase
kbGenMethod method = let
    calls' = kbGenCalls (Callable.methodBody method)
    args' = kbGenArgs (Callable.methodBody method)
    params' = extractMethodParams method
    dataflow' = extractMethodDataflow method
    funcs' = [ KBCallable (fqnifyMethod method) (Callable.methodLocation method) ]
    in KnowledgeBase calls' args' [] params' dataflow' funcs' 

kbGenFunction :: Callable.FunctionContent -> KnowledgeBase
kbGenFunction func = let
    calls' = kbGenCalls (Callable.funcBody func)
    args' = kbGenArgs (Callable.funcBody func)
    params' = extractFunctionParams func
    dataflow' = extractFunctionDataflow func
    funcs' = [ KBCallable (fqnifyFunc func) (Callable.funcLocation func) ]
    in KnowledgeBase calls' args' [] params' dataflow' funcs' 

kbGenScript :: Callable.ScriptContent -> KnowledgeBase
kbGenScript script = let
    calls' = kbGenScriptCalls script
    args' = kbGenScriptArgs script
    in KnowledgeBase calls' args' [] [] [] []

kbGenLambda :: Callable.LambdaContent -> KnowledgeBase
kbGenLambda lambda = let
    calls' = kbGenCalls (Callable.lambdaBody lambda)
    args' = kbGenArgs (Callable.lambdaBody lambda)
    lambdas' = [ KBLambda (Callable.lambdaLocation lambda) ]
    params' = extractLambdaParams lambda
    dataflow' = extractLambdaDataflow lambda
    in KnowledgeBase calls' args' lambdas' params' dataflow' []

fqnifyMethod :: Callable.MethodContent -> Fqn
fqnifyMethod method = let
    rawMethodName = Token.content (Token.getMethdNameToken (Callable.methodName method))
    hostingClassName = Token.content (Token.getClassNameToken (Callable.hostingClassName method))
    in Fqn (hostingClassName ++ "." ++ rawMethodName)

fqnifyFunc :: Callable.FunctionContent -> Fqn
fqnifyFunc func = let
    funcName = Token.content (Token.getFuncNameToken (Callable.funcName func))
    in Fqn funcName

extractFunctionDataflow :: Callable.FunctionContent -> [ Edge ]
extractFunctionDataflow func = extractDataflow (Callable.funcBody func)

extractMethodDataflow :: Callable.MethodContent -> [ Edge ]
extractMethodDataflow method = extractDataflow (Callable.methodBody method)

extractLambdaDataflow :: Callable.LambdaContent -> [ Edge ]
extractLambdaDataflow lambda = extractDataflow (Callable.lambdaBody lambda)

extractDataflow :: Cfg -> [ Edge ]
extractDataflow cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    in Data.List.foldl' (++) [] (dataflowEdges (Data.Set.toList instructions))

dataflowEdges :: [ Bitcode.InstructionContent ] -> [[ Edge ]]
dataflowEdges = Data.List.map dataflowEdges'

dataflowEdges' :: Bitcode.InstructionContent -> [ Edge ]
dataflowEdges' (Bitcode.Call call)           = dataflowCallEdges call
dataflowEdges' (Bitcode.Binop binop)         = dataflowBinopEdge binop
dataflowEdges' (Bitcode.Assign assign)       = [ dataflowAssignEdge assign ]
dataflowEdges' (Bitcode.FieldRead fieldRead) = [ dataflowFieldReadEdge fieldRead ]
dataflowEdges' _ = []

dataflowCallEdges :: Bitcode.CallContent -> [ Edge ]
dataflowCallEdges c = let
    everyArgToOutput = dataflowCallArgsEdges (Bitcode.callOutput c) (Bitcode.args c)
    calleeToOutput = edgify (Bitcode.callOutput c) (Bitcode.callee c)
    in calleeToOutput : everyArgToOutput 

dataflowCallArgsEdges :: Bitcode.Variable -> [ Bitcode.Variable ] -> [ Edge ]
dataflowCallArgsEdges output = Data.List.map $ edgify output

edgify :: Bitcode.Variable -> Bitcode.Variable -> Edge
edgify output arg = Edge { from = arg, to = output }

dataflowBinopEdge :: Bitcode.BinopContent -> [ Edge ]
dataflowBinopEdge b = let
    e1 = Edge { from = Bitcode.binopLhs b, to = Bitcode.binopOutput b }
    e2 = Edge { from = Bitcode.binopRhs b, to = Bitcode.binopOutput b }
    in [ e1, e2 ]

dataflowAssignEdge :: Bitcode.AssignContent -> Edge
dataflowAssignEdge a = Edge { from = Bitcode.assignInput a, to = Bitcode.assignOutput a }

dataflowFieldReadEdge :: Bitcode.FieldReadContent -> Edge
dataflowFieldReadEdge f = Edge { from = Bitcode.fieldReadInput f, to = Bitcode.fieldReadOutput f } 

extractLambdaParams :: Callable.LambdaContent -> [ Param ]
extractLambdaParams lambda = extractParams (Callable.lambdaLocation lambda) (Callable.lambdaBody lambda)

extractFunctionParams :: Callable.FunctionContent -> [ Param ]
extractFunctionParams f = extractParams (Token.getFuncNameLocation (Callable.funcName f)) (Callable.funcBody f)

extractMethodParams :: Callable.MethodContent -> [ Param ]
extractMethodParams m = extractParams (Callable.methodLocation m) (Callable.methodBody m)

extractParams :: Location -> Cfg -> [ Param ]
extractParams location cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    paramDecls = catMaybes (Data.Set.toList (Data.Set.map keepParamDecls instructions))
    in (toParams location paramDecls) 

toParams :: Location -> [ Bitcode.ParamDeclContent ] -> [ Param ]
toParams location = Data.List.map (toParam location)

toParam :: Location -> Bitcode.ParamDeclContent -> Param
toParam l (Bitcode.ParamDeclContent p) = Param (Bitcode.paramVariableSerialIdx p) (Bitcode.paramVariableToken p) l 

keepParamDecls :: Bitcode.InstructionContent -> Maybe Bitcode.ParamDeclContent
keepParamDecls (Bitcode.ParamDecl paramDecl) = Just paramDecl
keepParamDecls _ = Nothing

removeNondetCalls :: Bitcode.Variable -> Bitcode.CallContent -> Maybe Bitcode.CallContent
removeNondetCalls (Bitcode.SrcVariableCtor _) _ = Nothing
removeNondetCalls _ call = Just call

keepCalls :: Bitcode.InstructionContent -> Maybe Bitcode.CallContent
keepCalls (Bitcode.Call call) = removeNondetCalls (Bitcode.callee call) call
keepCalls _ = Nothing

combine :: [ Fqn ] -> [ Location ] -> [ Call ]
combine [] _ = []
combine _ [] = []
combine (fqn:fqns) (loc:locs) = (Call fqn loc) : (combine fqns locs)

kbGenScriptCalls :: Callable.ScriptContent -> [ Call ]
kbGenScriptCalls = kbGenCalls . Callable.scriptBody

kbGenCalls :: Cfg -> [ Call ]
kbGenCalls cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    calls = catMaybes (Data.Set.toList (Data.Set.map keepCalls instructions))
    callees = Data.List.map Bitcode.callee calls
    fqns = Data.List.map Bitcode.variableFqn callees
    locations = Data.List.map Bitcode.callLocation calls
    in combine fqns locations

kbGenScriptArgs :: Callable.ScriptContent -> [ Arg ]
kbGenScriptArgs = kbGenArgs . Callable.scriptBody 

kbGenArgs :: Cfg -> [ Arg ]
kbGenArgs cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes) 
    calls = catMaybes (Data.Set.toList (Data.Set.map keepCalls instructions))
    in kbGenScriptArgsFromCalls calls 

kbGenScriptArgsFromCalls :: [ Bitcode.CallContent ] -> [ Arg ]
kbGenScriptArgsFromCalls calls = Data.List.foldl' (++) [] (kbGenScriptArgsFromCalls' calls)

kbGenScriptArgsFromCalls' :: [ Bitcode.CallContent ] -> [[ Arg ]]
kbGenScriptArgsFromCalls' = Data.List.map kbGenScriptArgsFromCall

kbGenScriptArgsFromCall :: Bitcode.CallContent -> [ Arg ]
kbGenScriptArgsFromCall call = kbGenScriptArgsFromCall' 0 (Bitcode.callLocation call) (Bitcode.args call)

kbGenScriptArgsFromCall' :: Word -> Location -> [ Bitcode.Variable ] -> [ Arg ]
kbGenScriptArgsFromCall' _ location [] = []
kbGenScriptArgsFromCall' i location (arg:args) = let
    a = Arg (Bitcode.variableFqn arg) i (Bitcode.locationVariable arg) location
    in a : (kbGenScriptArgsFromCall' (i+1) location args)

