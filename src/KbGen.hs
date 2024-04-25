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
          dataflow :: [ Edge ]
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

data Param
   = Param
     {
         paramSerialIdx :: Word,
         paramName :: Token.ParamName,
         callableContext :: Location
     }
     deriving ( Show, Generic, ToJSON )

emptyKnowledgeBase :: KnowledgeBase
emptyKnowledgeBase = KnowledgeBase [] [] [] [] []

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
    in KnowledgeBase calls' args' lambdas' params' dataflow' 

kbGen'' :: Callable -> KnowledgeBase
kbGen'' (Callable.Script script) = kbGenScript script
kbGen'' (Callable.Lambda lambda) = kbGenLambda lambda
kbGen'' (Callable.Method method) = emptyKnowledgeBase
kbGen'' (Callable.Function func) = emptyKnowledgeBase

kbGenScript :: Callable.ScriptContent -> KnowledgeBase
kbGenScript script = let
    calls' = kbGenScriptCalls script
    args' = kbGenScriptArgs script
    in KnowledgeBase calls' args' [] [] []

kbGenLambda :: Callable.LambdaContent -> KnowledgeBase
kbGenLambda lambda = let
    calls' = kbGenCalls (Callable.lambdaBody lambda)
    args' = kbGenArgs (Callable.lambdaBody lambda)
    lambdas' = [ KBLambda (Callable.lambdaLocation lambda) ]
    params' = extractLambdaParams lambda
    dataflow' = extractLambdaDataflow lambda
    in KnowledgeBase calls' args' lambdas' params' dataflow'

extractLambdaDataflow :: Callable.LambdaContent -> [ Edge ]
extractLambdaDataflow lambda = extractLambdaDataflow' (Callable.lambdaBody lambda)

extractLambdaDataflow' :: Cfg -> [ Edge ]
extractLambdaDataflow' cfg = let
    nodes = Cfg.actualNodes $ Cfg.nodes cfg
    instructions = Data.Set.map Bitcode.instructionContent (Data.Set.map Cfg.theInstructionInside nodes)
    in Data.List.foldl' (++) [] (dataflowEdges (Data.Set.toList instructions))

dataflowEdges :: [ Bitcode.InstructionContent ] -> [[ Edge ]]
dataflowEdges = Data.List.map dataflowEdges'

dataflowEdges' :: Bitcode.InstructionContent -> [ Edge ]
dataflowEdges' (Bitcode.Call call)           = dataflowCallEdges call
dataflowEdges' (Bitcode.Assign assign)       = [ dataflowAssignEdge assign ]
dataflowEdges' (Bitcode.FieldRead fieldRead) = [ dataflowFieldReadEdge fieldRead ]
dataflowEdges' _ = []

dataflowCallEdges :: Bitcode.CallContent -> [ Edge ]
dataflowCallEdges c = dataflowCallArgsEdges (Bitcode.callOutput c) (Bitcode.args c)

dataflowCallArgsEdges :: Bitcode.Variable -> [ Bitcode.Variable ] -> [ Edge ]
dataflowCallArgsEdges output = Data.List.map $ dataflowCallArgEdge output

dataflowCallArgEdge :: Bitcode.Variable -> Bitcode.Variable -> Edge
dataflowCallArgEdge output arg = Edge { from = arg, to = output }

dataflowAssignEdge :: Bitcode.AssignContent -> Edge
dataflowAssignEdge a = Edge { from = Bitcode.assignInput a, to = Bitcode.assignOutput a }

dataflowFieldReadEdge :: Bitcode.FieldReadContent -> Edge
dataflowFieldReadEdge f = Edge { from = Bitcode.fieldReadInput f, to = Bitcode.fieldReadOutput f } 

extractLambdaParams :: Callable.LambdaContent -> [ Param ]
extractLambdaParams lambda = extractLambdaParams' (Callable.lambdaLocation lambda) (Callable.lambdaBody lambda)

extractLambdaParams' :: Location -> Cfg -> [ Param ]
extractLambdaParams' location cfg = let
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

keepCalls :: Bitcode.InstructionContent -> Maybe Bitcode.CallContent
keepCalls (Bitcode.Call call) = Just call 
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

